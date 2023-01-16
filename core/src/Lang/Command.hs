{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | This module implements commands.
--
-- Style guide for commands for the future:
-- All commands and paths are interpreted relative to the current location
-- We can reintroduce the ability to execute commands relative to a different
-- location later via an `at` command that changes the location and then
-- changes it back.
-- This means that new nodes created and edges created etc start/end at the
-- current node
-- Commands that act on nodes should also act on at least deterministic
-- paths and if possible nondeterministic paths too
module Lang.Command where

import Control.Monad (zipWithM_)
import Effect.Console
import Effect.Editor
import Effect.FileTypeOracle
import Effect.Filesystem
import Effect.FreshNID
import Effect.Graph
import Effect.Graph.Advanced
import Effect.Graph.Check
import Effect.Graph.Export.Filesystem (exportToDirectory)
import Effect.Graph.Import.ByteString
import Effect.Graph.Import.Filesystem
import Effect.NodeLocated
import Effect.Time
import Effect.Warn
import Effect.Web
import qualified Extensibility
import GHC.Generics
import Graph (Connect (..), Edge (..), dataOf, nilNID, outgoingConnectsOf)
import qualified Graph.Serialize2 as S2
import History
import Lang.Path
import MyPrelude hiding (throwString)
import Polysemy.Readline
import Polysemy.State
import Singleton
import UserError

data Command
  = -- | cd
    ChangeNode (Path String)
  | -- | d
    Dualize
  | -- | mk
    Make (Path String)
  | -- | mg
    Merge (Path String)
  | -- | cl
    Clone (Path String) String
  | -- | ls
    ListOut
  | -- | q
    Query (Path String) String
  | -- | t
    Tag (Path String) (Path String)
  | Text String String
  | -- | desc
    Describe String
  | -- | rm
    Remove (Path String)
  | -- | rmnf
    RemoveNode (Path String)
  | -- | at
    At (Path String) Command
  | -- | dd
    Dedup String
  | -- | flatten:
    -- Takes every node transition/* and creates edges transition to them.
    -- The purpose of this is to convert from a form where things have explicit
    -- but unnecessary names to a form where the edge is the only identifiying
    -- attribute.
    -- This can be considered to be the inverse of dedup in a sense
    Flatten String
  | -- | nid
    NodeId
  | -- | :d
    Debug
  | -- | si
    ShowImage
  | -- | :i
    Import FilePath
  | -- | wget
    ImportUrl String
  | -- | fsck
    Check
  | -- | fix
    Fix
  | -- | mv
    Move (Path String) (Path String)
  | -- | rn
    Rename (Path String) (Path String)
  | -- | vi
    Edit
  | -- | back: Go back in history by a certain number of steps. Greater number
    -- than amount of history goes maximum amount backwards. Negative number
    -- attempts to go forward in history if there is any recorded.
    Back Int
  | -- | Turn the largest non-cyclic graph from the current location into a
    -- file system structure representing the tree. Uses cloning when possible
    -- to minimize disk footprint; takes a filepath to write the tree to as an
    -- argument.
    Materialize FilePath
  | -- | Execute some arbitrary Haskell code that has access to the graph at
    -- the current node. The Haskell code is given by the data at the node given
    -- as an argument.
    Exec (Path String)
  | -- | Collect same transitions into a transition to a single node that
    -- transitions to the nodes the previous transition used to
    Collect String
  deriving (Eq, Show, Ord, Generic)

singleErr :: String -> Set NID -> UserError
singleErr cmd xs =
  OtherError $
    cmd ++ " needs a path that resolves to a single node\n"
      ++ "but it resolved to: "
      ++ show (setToList xs)

printTransitions ::
  Member Console effs =>
  Set (Connect String) ->
  Sem effs ()
printTransitions = mapM_ (echo . dtransition)
  where
    dtransition (Connect t nid) = show t ++ " at " ++ show nid

promptYesNo ::
  Member Readline r =>
  String ->
  Sem r Bool
promptYesNo prompt = do
  line <- getInputLine prompt
  let result
        | line `elem` map Just ["yes", "y", "Y"] = pure True
        | line `elem` map Just ["n", "no", "N"] = pure False
        | otherwise = promptYesNo "please enter (y/n): "
  result

-- | Check to make sure that the current state of the graph is not dualized
guardDangerousDualizedOperation ::
  Members [Readline, Error UserError, Dualizeable] r => Sem r ()
guardDangerousDualizedOperation = do
  isDual' <- isDual <$> get @IsDual
  when isDual' do
    outputStrLn "the graph is currently dualized"
    outputStrLn "the operation you are attempting may be dangerous in that state"
    promptYesNo "proceed (y/n): " >>= bool (throw OperationCancelled) (pure ())

interpretCommand ::
  ( Members [Console, Error UserError, SetLocation, GetLocation, Dualizeable] effs,
    Members [FileSystemTree, Web, FreshNID, GetTime, Editor, State History] effs,
    Members [FileTypeOracle, Readline, Warn UserError] effs,
    -- TODO: remove this inclusion of RawGraph + Embed IO here; probably the
    -- best way to do this is to allow commands to be defined as @stack@
    -- scripts, and then rewrite materialize and any other commands that
    -- require this outside of
    Members [RawGraph, Embed IO] effs,
    HasGraph String effs
  ) =>
  Command ->
  Sem effs ()
interpretCommand = \case
  ChangeNode p -> do
    nid <- currentLocation
    let err = singleErr "cd"
    nid' <- the' err =<< subsumeUserError @Missing (resolvePathSuccesses nid p)
    changeLocation nid'
  NodeId -> currentLocation >>= echo . show
  Dualize -> dualize
  Make p -> do
    nid <- currentLocation
    subsumeUserError $ mkPath nid p >> pure ()
  Merge p -> do
    nid <- currentLocation
    nids <- subsumeUserError (resolvePathSuccesses nid p)
    whenNonNull (setToList nids) $
      \xs -> subsumeUserError (mergeNodes @String xs) >> pure ()
  Remove p -> do
    nid <- currentLocation
    subsumeUserError $ delPath nid p
  RemoveNode p -> do
    nid <- currentLocation
    nids <- subsumeUserError (resolvePathSuccesses nid p)
    forM_ nids $ deleteNode @String
  Clone p t -> do
    nid <- currentLocation
    let err = singleErr "clone"
    nid' <- the' err =<< subsumeUserError (resolvePathSuccesses nid p)
    nid'' <- subsumeUserError (cloneNode @String nid')
    cnid <- currentLocation
    insertEdge $ Edge cnid t nid''
  Query p t -> do
    nid <- currentLocation
    nids <- subsumeUserError (resolvePathSuccesses nid p)
    nnid <- subsumeUserError (nid `transitionsFreshVia` t)
    _ <- subsumeUserError (mergeNodes @String (nnid `ncons` toList nids))
    pure ()
  Tag p q -> do
    nid <- currentLocation
    let err = singleErr "the last argument of tag"
    target <- the' err =<< subsumeUserError (resolvePathSuccesses nid q)
    nnids <- subsumeUserError (mkPath nid (p :/ Literal ""))
    _ <- subsumeUserError (mergeNodes @String (target `ncons` toList nnids))
    pure ()
  Text t s -> do
    nid <- currentLocation
    vNid <- the' (error "only creating one path") =<< subsumeUserError (mkPath nid (Literal t))
    setData vNid (Just (encodeUtf8 (fromString s)))
  Describe d -> interpretCommand (Text "description" d)
  At p c -> do
    nid <- currentLocation
    locations <- subsumeUserError (resolvePathSuccesses nid p)
    forM_ locations $ \nid' -> local @NID (const nid') $ interpretCommand c
  Dedup t -> do
    nid <- currentLocation
    ambiguities <- subsumeUserError (resolvePathSuccesses nid (Literal t))
    let noSuffix = repeat ""
        suffixes
          | length ambiguities < 2 = noSuffix
          | otherwise = show <$> ([1 ..] :: [Int])
    forM_ ambiguities $ \amb -> deleteEdge (Edge nid t amb)
    zipWithM_
      (\a s -> insertEdge (Edge nid (t ++ s) a))
      (toList ambiguities)
      suffixes
  Flatten t -> do
    nid <- currentLocation
    let err =
          const . OtherError $
            "flatten only works if there is only a single node that the literal resolves to"
    nodeToFlattenFrom <- the' err =<< subsumeUserError (resolvePathSuccesses nid (Literal t))
    nodesToFlatten <- subsumeUserError $ resolvePathSuccesses nodeToFlattenFrom Wild
    deleteEdge (Edge nid t nodeToFlattenFrom)
    for_ [Edge nid t nid' | nid' <- toList nodesToFlatten] insertEdge
  ListOut -> do
    n <- subsumeUserError currentNode
    printTransitions (outgoingConnectsOf n)
  ShowImage -> do
    n <- subsumeUserError (currentNode @String)
    forM_ (dataOf n) $ subsumeUserError @Missing . displayImage . fromStrict
  -- it probably would make sense to factor these commands out into separate
  -- layers of commands that can be handled at different levels
  Import fp -> do
    guardDangerousDualizedOperation
    subsumeUserError $ importDirectory fp nilNID
    changeLocation nilNID
  ImportUrl uri -> do
    guardDangerousDualizedOperation
    nid <- subsumeUserError (importUrl nilNID uri)
    changeLocation nid
  Debug -> do
    echo "current node:"
    currentLocation >>= subsumeUserError . getNodeSem >>= echo . show @(Node String)
    echo "history:"
    get @History >>= echo . show
  -- echo "node-ids in the graph:"
  -- nodeManifest @String >>= echo . show
  Check -> reportToConsole @String (fsck @String)
  Fix -> fixErrors @String (fsck @String)
  Move p q -> do
    nid <- currentLocation
    let err = singleErr "the last argument of mv"
    target <- the' err =<< subsumeUserError (resolvePathSuccesses nid q)
    subsumeUserError (mvPath nid p target)
  Rename p q -> do
    nid <- currentLocation
    let err xs =
          OtherError $
            "the first argument to rn require the path to only resolve to "
              ++ "one node but they resolved to \n"
              ++ (show . map endPoint . setToList $ xs)
    c <- the' err =<< subsumeUserError (resolvePathSuccessesDetail nid p)
    subsumeUserError (renameDPath c nid q)
  Edit -> do
    n <- subsumeUserError @Missing currentLocation
    invokeEditor [n]
  Back n -> do
    history <- get @History
    let (_, history') = backInTime n history
    -- technically this could lead to being on an invalid node that is already
    -- deleted. we don't make an effort to change the past when things are
    -- deleted in the future right now
    -- we don't need to use changeLocation here because setting the history
    -- already modifies the location and if we set the location we would end up
    -- adding duplicates to the history
    put @History history'
  Materialize fp -> do
    nid <- subsumeUserError @Missing currentLocation
    exportToDirectory nid fp
  Exec p -> do
    nid <- currentLocation
    let err = singleErr "the argument to exec"
    target <- the' err =<< subsumeUserError (resolvePathSuccesses nid p)
    base <- getGraphFilePath
    Extensibility.runScript (S2.nodeDataFile base target)
  Collect t -> do
    currentNid <- currentLocation
    nids <- subsumeUserError (resolvePathSuccesses currentNid (Literal t))
    newNid <-
      the' (error "only creating one path")
        =<< subsumeUserError (mkPath currentNid (Literal t))
    for_ nids $ \nid -> do
      deleteEdge (Edge currentNid t nid)
      insertEdge (Edge newNid "" nid)
