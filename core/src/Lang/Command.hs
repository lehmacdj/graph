{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lang.Command where

import Control.Monad (zipWithM)
import Effect.Console
import Effect.Editor
import Effect.Filesystem
import Effect.FreshNID
import Effect.Graph
import Effect.Graph.Advanced
import Effect.Graph.Check
import Effect.Graph.Import.ByteString
import Effect.Graph.Import.Filesystem
import Effect.Load
import Effect.NodeLocated
import Effect.Throw
import Effect.Time
import Effect.Web
import Graph (Connect (..), Edge (..), dataOf, nilNID, outgoingConnectsOf)
import History
import Lang.APath
import MyPrelude
import Polysemy.State
import Singleton
import UserError

data Command
  = -- | cd
    ChangeNode (APath String)
  | -- | d
    Dualize
  | -- | mk
    Make (APath String)
  | -- | mg
    Merge (APath String)
  | -- | cl
    Clone (APath String) String
  | -- | ls
    ListOut
  | -- | q
    Query (APath String) String
  | -- | t
    Tag (APath String) (APath String)
  | -- | rm
    Remove (APath String)
  | -- | rmnf
    RemoveNode (APath String)
  | -- | at
    At (APath String) Command
  | -- | dd
    Dedup String
  | -- | :l
    Load FilePath
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
    Move (APath String) (APath String)
  | -- | rn
    Rename (APath String) (APath String)
  | -- | vi
    Edit
  | -- | back: Go back in history by a certain number of steps. Greater number
    -- than amount of history goes maximum amount backwards. Negative number
    -- attempts to go forward in history if there is any recorded.
    Back Int
  deriving (Eq, Show, Ord)

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

interpretCommand ::
  ( Members [Console, ThrowUserError, SetLocation, GetLocation, Dualizeable] effs,
    Members [FileSystemTree, Web, Load, FreshNID, GetTime, Editor, State History] effs,
    HasGraph String effs
  ) =>
  Command ->
  Sem effs ()
interpretCommand = \case
  ChangeNode a -> do
    (nid, p) <- relativizeAPath a
    let err = singleErr "cd"
    nid' <- the' err =<< subsumeMissing (resolvePathSuccesses nid p)
    changeLocation nid'
  NodeId -> currentLocation >>= echo . show
  Dualize -> dualize
  Make a -> do
    (nid, p) <- relativizeAPath a
    subsumeMissing $ mkPath nid p >> pure ()
  Merge a -> do
    (nid, p) <- relativizeAPath a
    nids <- subsumeMissing (resolvePathSuccesses nid p)
    whenNonNull (setToList nids) $
      \xs -> subsumeMissing (mergeNodes @String xs) >> pure ()
  Remove a -> do
    (nid, p) <- relativizeAPath a
    subsumeMissing $ delPath nid p
  RemoveNode a -> do
    (nid, p) <- relativizeAPath a
    nids <- subsumeMissing (resolvePathSuccesses nid p)
    forM_ nids $ deleteNode @String
  Clone a t -> do
    (nid, p) <- relativizeAPath a
    let err = singleErr "clone"
    nid' <- the' err =<< subsumeMissing (resolvePathSuccesses nid p)
    nid'' <- subsumeMissing (cloneNode @String nid')
    cnid <- currentLocation
    insertEdge $ Edge cnid t nid''
  Query a t -> do
    (nid, p) <- relativizeAPath a
    nids <- subsumeMissing (resolvePathSuccesses nid p)
    nnid <- subsumeMissing (nid `transitionsFreshVia` t)
    _ <- subsumeMissing (mergeNodes @String (nnid `ncons` toList nids))
    pure ()
  Tag a b -> do
    (nid, p) <- relativizeAPath a
    (nid', q) <- relativizeAPath b
    nnids <- subsumeMissing (mkPath nid p)
    let err = singleErr "the last argument of tag"
    target <- the' err =<< subsumeMissing (resolvePathSuccesses nid' q)
    _ <- subsumeMissing (mergeNodes @String (target `ncons` toList nnids))
    pure ()
  At a c -> do
    (nid, p) <- relativizeAPath a
    locations <- subsumeMissing (resolvePathSuccesses nid p)
    cnid <- currentLocation
    forM_ locations $ \nid' -> do
      local @NID (const nid') $ interpretCommand c
  Dedup t -> do
    nid <- currentLocation
    ambiguities <- subsumeMissing (resolvePathSuccesses nid (Literal t))
    let noSuffix = repeat ""
        suffixes
          | length ambiguities < 2 = noSuffix
          | otherwise = show <$> ([1 ..] :: [Int])
    forM_ ambiguities $ \amb -> deleteEdge (Edge nid t amb)
    _ <-
      zipWithM
        (\a s -> insertEdge (Edge nid (t ++ s) a))
        (toList ambiguities)
        suffixes
    pure ()
  ListOut -> do
    n <- subsumeMissing currentNode
    printTransitions (outgoingConnectsOf n)
  ShowImage -> do
    n <- subsumeMissing (currentNode @String)
    forM_ (dataOf n) $ subsumeMissing . displayImage
  -- it probably would make sense to factor these commands out into separate
  -- layers of commands that can be handled at different levels
  Import fp -> currentLocation >>= subsumeMissing . importDirectory fp
  ImportUrl uri -> do
    nid <- subsumeMissing (importUrl nilNID uri)
    changeLocation nid
  Load fp -> do
    setLoaded fp
  Debug -> do
    echo "current node:"
    currentLocation >>= subsumeMissing . getNode' >>= echo . show @(Node String)
    echo "history:"
    get @History >>= echo . show
  -- echo "node-ids in the graph:"
  -- nodeManifest @String >>= echo . show
  Check -> reportToConsole @String (fsck @String)
  Fix -> fixErrors @String (fsck @String)
  Move a b -> do
    (nid, p) <- relativizeAPath a
    (nid', q) <- relativizeAPath b
    let err = singleErr "the last argument of mv"
    target <- the' err =<< subsumeMissing (resolvePathSuccesses nid' q)
    subsumeMissing (mvPath nid p target)
  Rename a b -> do
    (nid, p) <- relativizeAPath a
    (nid', q) <- relativizeAPath b
    let err xs =
          OtherError $
            "the first argument to rn require the path to only resolve to "
              ++ "one node but they resolved to \n"
              ++ (show . map endPoint . setToList $ xs)
    c <- the' err =<< subsumeMissing (resolvePathSuccessesDetail nid p)
    subsumeMissing (renameDPath c nid' q)
  Edit -> do
    n <- subsumeMissing currentLocation
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