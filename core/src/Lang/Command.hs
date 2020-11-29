{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
import Effect.Filesystem
import Effect.FreshNID
import Effect.Graph
import Effect.Graph.Advanced
import Effect.Graph.Check
import Effect.Graph.Import.ByteString
import Effect.Graph.Import.Filesystem
import Effect.NodeLocated
import Effect.Throw
import Effect.Time
import Effect.Web
import GHC.Generics
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
    Move (APath String) (APath String)
  | -- | rn
    Rename (APath String) (APath String)
  | -- | vi
    Edit
  | -- | back: Go back in history by a certain number of steps. Greater number
    -- than amount of history goes maximum amount backwards. Negative number
    -- attempts to go forward in history if there is any recorded.
    Back Int
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

interpretCommand ::
  ( Members [Console, ThrowUserError, SetLocation, GetLocation, Dualizeable] effs,
    Members [FileSystemTree, Web, FreshNID, GetTime, Editor, State History] effs,
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
    forM_ locations $ \nid' -> local @NID (const nid') $ interpretCommand c
  Dedup t -> do
    nid <- currentLocation
    ambiguities <- subsumeMissing (resolvePathSuccesses nid (Literal t))
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
    nodeToFlattenFrom <- the' err =<< subsumeMissing (resolvePathSuccesses nid (Literal t))
    nodesToFlatten <- subsumeMissing $ resolvePathSuccesses nodeToFlattenFrom Wild
    deleteEdge (Edge nid t nodeToFlattenFrom)
    for_ [Edge nid t nid' | nid' <- toList nodesToFlatten] insertEdge
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
