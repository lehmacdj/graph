{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lang.Command where

import Control.Monad (zipWithM)
import Control.Monad.Freer.Fresh
import Effect.Console
import Effect.Editor
import Effect.Filesystem
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
import Graph (Connect (..), Edge (..), dataOf, outgoingConnectsOf)
import Lang.APath
import MyPrelude
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
  Eff effs ()
printTransitions = mapM_ (echo . dtransition)
  where
    dtransition (Connect t nid) = show t ++ " at " ++ show nid

-- | set the freshness to a certain number
resetFresh :: Member (Writer NID) effs => NID -> Eff effs ()
resetFresh = tell

interpretCommand ::
  ( Members [Console, ThrowUserError, SetLocation, GetLocation, Fresh, Dualizeable] effs,
    Members [FileSystemTree, Web, Load, Writer NID, GetTime, Editor] effs,
    HasGraph String effs
  ) =>
  Command ->
  Eff effs ()
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
      changeLocation nid'
      interpretCommand c
    changeLocation cnid
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
    nid <- subsumeMissing (importUrl 0 uri)
    changeLocation nid
  Load fp -> do
    setLoaded fp
  Debug -> do
    echo "current node:"
    currentLocation >>= subsumeMissing . getNode' >>= echo . show @(Node String)
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
