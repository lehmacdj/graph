{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lang.Command2 where

import MyPrelude

import Control.Monad (zipWithM)
import Control.Monad.Freer

import Graph (Edge(..), Connect(..), outgoingConnectsOf, dataOf)

import Effect.Console
import Effect.Throw
import Effect.NodeLocated
import Effect.Graph
import Effect.Graph.Advanced
import Control.Monad.Freer.Fresh

import Lang.APath

data Command
  = ChangeNode (APath String)         -- ^ cd
  | Dualize                           -- ^ d
  | Make (APath String)               -- ^ mk
  | Merge (APath String)              -- ^ mg
  | Clone (APath String) String       -- ^ cl
  | ListOut                           -- ^ ls
  | Query (APath String) String       -- ^ q
  | Tag (APath String) (APath String) -- ^ t
  | Remove (APath String)             -- ^ rm
  | At (APath String) Command         -- ^ at
  | Dedup String                      -- ^ dd
  | Dump FilePath
  | Load FilePath
  | NodeId
  | Debug
  | ShowImage
  | Import FilePath
  | ImportUrl String
  deriving (Eq, Show, Ord)

singleErr :: String -> Err
singleErr cmd = UE $ cmd ++ " needs a path that resolves to a single node"

printTransitions
  :: Member Console effs
  => Set (Connect String) -> Eff effs ()
printTransitions = mapM_ (echo . dtransition) where
  dtransition (Connect t nid) = show t ++ " at " ++ show nid

interpretCommand
  :: ( Members [Console, Throw, SetLocation, GetLocation, Fresh, Dualizeable] effs
     , HasGraph String effs
     )
  => Command -> Eff effs ()
interpretCommand = \case
  ChangeNode a -> do
    (nid, p) <- relativizeAPath a
    let err = const $ singleErr "cd"
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
  Clone a t -> do
    (nid, p) <- relativizeAPath a
    let err = const $ singleErr "clone"
    nid' <- the' err =<< subsumeMissing (resolvePathSuccesses nid p)
    nid'' <- subsumeMissing (cloneNode @String nid')
    cnid <- currentLocation
    insertEdge $ Edge cnid t nid''
  Query a t  -> do
    (nid, p) <- relativizeAPath a
    nids <- subsumeMissing (resolvePathSuccesses nid p)
    nnid <- subsumeMissing (nid `transitionsFreshVia` t)
    _ <- subsumeMissing (mergeNodes @String (nnid `ncons` toList nids))
    pure ()
  Tag a b -> do
    (nid, p) <- relativizeAPath a
    (nid', q) <- relativizeAPath b
    nnids <- subsumeMissing (mkPath nid p)
    let err = const $ singleErr "the last argument of tag"
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
    let
      noSuffix = repeat ""
      suffixes
        | length ambiguities < 2 = noSuffix
        | otherwise = show <$> ([1..] :: [Int])
    forM_ ambiguities $ \amb -> deleteEdge (Edge nid t amb)
    _ <- zipWithM (\a s -> insertEdge (Edge nid (t ++ s) a))
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
  Import _ -> undefined
  ImportUrl _ -> undefined
  Dump _ -> error "unsupported"
  Load _ -> error "unsupported"
  Debug -> error "unsupported"
