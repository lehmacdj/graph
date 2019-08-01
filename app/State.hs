{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module State where

import Control.Monad.Unique
import Graph.Serialize2

import Control.Repl
import Data.Foldable (toList)
import Data.Set (Set)
import Text.Printf
import Control.Lens
import Data.List (intercalate)
import Control.Monad.State

import Graph

import Lang.APath

data S = S
  { _filePath :: Maybe FilePath
  , _nextId :: Id -- ^ next id that is unique within the current graph
  , _currentNID :: Id
  , _graph :: Graph String
  }
  deriving (Show, Eq, Ord)
makeLenses ''S

-- | Create a node with a unique id not yet in the graph
-- intended for use in larger monad states using zoom from Control.Lens.Zoom
-- Every node should be created using this method, this guarantees that every
-- new node has a unique id.
freshNode :: Repl S (Node t)
freshNode = do
  nid <- use nextId
  nextId += 1
  pure (emptyNode nid)

instance MonadUnique Id (Repl S) where
  fresh = do
    nid <- use nextId
    nextId += 1
    pure nid

emptyS :: S
emptyS = S Nothing 1 0 (insertNode (emptyNode 0) emptyGraph)

currentNode :: MonadState S m => m (Node String)
currentNode = lookupNode <$> use graph <*> use currentNID

currentNodeDataFile :: Repl S FilePath
currentNodeDataFile = do
  cnid <- use currentNID
  p <- use filePath
  case p of
    Just base -> pure (nodeDataFile base cnid)
    Nothing -> error "there is no current path"

errorNoEdge :: String -> Repl S ()
errorNoEdge = liftIO . printf "edge missing '%s': failed to execute command\n"

printTransitions :: Set (Connect String) -> IO ()
printTransitions = putStrLn . unlines' . fmap dtransition . toList where
  unlines' = intercalate "\n"
  dtransition (Connect t nid) = show t ++ " at " ++ show nid

describe :: String -> Maybe a -> Either String a
describe s Nothing = Left s
describe _ (Just x) = Right x

apathResolve
  :: MonadState S m
  => APath String
  -> m (Either String (Node String, Path String))
apathResolve (Absolute nid p) = do
  g <- use graph
  pure . describe ("invalid nid " ++ show nid) $ (\x -> (x, p)) <$> maybeLookupNode g nid
apathResolve (Relative p) = Right <$> do
  n <- currentNode
  pure (n, p)

-- | Same as withAPath' but with a default result if the lookup fails
withAPath'
  :: (MonadState S m, MonadIO m)
  => m a
  -> APath String
  -> (Node String -> Path String -> m a)
  -> m a
withAPath' d a f = do
  r <- apathResolve a
  case r of
    Left e -> liftIO (putStrLn e) >> d
    Right (n, p) -> f n p

withAPath
  :: (MonadState S m, MonadIO m)
  => APath String
  -> (Node String -> Path String -> m ())
  -> m ()
withAPath = withAPath' (pure ())

withTwoAPaths
  :: (MonadState S m, MonadIO m)
  => APath String
  -> APath String
  -> (Node String -> Path String -> Node String -> Path String -> m ())
  -> m ()
withTwoAPaths a b f = do
  r <- apathResolve a
  r' <- apathResolve b
  case sequenceOf both (r, r') of
    Left e -> liftIO $ putStrLn e
    Right ((n, p), (n', p')) -> f n p n' p'
