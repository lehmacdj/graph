{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module State where

import Graph
import Control.Lens

import Control.Repl
import Control.Monad.Unique

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
