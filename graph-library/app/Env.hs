{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Env where

import ClassyPrelude
import Control.Lens
import Control.Monad.Unique
import Control.Repl
import Effect.Graph (IsDual (..))
import Graph
import Text.Printf

data Env = Env
  { _filePath :: IORef (Maybe FilePath),
    -- | next id that is unique within the current graph
    _nextId :: IORef NID,
    _currentNID :: IORef NID,
    _isDualized :: IORef IsDual
  }

makeLenses ''Env

modifyOf ::
  (MonadReader env m, MonadIO m) =>
  Lens' env (IORef r) ->
  (r -> r) ->
  m r
modifyOf l f = do
  ref <- view l
  r <- readIORef ref
  modifyIORef' ref f
  pure r

-- | Create a new unique NID
freshNID :: (MonadReader Env m, MonadIO m) => m NID
freshNID = modifyOf nextId (+ 1)

instance MonadUnique NID (Repl Env) where
  fresh = freshNID

emptyEnv :: IO Env
emptyEnv =
  Env
    <$> newIORef Nothing
    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef (IsDual False)

errorNoEdge :: String -> Repl Env ()
errorNoEdge = liftIO . printf "edge missing '%s': failed to execute command\n"

describe :: String -> Maybe a -> Either String a
describe s Nothing = Left s
describe _ (Just x) = Right x
