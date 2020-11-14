{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Env where

import ClassyPrelude
import Control.Lens
import Control.Repl
import Effect.Graph (IsDual (..))
import Graph
import History
import Text.Printf

data Env = Env
  { _filePath :: IORef (Maybe FilePath),
    -- | next id that is unique within the current graph
    _nextId :: IORef NID,
    _history :: IORef History,
    _isDualized :: IORef IsDual
  }

makeLenses ''Env

emptyEnv :: IO Env
emptyEnv =
  Env
    <$> newIORef Nothing
    <*> newIORef nilNID
    <*> newIORef (History [] nilNID [])
    <*> newIORef (IsDual False)

errorNoEdge :: String -> Repl Env ()
errorNoEdge = liftIO . printf "edge missing '%s': failed to execute command\n"

describe :: String -> Maybe a -> Either String a
describe s Nothing = Left s
describe _ (Just x) = Right x
