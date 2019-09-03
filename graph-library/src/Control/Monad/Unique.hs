{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Unique where

class Monad m => MonadUnique u m where
  fresh :: m u
