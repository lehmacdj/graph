{-# LANGUAGE TemplateHaskell #-}

module MyPrelude.EarlyReturn
  ( EarlyReturn (..),
    returnEarly,
    withEarlyReturn,
  )
where

import GHC.Generics
import Polysemy
import Polysemy.Error
import Prelude

data EarlyReturn result m a where
  ReturnEarly :: result -> EarlyReturn result m a

makeSem ''EarlyReturn

newtype EarlyReturnResult result = EarlyReturnResult {result :: result}
  deriving stock (Show, Eq, Ord, Generic)

withEarlyReturn ::
  Sem (EarlyReturn result : r) result ->
  Sem r result
withEarlyReturn =
  fmap (either id id) . runError . reinterpret \case
    ReturnEarly x -> throw x
