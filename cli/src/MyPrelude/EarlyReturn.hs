{-# LANGUAGE TemplateHaskell #-}

module MyPrelude.EarlyReturn
  ( EarlyReturn (..),
    returnEarly,
    unwrapReturningDefault,
    unwrapReturningDefaultM,
    withEarlyReturn,
    withEarlyReturn_,
  )
where

import GHC.Generics
import Polysemy
import Polysemy.Error
import Prelude
import MyPrelude.MaybeEither

data EarlyReturn result m a where
  ReturnEarly :: result -> EarlyReturn result m a

makeSem ''EarlyReturn

unwrapReturningDefault ::
  Member (EarlyReturn result) r =>
  result ->
  Maybe a ->
  Sem r a
unwrapReturningDefault def = unwrapDefaulting (returnEarly def)

unwrapReturningDefaultM ::
  Member (EarlyReturn result) r =>
  result ->
  Sem r (Maybe a) ->
  Sem r a
unwrapReturningDefaultM def = unwrapDefaultingM (returnEarly def)

newtype EarlyReturnResult result = EarlyReturnResult {result :: result}
  deriving stock (Show, Eq, Ord, Generic)

withEarlyReturn_ :: Sem '[EarlyReturn result] result -> result
withEarlyReturn_ = run . withEarlyReturn

withEarlyReturn ::
  Sem (EarlyReturn result : r) result ->
  Sem r result
withEarlyReturn =
  fmap (either id id) . runError . reinterpret \case
    ReturnEarly x -> throw x
