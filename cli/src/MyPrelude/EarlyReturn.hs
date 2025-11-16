{-# LANGUAGE TemplateHaskell #-}

module MyPrelude.EarlyReturn
  ( EarlyReturn (..),
    returnEarly,
    unwrapReturningDefault,
    unwrapReturningDefaultM,
    unwrap,
    unwrapM,
    withEarlyReturn,
    withEarlyReturn_,
    withEarlyReturnIO,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.TH
import GHC.Generics
import MyPrelude.MaybeEither
import Prelude

data EarlyReturn result :: Effect where
  ReturnEarly :: result -> EarlyReturn result m a

makeEffect ''EarlyReturn

unwrapReturningDefault ::
  (EarlyReturn result :> es) =>
  result ->
  Maybe a ->
  Eff es a
unwrapReturningDefault def = unwrapDefaulting (returnEarly def)

unwrapReturningDefaultM ::
  (EarlyReturn result :> es) =>
  result ->
  Eff es (Maybe a) ->
  Eff es a
unwrapReturningDefaultM def = unwrapDefaultingM (returnEarly def)

unwrap ::
  (EarlyReturn (Maybe result) :> es) =>
  Maybe a ->
  Eff es a
unwrap = unwrapReturningDefault Nothing

unwrapM ::
  (EarlyReturn (Maybe result) :> es) =>
  Eff es (Maybe a) ->
  Eff es a
unwrapM = unwrapReturningDefaultM Nothing

newtype EarlyReturnResult result = EarlyReturnResult {result :: result}
  deriving stock (Show, Eq, Ord, Generic)

withEarlyReturn_ :: Eff '[EarlyReturn result] result -> result
withEarlyReturn_ = runPureEff . withEarlyReturn

withEarlyReturnIO ::
  Eff '[EarlyReturn result, IOE] result ->
  IO result
withEarlyReturnIO = runEff . withEarlyReturn

withEarlyReturn ::
  Eff (EarlyReturn result : es) result ->
  Eff es result
withEarlyReturn =
  fmap (either id id) . runError . reinterpret (\_ -> \case
    ReturnEarly x -> throwError x)
