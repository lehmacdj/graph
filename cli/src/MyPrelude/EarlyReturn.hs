{-# LANGUAGE AllowAmbiguousTypes #-}

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
import Effectful.Error.Static (runErrorNoCallStack, throwError_)
import GHC.Generics
import MyPrelude.MaybeEither
import Prelude

data EarlyReturn result :: Effect where
  ReturnEarly :: result -> EarlyReturn result m a

type instance DispatchOf (EarlyReturn result) = Dynamic

returnEarly ::
  forall result effs a.
  (EarlyReturn result :> effs) =>
  result ->
  Eff effs a
returnEarly = send . ReturnEarly

unwrapReturningDefault ::
  (EarlyReturn result :> r) =>
  result ->
  Maybe a ->
  Eff r a
unwrapReturningDefault def = unwrapDefaulting (returnEarly def)

unwrapReturningDefaultM ::
  (EarlyReturn result :> r) =>
  result ->
  Eff r (Maybe a) ->
  Eff r a
unwrapReturningDefaultM def = unwrapDefaultingM (returnEarly def)

unwrap ::
  forall m r a.
  (Monoid m) =>
  (EarlyReturn m :> r) =>
  Maybe a ->
  Eff r a
unwrap = unwrapReturningDefault @m mempty

unwrapM ::
  forall m r a.
  (Monoid m) =>
  (EarlyReturn m :> r) =>
  Eff r (Maybe a) ->
  Eff r a
unwrapM = unwrapReturningDefaultM @m mempty

newtype EarlyReturnResult result = EarlyReturnResult {result :: result}
  deriving stock (Show, Eq, Ord, Generic)

withEarlyReturn_ :: Eff '[EarlyReturn result] result -> result
withEarlyReturn_ = runPureEff . withEarlyReturn

withEarlyReturnIO ::
  Eff '[EarlyReturn result, IOE] result ->
  IO result
withEarlyReturnIO = runEff . withEarlyReturn

withEarlyReturn ::
  Eff (EarlyReturn result : r) result ->
  Eff r result
withEarlyReturn =
  fmap (either id id)
    . reinterpret runErrorNoCallStack \_ -> \case
      ReturnEarly x -> throwError_ x
