-- | Input effect: a context that can supply values of type @i@ on demand.
-- Port of polysemy's @Input@ effect, which has no effectful counterpart.
-- Deliberately not part of MyPrelude because the @input@ accessor's name
-- collides with common local binding names.
module Effect.Input where

import MyPrelude

data Input i :: Effect where
  Input :: Input i m i

type instance DispatchOf (Input i) = Dynamic

input :: forall i effs. (Input i :> effs) => Eff effs i
input = send Input

runInputConst :: forall i effs a. i -> Eff (Input i : effs) a -> Eff effs a
runInputConst i = interpret \_ -> \case Input -> pure i

-- | Run an 'Input' effect by evaluating a monadic action for every request.
supplyInputVia ::
  forall i effs.
  Eff effs i ->
  Eff (Input i : effs) ~> Eff effs
supplyInputVia supplier = interpret \_ -> \case Input -> supplier

-- | Run an 'Input' effect by always returning the value returned by a monadic
-- action once. This is useful for initializing an input in terms of a State
-- effect for example.
runInputConstSem :: Eff effs i -> Eff (Input i : effs) a -> Eff effs a
runInputConstSem initializationAction action =
  initializationAction >>= \val -> runInputConst val action

-- | Map an 'Input' contravariantly.
contramapInput ::
  forall i i' effs a.
  (Input i' :> effs) =>
  (i' -> i) ->
  Eff (Input i : effs) a ->
  Eff effs a
contramapInput f = interpret \_ -> \case Input -> f <$> input @i'

-- | Map an 'Input' contravariantly through a monadic function.
contramapInputSem ::
  forall i i' effs a.
  (Input i' :> effs) =>
  (i' -> Eff effs i) ->
  Eff (Input i : effs) a ->
  Eff effs a
contramapInputSem f = interpret \_ -> \case Input -> f =<< input @i'

applyInput ::
  forall i effs a. (Input i :> effs) => (i -> Eff effs a) -> Eff effs a
applyInput f = input @i >>= f

applyInput2 ::
  forall i effs a b.
  (Input i :> effs) =>
  (i -> b -> Eff effs a) ->
  b ->
  Eff effs a
applyInput2 f b = input @i >>= flip f b
