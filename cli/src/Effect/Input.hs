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

-- | Run an 'Input' effect by always returning the value returned by a monadic
-- action once. This is useful for initializing an input in terms of a State
-- effect for example.
runInputConstM :: Eff effs i -> Eff (Input i : effs) a -> Eff effs a
runInputConstM initializationAction action =
  initializationAction >>= \val -> runInputConst val action
