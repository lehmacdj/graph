
module Effect.Throw
  ( module Effect.Throw,
    Error,
  )
where

import Graph.Types
import MyPrelude
import Polysemy.Error
import UserError

newtype Missing = Missing {unMissing :: NID}
  deriving (Show, Eq, Ord)

type ThrowMissing = Error Missing

throwMissing :: Member ThrowMissing effs => NID -> Sem effs a
throwMissing = Polysemy.Error.throw . Missing

subsumeMissing :: Member ThrowUserError effs => Sem (ThrowMissing ': effs) ~> Sem effs
subsumeMissing = (`handleError` (UserError.throw . MissingNode . unMissing))
