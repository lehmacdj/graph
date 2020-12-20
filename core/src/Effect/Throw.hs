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

throwMissing :: Member (Error Missing) effs => NID -> Sem effs a
throwMissing = throw . Missing

subsumeMissing ::
  Member (Error UserError) effs => Sem (Error Missing ': effs) ~> Sem effs
subsumeMissing = (`handleError` (throw . MissingNode . unMissing))
