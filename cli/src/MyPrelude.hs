module MyPrelude
  ( -- * Fundamentally MyPrelude is ClassyPrelude with some extra stuff
    module ClassyPrelude,
    unconsumed,
    traceWith,
    traceShowWith,

    -- * Extra exports (see the import list below)
    module X,
  )
where

import ClassyPrelude hiding (fromException, throwString)
import Control.Comonad as X
import Control.DeepSeq as X
import Data.Coerce as X (Coercible, coerce)
import Data.Functor.Contravariant as X
import Data.Kind as X (Type)
import Data.Set.Ordered.Orphans as X ()
import Data.Void as X
import Debug.Trace
import GHC.Generics as X (Generic, Rep)
import GHC.Stack as X (HasCallStack)
import MyPrelude.Collections as X
import MyPrelude.EarlyReturn as X
import MyPrelude.Effect as X
import MyPrelude.Function as X
import MyPrelude.IO as X
import MyPrelude.JSON as X
import MyPrelude.MaybeEither as X
import MyPrelude.MonadApplicative as X

unconsumed :: (Contravariant f) => f a -> f Void
unconsumed = contramap absurd

traceWith :: (a -> Text) -> a -> a
traceWith f a = Debug.Trace.trace (unpack (f a)) a
{-# WARNING traceWith "Don't leave traces in code" #-}

traceShowWith :: (Show b) => (a -> b) -> a -> a
traceShowWith = traceWith . (tshow .)
{-# WARNING traceShowWith "Don't leave traces in code" #-}
