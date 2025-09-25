module MyPrelude
  ( -- * Fundamentally MyPrelude is ClassyPrelude with some extra stuff
    module ClassyPrelude,
    unconsumed,
    traceWith,
    traceShowWith,
    nonNull,
    minOn,
    maxOn,

    -- * Extra exports (see the import list below)
    module X,
  )
where

import ClassyPrelude hiding (fromException, nonNull, throwString)
import Control.Comonad as X
import Control.DeepSeq as X
import Data.Coerce as X (Coercible, coerce)
import Data.Functor.Contravariant as X
import Data.Kind as X (Type)
import Data.Orphans ()
import Data.Void as X
import Debug.Trace
import GHC.Generics as X (Generic, Rep)
import GHC.Stack as X (HasCallStack)
import Generic.Data as X (Generically (..), Generically1 (..))
import Language.Haskell.TH.Syntax as X (Lift)
import MyPrelude.Collections as X
import MyPrelude.EarlyReturn as X
import MyPrelude.Effect as X
import MyPrelude.Function as X
import MyPrelude.IO as X
import MyPrelude.JSON as X
import MyPrelude.MaybeEither as X
import MyPrelude.MonadApplicative as X
import MyPrelude.Orphans as X ()
import MyPrelude.RawStrings as X
import MyPrelude.Regex as X
import Prelude as X (Show (showsPrec), ShowS, fail, showParen, showString, shows)

unconsumed :: (Contravariant f) => f a -> f Void
unconsumed = contramap absurd

traceWith :: (a -> Text) -> a -> a
traceWith f a = Debug.Trace.trace (unpack (f a)) a
{-# WARNING traceWith "Don't leave traces in code" #-}

traceShowWith :: (Show b) => (a -> b) -> a -> a
traceShowWith = traceWith . (tshow .)
{-# WARNING traceShowWith "Don't leave traces in code" #-}

nonNull ::
  (MonoFoldable mono, MonoFoldable mono') =>
  Iso (NonNull mono) (NonNull mono') mono mono'
nonNull = iso toNullable impureNonNull

minOn :: (Ord b) => (a -> b) -> a -> a -> a
minOn f x y = if f x <= f y then x else y

maxOn :: (Ord b) => (a -> b) -> a -> a -> a
maxOn f x y = if f x >= f y then x else y
