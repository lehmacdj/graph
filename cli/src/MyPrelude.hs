module MyPrelude
  ( -- * Fundamentally MyPrelude is ClassyPrelude with some extra stuff
    module ClassyPrelude,

    -- * Extra exports (see the import list below)
    module X,
  )
where

import ClassyPrelude hiding (throwString, fromException)
import Control.DeepSeq as X
import Data.Coerce as X (Coercible, coerce)
import Data.Generics.Labels as X
import Data.Set.Ordered.Orphans as X ()
import GHC.Generics as X (Generic, Rep)
import GHC.Stack as X (HasCallStack)
import MyPrelude.EarlyReturn as X
import MyPrelude.Collections as X
import MyPrelude.MaybeEither as X
import MyPrelude.IO as X
import MyPrelude.JSON as X
import MyPrelude.Function as X
import MyPrelude.Effect as X
import MyPrelude.MonadApplicative as X
import Data.Kind as X (Type)
