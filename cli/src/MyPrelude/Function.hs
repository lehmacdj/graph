module MyPrelude.Function
  ( module MyPrelude.Function,
    module X,
  )
where

import ClassyPrelude
import Control.Arrow as X ((>>>))

-- | natural transformation
type (~>) f g = forall x. f x -> g x

-- | Apply a function n times to a given value
-- implementation taken from protolude
applyN :: forall a. Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f :: [a -> a])
