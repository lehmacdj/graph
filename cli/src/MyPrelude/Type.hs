module MyPrelude.Type
  ( module MyPrelude.Type,
    module X,
  )
where

import Data.Kind as X (Type)

type family Concat (a :: [k]) (b :: [k]) :: [k] where
  Concat '[] b = b
  Concat (a ': as) b = a ': Concat as b
