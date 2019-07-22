{-|
   Absolute paths, paths that may be augmented with an nid:path or may be
   relative.
 -}
module Lang.APath
  ( module Lang.APath
  , module Lang.Path
  , Id
  ) where

import Graph
import Lang.Path

data APath t
  = Relative (Path t)
  | Absolute Id (Path t)
  deriving (Show, Eq, Ord)
