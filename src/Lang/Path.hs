module Lang.Path where

data Path t
  = Zero
  | One
  | Literal t
  | Path t :. Path t
  | Path t :+ Path t
  | Path t :& Path t
  deriving (Show, Eq, Ord)
