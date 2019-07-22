module Lang.Command2 where

import Lang.APath

data Command
  = ChangeNode (APath String) -- ^ cd
  | Dualize -- ^ d
  | Make (APath String) -- ^ mk
  | Merge (APath String) -- ^ mg
  | Clone (APath String) (APath String) -- ^ cl
  | List -- ^ ls
  | AddLinksToFrom (APath String) (APath String) -- ^ tf
  | AddLinksFromTo (APath String) (APath String) -- ^ ft
  | Remove (APath String) -- ^ rm
  | At (APath String) Command -- ^ at
  | Dump FilePath
  | Load FilePath
  | NodeId
  | Debug
  | ShowImage
  | Import FilePath
  | ImportUrl String
  deriving (Eq, Show, Ord)
