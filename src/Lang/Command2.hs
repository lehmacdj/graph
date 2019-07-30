module Lang.Command2 where

import Lang.APath

data Command
  = ChangeNode (APath String)         -- ^ cd
  | Dualize                           -- ^ d
  | Make (APath String)               -- ^ mk
  | Merge (APath String)              -- ^ mg
  | Clone (APath String) String       -- ^ cl
  | ListOut                           -- ^ ls
  | Query (APath String) String       -- ^ q
  | Tag (APath String) (APath String) -- ^ t
  | Remove (APath String)             -- ^ rm
  | At (APath String) Command         -- ^ at
  | Dedup String                      -- ^ dd
  | Dump FilePath
  | Load FilePath
  | NodeId
  | Debug
  | ShowImage
  | Import FilePath
  | ImportUrl String
  deriving (Eq, Show, Ord)
