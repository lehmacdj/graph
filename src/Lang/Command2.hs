module Lang.Command2 where

import Lang.APath

data Command
  = ChangeNode (APath String)
  | Dualize
  | Make (APath String)
  | Merge (APath String)
  | Clone (APath String) (APath String)
  | List
  | AddLinksToFrom (APath String) (APath String)
  | AddLinksFromTo (APath String) (APath String)
  | Remove (APath String)
  | At (APath String) Command
  deriving (Eq, Show, Ord)
