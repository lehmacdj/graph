module Command where

import Data.Graph

data Command
  = Quit
  | ChangeNode String
  | Dualize
  | MakeNode String
  | NodeId
  | ListOut
  | ListIn
  | AddEdgeTo Id String
  | AddEdgeFrom Id String
  | Dump FilePath
  | Load FilePath
  | Goto Id
  | Debug
  | RemoveEdgeOut String
  | RemoveEdgeIn String
  | CloneNode Id
  deriving (Eq, Show, Ord)
