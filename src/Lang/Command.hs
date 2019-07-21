module Lang.Command where

import Graph

data Command
  = ChangeNode String
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
  | ShowImage
  | SetBinaryData FilePath
  | Import FilePath
  | ImportUrl String
  deriving (Eq, Show, Ord)
