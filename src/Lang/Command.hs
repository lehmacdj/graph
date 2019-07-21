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


-- Commands we want:
-- # mv path* path1: move many things to be linked to by the thing linked to
-- # rn path1 path1: rename a path to another path
-- rm path*: delete a bunch of links (primitive)
-- cd path1: access a new thing (primitive)
-- tf path* path1: create link to all paths in path1
-- ft path* path1: create link from all paths in path1
-- ls
-- at path1 command
-- cl path1 path*: clone a node to all specified paths
-- mg path*: make all of path* point to the same node
-- mk path*: make all of the paths exist
-- d: dualize
--
-- rn path1 path1 = lt path1 #;
--
-- when a new name needs to be used to refer to an item selected by path*
-- its string representation of its deterministic path component is used
--
-- mg, cl, rm, cd, ls, mk, tf, ft, d at least should be implemented as primitive
-- at least at first
-- mv p q = tf p q; rm p
-- rn p q = mk q; mg (p + q); rm p

-- other commands
-- pop <n> go back n steps through the history of operations
-- requires storing a history, which probably requires creating a new
-- data type that keeps track of cd and d
