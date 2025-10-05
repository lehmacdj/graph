-- | This module implements commands.
--
-- Style guide for commands for the future:
-- All commands and paths are interpreted relative to the current location
-- We can reintroduce the ability to execute commands relative to a different
-- location later via an `at` command that changes the location and then
-- changes it back.
-- This means that new nodes created and edges created etc start/end at the
-- current node
-- Commands that act on nodes should also act on at least deterministic
-- paths and if possible nondeterministic paths too
module Models.Command where

import Models.Path.ParsedPath
import Models.Path.Simple (Path)
import MyPrelude

data Command
  = -- | cd
    ChangeNode (Path String)
  | -- | d
    Dualize
  | -- | mk
    Make (Path String)
  | -- | mg
    Merge (Path String)
  | -- | cl
    Clone (Path String) String
  | -- | ls
    ListOut
  | -- | q
    Query (Path String) String
  | -- | t
    Tag (Path String) (Path String)
  | Text String String
  | -- | desc
    Describe String
  | -- | rm
    Remove (Path String)
  | -- | rmnf
    RemoveNode (Path String)
  | -- | at
    At (Path String) Command
  | -- | dd
    Dedup String
  | -- | flatten:
    -- Takes every node transition/* and creates edges transition to them.
    -- The purpose of this is to convert from a form where things have explicit
    -- but unnecessary names to a form where the edge is the only identifiying
    -- attribute.
    -- This can be considered to be the inverse of dedup in a sense
    Flatten String
  | -- | nid
    NodeId
  | -- | :d
    Debug
  | -- | si
    ShowImage
  | -- | :i
    Import FilePath
  | -- | wget
    ImportUrl String
  | -- | al for add link though its really just generally adding text
    AddText Text
  | -- | fsck
    Check
  | -- | fix
    Fix
  | -- | mv
    Move (Path String) (Path String)
  | -- | rn
    Rename (Path String) (Path String)
  | -- | alias, cp: while cp is misleading it's similar enough to shell cp that
    -- naming it cp is better for muscle memory + al is already taken by add link
    Alias (Path String) (Path String)
  | -- | vi
    Edit
  | -- | back: Go back in history by a certain number of steps. Greater number
    -- than amount of history goes maximum amount backwards. Negative number
    -- attempts to go forward in history if there is any recorded.
    Back Int
  | -- | Turn the largest non-cyclic graph from the current location into a
    -- file system structure representing the tree. Uses cloning when possible
    -- to minimize disk footprint; takes a filepath to write the tree to as an
    -- argument.
    Materialize FilePath
  | -- | Collect same transitions into a transition to a single node that
    -- transitions to the nodes the previous transition used to
    Collect String
  | -- | Execute a list of commands sequentially
    Seq (TwoElemList Command)
  | -- | debug-v2-path, v2
    V2Path (ParsedPath Text)
  deriving (Eq, Show, Ord, Generic)
