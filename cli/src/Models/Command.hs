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
--
-- Every path argument is a 'ParsedPath': its directives (@%targets(…)@,
-- @%last@, @%history(n)@, URLs) are expanded when the command runs,
-- relative to the location and history at that moment (see
-- 'Graph.Command.expandDirectives').
module Models.Command where

import Models.Path.ParsedPath
import MyPrelude

data Command
  = -- | cd
    ChangeNode ParsedPath
  | -- | d
    Dualize
  | -- | mk
    Make ParsedPath
  | -- | mk --allow-orphans: also create nodes not connected to the graph
    MakeAllowingOrphans ParsedPath
  | -- | mg
    Merge ParsedPath
  | -- | t
    Tag ParsedPath ParsedPath
  | Text Text Text
  | -- | desc
    Describe Text
  | -- | rm
    Remove ParsedPath
  | -- | rmnf
    RemoveNode ParsedPath
  | -- | at
    At ParsedPath Command
  | -- | dd
    Dedup Text
  | -- | nid
    NodeId
  | -- | :d
    Debug
  | -- | si
    ShowImage
  | -- | :i
    Import FilePath
  | -- | wget
    ImportUrl URI
  | -- | fsck
    Check
  | -- | fix
    Fix
  | -- | mv
    Move ParsedPath ParsedPath
  | -- | rn [--all-parents]
    Rename DestinationParents ParsedPath ParsedPath
  | -- | cp: copy the selected edge into a destination node
    Copy ParsedPath ParsedPath
  | -- | cl, clone, alias [--all-parents]: relabel without deleting the
    -- selected edge
    Alias DestinationParents ParsedPath ParsedPath
  | -- | vi
    Edit
  | -- | back: Go back in history by a certain number of steps. Greater number
    -- than amount of history goes maximum amount backwards. Negative number
    -- attempts to go forward in history if there is any recorded.
    Back Int
  | -- | Execute a list of commands sequentially
    Seq (TwoElemList Command)
  | -- | preview, ls
    Preview ParsedPath
  deriving (Eq, Show, Ord, Generic)

-- | How many parents a @rn@/@cl@ destination may create the edge under: the
-- prefix before its final label must resolve to one node unless the command
-- was given @--all-parents@.
data DestinationParents
  = OneParent
  | AllParents
  deriving (Eq, Show, Ord, Generic)
