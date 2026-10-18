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

import Models.NID (NID)
import Models.Path.ParsedPath
import MyPrelude

data Command
  = -- | cd
    ChangeNode ParsedPath
  | -- | d
    Dualize
  | -- | mk [--max-repairs N]
    Make MaxRepairs ParsedPath
  | -- | mg [--keep] p1 [--keep] p2 …: merge the nodes, one per argument,
    -- into one; at most one argument is marked @--keep@ (see 'MergeArgument')
    Merge (TwoElemList MergeArgument)
  | -- | t, tag [--max-repairs N]
    Tag MaxRepairs ParsedPath ParsedPath
  | Text Text Text
  | -- | desc
    Describe Text
  | -- | rm [--dry-run]
    Remove RemoveMode ParsedPath
  | -- | rmnf [--dry-run]
    RemoveNode RemoveMode ParsedPath
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
  | -- | fsck
    Check
  | -- | fix [--dry-run] [@nid…]: repair the damage that fsck lists, or only
    -- the damage that involves the given nodes
    Fix FixMode FixScope
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

-- | The option @--max-repairs N@ of @mk@ and @tag@, if it was given: how
-- many walks the command may make of its path. Without it the limit is
-- 'Models.PathPattern.Residuate.defaultMaxRepairs'.
type MaxRepairs = Maybe RepairsLimit

-- | An argument of @mg@: a path that must resolve to a single node. The
-- 'Kept' node survives the merge, keeping its NID and data.
data MergeArgument
  = Merged ParsedPath
  | Kept ParsedPath
  deriving (Eq, Show, Ord, Generic)

mergeArgumentPath :: MergeArgument -> ParsedPath
mergeArgumentPath = \case
  Merged p -> p
  Kept p -> p

isKept :: MergeArgument -> Bool
isKept = \case
  Kept _ -> True
  Merged _ -> False

-- | How many parents a @rn@/@cl@ destination may create the edge under: the
-- prefix before its final label must resolve to one node unless the command
-- was given @--all-parents@.
data DestinationParents
  = OneParent
  | AllParents
  deriving (Eq, Show, Ord, Generic)

-- | Whether @fix@ makes its repairs or only lists them (@fix --dry-run@)
data FixMode
  = ApplyRepairs
  | ListRepairs
  deriving (Eq, Show, Ord, Generic)

-- | Which damage @fix@ repairs: all of it, or the damage that involves one
-- of the given nodes (@fix \@nid…@), as 'Models.Damage.damagedNodes' has it
-- (see 'Graph.Fix.inScope')
data FixScope
  = AllDamage
  | DamageOf (Set NID)
  deriving (Eq, Show, Ord, Generic)

-- | Whether @rm@/@rmnf@ delete what their path selects or only list what
-- they would delete (@--dry-run@)
data RemoveMode
  = DeleteSelected
  | ListDeletions
  deriving (Eq, Show, Ord, Generic)

-- | Whether a command needs the graph opened: its origin and system nodes
-- there. @fsck@ and @fix@ don't, so that they can run on a graph that is too
-- damaged for that. A sequence doesn't either: each of its commands is asked
-- when it runs, so that @fix ; ls@ repairs the graph and then lists it.
needsOpenGraph :: Command -> Bool
needsOpenGraph = \case
  Check -> False
  Fix _ _ -> False
  Seq _ -> False
  _ -> True
