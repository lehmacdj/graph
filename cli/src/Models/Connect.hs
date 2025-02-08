-- | Utilities for dealing with connects and following them.
module Models.Connect where

import Control.Lens (filtered)
import Data.Set.Lens
import Models.Graph
import MyPrelude

pairOfConnect :: Connect t -> (t, NID)
pairOfConnect (Connect x nid) = (x, nid)

-- | Follow a connect if one exists returning the matching id if it is present.
-- We can imagine this as pattern matching. If we have the following graph:
--
-- n0 -a> n1
--  |
--  b
--  \/
--  n2
--
-- And outN0 is the @outgoingConnectsOf n0@, then @matchConnect "a" n0 = n2@.
-- TODO: make this return (Set NID)
matchConnect :: ValidTransition t => t -> Set (Connect t) -> [NID]
matchConnect x cs =
  map (view #node)
    . filter ((== x) . view #transition)
    $ toList cs

-- | selfLoopify n1 n2 is a function that makes self loops on n1 self loops on n2
selfLoopify ::
  ValidTransition t =>
  NID ->
  NID ->
  Set (Connect t) ->
  Set (Connect t)
selfLoopify nid nid' = (setmapped . #node . filtered (== nid)) .~ nid'
