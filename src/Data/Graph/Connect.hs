-- | Utilities for dealing with connects and following them.

module Data.Graph.Connect where

import Data.Set (Set)
import Data.Graph
import Data.Foldable

pairOfConnect :: Connect t -> (t, Id)
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
matchConnect :: TransitionValid t => t -> Set (Connect t) -> Maybe Id
matchConnect x cs = lookup x (pairOfConnect <$> toList cs)
