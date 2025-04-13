-- | Utilities for dealing with connects and following them.
module Models.Connect
  ( module Models.Connect,
    module Models.Common,
  )
where

import Data.Aeson
import Data.Set.Lens
import Models.Common
import Models.NID
import MyPrelude

-- | A transition from/to a node to/from another node.
-- The first node isn't represented here, because this is used only in the node
-- structure where the first node is clear from context.
data Connect t = Connect
  { transition :: t,
    node :: NID
  }
  deriving (Eq, Ord, Generic, NFData)

instance (Show t) => CompactNodeShow (Connect t) where
  type Augmentation (Connect t) = Void
  minimumNidLength settings c = minimumNidLength settings c.node
  nshowSettings settings (Connect t nid) =
    nshowSettings settings nid ++ " via " ++ tshow t

instance (Show t) => Show (Connect t) where
  show = unpack . nshowDefault

instance (FromJSON t, ValidTransitionNCS t) => FromJSON (Connect t)

instance (ToJSON t, ValidTransitionNCS t) => ToJSON (Connect t) where
  toEncoding = genericToEncoding defaultOptions

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
matchConnect :: (ValidTransition t) => t -> Set (Connect t) -> [NID]
matchConnect x cs =
  map (view #node)
    . filter ((== x) . view #transition)
    $ toList cs

-- | selfLoopify n1 n2 is a function that makes self loops on n1 self loops on n2
selfLoopify ::
  (ValidTransition t) =>
  NID ->
  NID ->
  Set (Connect t) ->
  Set (Connect t)
selfLoopify nid nid' = (setmapped . #node . filtered (== nid)) .~ nid'
