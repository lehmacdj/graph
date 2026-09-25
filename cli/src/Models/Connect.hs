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
-- The type of node identifiers is a parameter; in general we only use the type
-- alias 'Connect' that fixes it to 'NID'.
data Connect' n t = Connect
  { transition :: t,
    node :: n
  }
  deriving (Eq, Ord, Generic, NFData)

-- | A connect between nodes identified by 'NID'. See 'Connect'' for more
-- information.
type Connect t = Connect' NID t

instance (Show t, CompactNIDShow n) => CompactNodeShow (Connect' n t) where
  type Augmentation (Connect' n t) = Void
  minimumNidLength settings c = minimumNidLength settings c.node
  nshowSettings settings (Connect t nid) =
    nshowSettings settings nid ++ " via " ++ tshow t

instance (Show t, CompactNIDShow n) => Show (Connect' n t) where
  show = unpack . nshowDefault

instance
  (FromJSON t, FromJSON n, ValidTransitionNCS t) =>
  FromJSON (Connect' n t)

instance (ToJSON t, ToJSON n, ValidTransitionNCS t) => ToJSON (Connect' n t) where
  toEncoding = genericToEncoding defaultOptions

pairOfConnect :: Connect' n t -> (t, n)
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
matchConnect ::
  (ValidTransition t, ValidNID n) =>
  t ->
  Set (Connect' n t) ->
  [n]
matchConnect x = toListOf (matchedConnects x)

matchedConnects ::
  (ValidTransition t, ValidNID n) =>
  t ->
  Fold (Set (Connect' n t)) n
matchedConnects x = folded . filtered (\c -> c.transition == x) . #node

-- | selfLoopify n1 n2 is a function that makes self loops on n1 self loops on n2
selfLoopify ::
  (ValidTransition t, ValidNID n) =>
  n ->
  n ->
  Set (Connect' n t) ->
  Set (Connect' n t)
selfLoopify nid nid' = (setmapped . #node . filtered (== nid)) .~ nid'
