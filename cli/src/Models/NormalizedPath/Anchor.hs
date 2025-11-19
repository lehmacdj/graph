module Models.NormalizedPath.Anchor where

import Models.NID
import MyPrelude

data Anchor
  = Unanchored
  | JoinPoint {excluding :: Set NID}
  | Specific NID
  deriving stock (Show, Eq, Ord, Generic, Lift)
  deriving anyclass (NFData)

data FullyAnchored
  = FJoinPoint {excluding :: Set NID}
  | FSpecific NID
  deriving stock (Show, Eq, Ord, Generic, Lift)
  deriving anyclass (NFData)

fullyAnchor :: Anchor -> FullyAnchored
fullyAnchor = \case
  Unanchored -> FJoinPoint mempty
  JoinPoint {..} -> FJoinPoint {..}
  Specific nid -> FSpecific nid

fullySpecific :: Anchor -> Maybe NID
fullySpecific = \case
  Unanchored -> Nothing
  JoinPoint {} -> Nothing
  Specific nid -> Just nid
