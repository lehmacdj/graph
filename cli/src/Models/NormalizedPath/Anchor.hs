module Models.NormalizedPath.Anchor where

import Models.NID
import MyPrelude

data Anchor
  = Unanchored
  | JoinPoint {excluding :: Set NID}
  | Specific NID
  deriving stock (Eq, Ord, Generic, Lift)
  deriving anyclass (NFData)

data FullyAnchored
  = FJoinPoint {excluding :: Set NID}
  | FSpecific NID
  deriving stock (Eq, Ord, Generic, Lift)
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

instance Show Anchor where
  showsPrec _ = showsAnchor

showsAnchor :: Anchor -> ShowS
showsAnchor = \case
  Unanchored -> id
  JoinPoint nids
    | null nids -> showString "@"
    | otherwise ->
        showString "!{"
          . showString (intercalate ", " $ map show (toList nids))
          . showString "}"
  Specific nid -> shows nid

instance Show FullyAnchored where
  showsPrec _ = showsFullyAnchored

showsFullyAnchored :: FullyAnchored -> ShowS
showsFullyAnchored = \case
  FJoinPoint nids
    | null nids -> showString "@"
    | otherwise ->
        showString "!{"
          . showString (intercalate ", " $ map show (toList nids))
          . showString "}"
  FSpecific nid -> shows nid
