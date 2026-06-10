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

unifyFullyAnchored :: Bool -> NID -> FullyAnchored -> Maybe NID
unifyFullyAnchored isRoot nid = \case
  FSpecific fnid
    | not isRoot && nid /= fnid -> Nothing
    | otherwise -> pure fnid
  FJoinPoint {..}
    | nid `notMember` excluding -> pure nid
    | otherwise -> Nothing

unifyAnchors :: Bool -> Anchor -> Anchor -> Maybe Anchor
unifyAnchors isRoot a1 a2 = case (a1, a2) of
  (Specific n1, Specific n2)
    | n1 == n2 -> Just (Specific n1)
    | otherwise -> Nothing
  (Specific n, JoinPoint {..})
    | n `notMember` excluding -> Just (Specific n)
    | otherwise -> Nothing
  (JoinPoint {..}, Specific n)
    | n `notMember` excluding -> Just (Specific n)
    | otherwise -> Nothing
  (JoinPoint {excluding = ex1}, JoinPoint {excluding = ex2}) ->
    Just (JoinPoint {excluding = ex1 `union` ex2})
  (Unanchored, a) -> Just a
  (a, Unanchored) -> Just a
