{-# LANGUAGE UndecidableInstances #-}

module Models.Path
  ( Path' (..),
    PathPhase (..),
  )
where

import Models.NID
import MyPrelude

-- | The most general path type. Parametrized with a phase, a functor that
-- allows wrapping nested paths, and the raw type of literals/transitions.
--
-- There are also submodules that expose simpler path types with common
-- parameters, e.g. @Models.Path.SimpleElaborated@ exposes the simple Path type
--
-- There's some constructors that I'd like to add, but haven't either because I
-- haven't needed them and/or because they would complicate the implementation
-- more than is worthwhile:
-- Path t :\ Path t -- ^ set minus (useful with wild to restrict)
-- Negate (Path t) -- ^ negate a path, if included obsolesces other operators
-- KleeneStar (Path t) -- ^ necessary for expressing arbitrary graph traversals as paths
--
-- WARNING: when updating this type, update the {-# COMPLETE #-} pragmas
-- in @Models.Path.*@ submodules too so that completeness checking works as
-- expected
data Path' (p :: PathPhase) f t
  = -- | Stay at a specific location / the current location.
    -- Acts as a join point forcing intersections of a path that end in it to
    -- match at the same node (e.g. `(a/@ & b)/c` is equivalent to `(a & b)
    -- /@/c`)
    One
  | -- | Zero path (bottom element in path algebra)
    Zero
  | -- | Matches any single transition
    Wild
  | -- | match a regex
    RegexMatch CheckedRegex
  | Literal t
  | -- | marks a specific NID. Acts like an anchor if after a :/, and as a root
    -- if at the start of a path. Creates Pointlike paths when :&-ed with
    -- other paths
    Absolute NID
  | -- | backlink-ify. traversing Backwards (Literal t) traverses the backlink
    -- (Literal t) instead, likewise for Wild. It inverts the order of paths and
    -- has no effect on node-location specifying path components or
    -- intersection/union operators
    Backwards' (f (Path' p f t))
  | -- | sequence
    f (Path' p f t) ::/ f (Path' p f t)
  | -- | union
    f (Path' p f t) ::+ f (Path' p f t)
  | -- | intersection
    f (Path' p f t) ::& f (Path' p f t)
  | -- | A location in the history
    LocationFromHistory (LocationFromHistoryVal p)
  deriving (Generic)

infixl 7 ::/

infixl 5 ::+

infixl 6 ::&

-- * Trees that grow machinery

-- | Compilation phase of a path
-- Used for "Trees That Grow" style extensions
data PathPhase
  = -- | Unelaborated paths may contain directives that need to be resolved
    -- before the path can be normalized
    Unelaborated
  | -- | Elaborated paths are fully resolved and ready for normalization
    Elaborated

type family LocationFromHistoryVal (p :: PathPhase) where
  LocationFromHistoryVal 'Unelaborated = Int
  LocationFromHistoryVal 'Elaborated = Void

-- | helper for producing the necessary constraints for a Path'
type Path'Constraints ::
  (Type -> Constraint) -> PathPhase -> (Type -> Type) -> Type -> Constraint
type Path'Constraints c p f t = (c t, c (LocationFromHistoryVal p), c (f (Path' p f t)))

-- | helper for producing the necessary constraints for a Path' excluding the
-- functorial type parameter
type PathConstraints ::
  (Type -> Constraint) -> PathPhase -> Type -> Constraint
type PathConstraints c p t = (c t, c (LocationFromHistoryVal p))

-- * Instances

deriving instance (Path'Constraints Lift p f t) => Lift (Path' p f t)

showsPath ::
  (PathConstraints Show p t) =>
  (Int -> f (Path' p f t) -> ShowS) ->
  Int ->
  Path' p f t ->
  ShowS
showsPath _ _ One = showString "@"
showsPath _ _ Zero = showString "!"
showsPath _ _ Wild = showString "*"
showsPath _ _ (Literal x) = shows x
showsPath _ _ (RegexMatch r) = shows r
showsPath _ _ (Absolute nid) = showString "@" . shows nid
showsPath showF d (Backwards' p) =
  showParen (d > 8) $ showString "~" . showF 8 p
showsPath showF d (l ::/ r) =
  showParen (d > 7) $ showF 8 l . showString " / " . showF 8 r
showsPath showF d (l ::& r) =
  showParen (d > 6) $ showF 7 l . showString " & " . showF 7 r
showsPath showF d (l ::+ r) =
  showParen (d > 5) $ showF 6 l . showString " + " . showF 6 r
showsPath _ _ (LocationFromHistory n) = showString "%location(" . shows n . showString ")"

instance (Show1 f, Path'Constraints Show p f t) => Show (Path' p f t) where
  showsPrec = showsPath showsPath1
    where
      showsPath1 :: (Show1 f, Show t) => Int -> f (Path' p f t) -> ShowS
      showsPath1 = liftShowsPrec (showsPath showsPath1) showList

instance {-# OVERLAPPING #-} (PathConstraints Show p t) => Show (Path' p Identity t) where
  showsPrec = showsPath skipIdentity
    where
      skipIdentity :: (Show t) => Int -> Identity (Path' p Identity t) -> ShowS
      skipIdentity prec (Identity p) = showsPath skipIdentity prec p

deriving instance (Eq1 f, Path'Constraints Eq p f t) => Eq (Path' p f t)

deriving instance (Ord1 f, Path'Constraints Ord p f t) => Ord (Path' p f t)
