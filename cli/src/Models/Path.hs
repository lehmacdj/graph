{-# LANGUAGE UndecidableInstances #-}

module Models.Path
  ( Path' (..),
    PathPhase (..),
    SourceRange (..),
    Directive (..),
    handleDirectivesWith,
    handleDirectivesQ,
  )
where

import Language.Haskell.TH (Exp, Q)
import Models.NID
import MyPrelude
import Utils.Parsing.Common (ParseError', SourceRange (..))

-- | The most general path type. Parametrized with a phase and the raw type of
-- literals/transitions.
--
-- There are also submodules that expose simpler path types with common
-- parameters, e.g. @Models.Path.Simple@ exposes the simple Path type
--
-- Semantically a path has two kinds of components:
-- * Those specifying a node, e.g. `@12`, `@`, `@ & a/b`, `!@3`
-- * Those specifying a transition, e.g. `a`, `*`, `~a`
--
-- There's some constructors that I'd like to add, but haven't either because I
-- haven't needed them and/or because they would complicate the implementation
-- more than is worthwhile:
-- Path t :\ Path t -- ^ set minus (useful with wild to restrict)
-- Negate (Path t) -- ^ negate a path, if included obsolesces other operators
--    this is very hard, consider not(a/b), which theoretically would need to
--    include paths of any length that don't match a/b
-- KleeneStar (Path t) -- ^ necessary for expressing arbitrary graph traversals as paths
-- NormalizedPath -- ^ embed an already normalized path, would be useful as an optimization
--
-- WARNING: when updating this type, update the {-# COMPLETE #-} pragmas
-- in @Models.Path.*@ submodules too so that completeness checking works as
-- expected
data Path' (p :: PathPhase) t
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
  | -- | negative assertion for some set of NIDs, written `!{@1, @2}`
    -- otherwise acts as an anchor, i.e. `@` when used as a node-location
    ExcludingNIDs (Set NID)
  | -- | backlink-ify. traversing Backwards (Literal t) traverses the backlink
    -- (Literal t) instead, likewise for Wild. It inverts the order of paths and
    -- has no effect on node-location specifying path components or
    -- intersection/union operators
    Backwards' (Path' p t)
  | -- | sequence
    Path' p t ::/ Path' p t
  | -- | union
    Path' p t ::+ Path' p t
  | -- | intersection
    Path' p t ::& Path' p t
  | -- | Directives are command like things that may appear in paths.
    -- They can be interpreted into a path using application state, etc.
    -- using 'handleDirectivesWith'.
    -- Currently I'm only including the source range for directives, but I'd
    -- like to add it to all path components eventually
    Directive SourceRange (DirectiveVal p t)
  | -- | A hole representing a parse error. Only available in the Partial phase.
    Hole (HoleVal p)
  deriving (Generic)

infixl 7 ::/

infixl 5 ::+

infixl 6 ::&

-- * Trees that grow machinery

-- | Compilation phase of a path
-- Used for "Trees That Grow" style extensions
data PathPhase
  = -- | Partially parsed paths that may contain parse errors
    -- We will use this for providing precise completions of paths + as the
    -- return value of parsers that recover and return more than 1 parse failure
    Partial
  | -- | paths may contain directives that need to be resolved before the path
    -- can be normalized
    WithDirectives
  | -- | Plain paths that are ready for normalization. These paths have no
    -- dependencies necessary for normalization
    Prenormal

-- | Determines what type of holes/errors can appear in a path based on the phase
type family HoleVal (p :: PathPhase) :: Type where
  HoleVal 'Partial = ParseError'
  HoleVal 'WithDirectives = Void
  HoleVal 'Prenormal = Void

data Directive (p :: PathPhase) t
  = -- | Reference a location in the location history of the graph CLI
    -- resolves to `Absolute nid` where `nid` is the NID at that location
    LocationFromHistory Int
  | -- | Resolves to the set of targets of a path.
    -- Among other things this is useful for filtering to a specific set of
    -- nodes, e.g. `""/(@ & %targets(#foo/*))` materializes the transitions
    -- which end in a node tagged `#foo`
    -- This can also be used to bake the current location into a later location
    -- in a path e.g. `*/%targets(@)` materializes only transitions from the
    -- current location to itself.
    Targets (Path' p t)
  | -- | A splice of a Haskell expression that resolves to a Path
    -- This is used by the QuasiQuoter, the CLI does not support this
    Splice String
  deriving (Generic)

deriving instance
  (Path'Constraints Eq p t) =>
  Eq (Directive p t)

deriving instance
  (Path'Constraints Ord p t) =>
  Ord (Directive p t)

deriving instance
  (Path'Constraints Lift p t) =>
  Lift (Directive p t)

showsDirective ::
  (Path'Constraints Show p t) =>
  Directive p t ->
  ShowS
showsDirective = \case
  LocationFromHistory i -> showString "%history(" . shows i . showString ")"
  Targets p -> showString "%targets(" . shows p . showString ")"
  Splice expr -> showString "%{" . showString expr . showString "}"

instance
  (Path'Constraints Show p t) =>
  Show (Directive p t)
  where
  showsPrec _ = showsDirective

type family DirectiveVal (p :: PathPhase) (t :: Type) :: Type where
  DirectiveVal 'Partial t = Directive 'Partial t
  DirectiveVal 'WithDirectives t = Directive 'WithDirectives t
  DirectiveVal 'Prenormal _ = Void

handleDirectivesWith ::
  (Applicative g) =>
  (SourceRange -> Directive 'WithDirectives t -> g (Path' 'Prenormal t)) ->
  Path' 'WithDirectives t ->
  g (Path' 'Prenormal t)
handleDirectivesWith interpretDirective = \case
  One -> pure One
  Zero -> pure Zero
  Wild -> pure Wild
  RegexMatch r -> pure (RegexMatch r)
  Literal x -> pure (Literal x)
  Absolute nid -> pure (Absolute nid)
  ExcludingNIDs nids -> pure (ExcludingNIDs nids)
  Backwards' p ->
    Backwards' <$> handleDirectivesWith interpretDirective p
  l ::/ r ->
    (::/)
      <$> handleDirectivesWith interpretDirective l
      <*> handleDirectivesWith interpretDirective r
  l ::+ r ->
    (::+)
      <$> handleDirectivesWith interpretDirective l
      <*> handleDirectivesWith interpretDirective r
  l ::& r ->
    (::&)
      <$> handleDirectivesWith interpretDirective l
      <*> handleDirectivesWith interpretDirective r
  Directive ann directive -> interpretDirective ann directive

handleDirectivesQ ::
  (Lift t) =>
  -- | function returning a TH expression that has type @Path 'Prenormal t@
  (SourceRange -> Directive 'WithDirectives t -> Q Exp) ->
  Path' 'WithDirectives t ->
  Q Exp
handleDirectivesQ interpretDirective = \case
  One -> [|One|]
  Zero -> [|Zero|]
  Wild -> [|Wild|]
  RegexMatch r -> [|RegexMatch r|]
  Literal x -> [|Literal x|]
  Absolute nid -> [|Absolute nid|]
  ExcludingNIDs nids -> [|ExcludingNIDs nids|]
  Backwards' p ->
    [|Backwards' $(handleDirectivesQ interpretDirective p)|]
  l ::/ r ->
    [|
      $(handleDirectivesQ interpretDirective l)
        ::/ $(handleDirectivesQ interpretDirective r)
      |]
  l ::+ r ->
    [|
      $(handleDirectivesQ interpretDirective l)
        ::+ $(handleDirectivesQ interpretDirective r)
      |]
  l ::& r ->
    [|
      $(handleDirectivesQ interpretDirective l)
        ::& $(handleDirectivesQ interpretDirective r)
      |]
  Directive ann directive -> interpretDirective ann directive

-- | helper for producing the necessary constraints for a Path'
type Path'Constraints ::
  (Type -> Constraint) -> PathPhase -> Type -> Constraint
type Path'Constraints c p t = (c t, c (DirectiveVal p t), c (HoleVal p))

-- * Instances

deriving instance (Path'Constraints Lift p t) => Lift (Path' p t)

showsPath ::
  forall p t.
  (Path'Constraints Show p t) =>
  Int ->
  Path' p t ->
  ShowS
showsPath _ One = showString "@"
showsPath _ Zero = showString "!"
showsPath _ Wild = showString "*"
showsPath _ (Literal x) = shows x
showsPath _ (RegexMatch r) = shows r
showsPath _ (Absolute nid) = shows nid
showsPath _ (ExcludingNIDs nids) =
  let commaSep = intercalate "," $ mapSet show nids
   in showString "!{" . showString commaSep . showString "}"
showsPath d (Backwards' p) =
  showParen (d > 8) $ showString "~" . showsPrec 8 p
showsPath d (l ::/ r) =
  showParen (d > 7) $
    showsPrec 8 l . showString " / " . showsPrec 8 r
showsPath d (l ::& r) =
  showParen (d > 6) $
    showsPrec 7 l . showString " & " . showsPrec 7 r
showsPath d (l ::+ r) =
  showParen (d > 5) $
    showsPrec 6 l . showString " + " . showsPrec 6 r
showsPath _ (Directive _ d) = shows d
showsPath _ (Hole err) = shows err

instance (Path'Constraints Show p t) => Show (Path' p t) where
  showsPrec = showsPath @p

deriving instance (Path'Constraints Eq p t) => Eq (Path' p t)

deriving instance (Path'Constraints Ord p t) => Ord (Path' p t)
