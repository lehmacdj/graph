{-# LANGUAGE UndecidableInstances #-}

module Models.Path
  ( Path' (..),
    PathPhase (..),
    SourceRange (..),
    Directive (..),
    NIDVal,
    IncompleteNID (..),
    HoleVal,
    DirectiveVal,
    Path'Constraints,
    handleDirectivesWith,
    handleDirectivesQ,
    parsedFromPartial,
  )
where

import Language.Haskell.TH (Exp, Q)
import Models.NID
import MyPrelude
import Utils.Parsing.Common

-- | The most general path type. Parametrized with a phase which is used for
-- "Trees that grow" style machinery.
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
-- Path :\ Path -- ^ set minus (useful with wild to restrict)
-- Negate Path -- ^ negate a path, if included obsolesces other operators
--    this is very hard, consider not(a/b), which theoretically would need to
--    include paths of any length that don't match a/b
-- KleeneStar Path -- ^ necessary for expressing arbitrary graph traversals as paths
-- NormalizedPath -- ^ embed an already normalized path, would be useful as an optimization
--
-- WARNING: when updating this type, update the {-# COMPLETE #-} pragmas
-- in @Models.Path.*@ submodules too so that completeness checking works as
-- expected
data Path' (p :: PathPhase)
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
  | Literal Text
  | -- | marks a specific NID. Acts like an anchor if after a :/, and as a root
    -- if at the start of a path. Creates Pointlike paths when :&-ed with
    -- other paths
    Absolute (NIDVal p)
  | -- | negative assertion for some set of NIDs, written `!{@1, @2}`
    -- otherwise acts as an anchor, i.e. `@` when used as a node-location
    ExcludingNIDs (Set (NIDVal p))
  | -- | backlink-ify. traversing Backwards (Literal t) traverses the backlink
    -- (Literal t) instead, likewise for Wild. It inverts the order of paths and
    -- has no effect on node-location specifying path components or
    -- intersection/union operators
    Backwards' (Path' p)
  | -- | sequence
    Path' p ::/ Path' p
  | -- | union
    Path' p ::+ Path' p
  | -- | intersection
    Path' p ::& Path' p
  | -- | Directives are command like things that may appear in paths.
    -- They can be interpreted into a path using application state, etc.
    -- using 'handleDirectivesWith'.
    -- Currently I'm only including the source range for directives, but I'd
    -- like to add it to all path components eventually
    Directive SourceRange (DirectiveVal p)
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

data IncompleteNID = IncompleteNID
  { offset :: Int,
    sourceRange :: SourceRange,
    base62Chars :: Text
  }
  deriving (Generic, Eq, Ord, Show, Lift)

type family NIDVal (p :: PathPhase) :: Type where
  NIDVal 'Partial = Either IncompleteNID NID
  NIDVal 'WithDirectives = NID
  NIDVal 'Prenormal = NID

-- | Effectively this is an effectful sublanguage of the path language
-- Directives must be interpreted using handleDirectivesWith or
-- handleDirectivesQ before paths may be normalized by the pure path
-- normalization algorithm
data Directive (p :: PathPhase)
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
    Targets (Path' p)
  | -- | A splice of a Haskell expression that resolves to a Path
    -- This is used by the QuasiQuoter, the CLI does not support this
    Splice String
  | -- | Triggers the import of a URI to the graph, resolving to the NID of
    -- the imported resource
    HttpResource URI
  deriving (Generic)

deriving instance
  (Path'Constraints Eq p) =>
  Eq (Directive p)

deriving instance
  (Path'Constraints Ord p) =>
  Ord (Directive p)

deriving instance
  (Path'Constraints Lift p) =>
  Lift (Directive p)

showsDirective ::
  (Path'Constraints Show p) =>
  Directive p ->
  ShowS
showsDirective = \case
  HttpResource uri -> renderURIShowS uri
  LocationFromHistory i -> showString "%history(" . shows i . showString ")"
  Targets p -> showString "%targets(" . shows p . showString ")"
  Splice expr -> showString "%{" . showString expr . showString "}"

instance
  (Path'Constraints Show p) =>
  Show (Directive p)
  where
  showsPrec _ = showsDirective

type family DirectiveVal (p :: PathPhase) :: Type where
  DirectiveVal 'Partial = Directive 'Partial
  DirectiveVal 'WithDirectives = Directive 'WithDirectives
  DirectiveVal 'Prenormal = Void

handleDirectivesWith ::
  (Applicative g) =>
  (SourceRange -> Directive 'WithDirectives -> g (Path' 'Prenormal)) ->
  Path' 'WithDirectives ->
  g (Path' 'Prenormal)
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
  -- | function returning a TH expression that has type @Path 'Prenormal@
  (SourceRange -> Directive 'WithDirectives -> Q Exp) ->
  Path' 'WithDirectives ->
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

-- | Convert a PartialPath to a ParsedPath, collecting all parse errors.
-- Returns either a list of all errors found, or a successfully parsed path.
parsedFromPartial ::
  Path' 'Partial ->
  Either (NonNull [ParseError']) (Path' 'WithDirectives)
parsedFromPartial = (.either) . go
  where
    go ::
      Path' 'Partial ->
      Validation (NonNull [ParseError']) (Path' 'WithDirectives)
    go = \case
      One -> pure One
      Zero -> pure Zero
      Wild -> pure Wild
      RegexMatch r -> pure (RegexMatch r)
      Literal x -> pure (Literal x)
      Absolute nid -> Absolute <$> goNID nid
      ExcludingNIDs nids -> ExcludingNIDs <$> traverseSetInOrder goNID nids
      Backwards' p -> Backwards' <$> go p
      l ::/ r -> (::/) <$> go l <*> go r
      l ::+ r -> (::+) <$> go l <*> go r
      l ::& r -> (::&) <$> go l <*> go r
      Directive ann directive -> Directive ann <$> goDirective directive
      Hole err -> Validation (Left (singletonNN err))
    goNID ::
      Either IncompleteNID NID ->
      Validation (NonNull [ParseError']) NID
    goNID = \case
      (Left IncompleteNID {..}) ->
        Validation . Left . singletonNN $
          FancyError offset (singletonSet (ErrorCustom IncompleteFullNID {..}))
      (Right nid) -> pure nid
    goDirective = \case
      HttpResource uri -> pure (HttpResource uri)
      LocationFromHistory i -> pure (LocationFromHistory i)
      Targets p -> Targets <$> go p
      Splice s -> pure (Splice s)

-- | helper for producing the necessary constraints for a Path'
type Path'Constraints ::
  (Type -> Constraint) -> PathPhase -> Constraint
type Path'Constraints c p = (c (DirectiveVal p), c (HoleVal p), c (NIDVal p))

-- * Instances

deriving instance (Path'Constraints Lift p) => Lift (Path' p)

showsPath ::
  forall p.
  (Path'Constraints Show p) =>
  Int ->
  Path' p ->
  ShowS
showsPath _ One = showString "@"
showsPath _ Zero = showString "%never"
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

instance (Path'Constraints Show p) => Show (Path' p) where
  showsPrec = showsPath @p

deriving instance (Path'Constraints Eq p) => Eq (Path' p)

deriving instance (Path'Constraints Ord p) => Ord (Path' p)
