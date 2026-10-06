{-# LANGUAGE UndecidableInstances #-}

module Models.Path
  ( Path' (..),
    UnionKind (..),
    AlternativesLimit (..),
    RepairsLimit (..),
    PathLimits (..),
    showLimits,
    OverrideSettings (..),
    PathPhase (..),
    SourceRange (..),
    Directive (..),
    NIDVal,
    IncompleteNID (..),
    HoleVal,
    DirectiveVal,
    DefaultUnionVal,
    WithLimitsVal,
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
-- PathPattern -- ^ embed an already compiled pattern, would be useful as an optimization
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
  | -- | A union: @l | r@ ('Relational') or @l ++ r@ ('Shared').
    Union UnionKind (Path' p) (Path' p)
  | -- | @l + r@, a union whose kind depends on the command: like a
    -- directive it is lowered by 'handleDirectivesWith', which is told the
    -- kind. It does not exist in the Prenormal phase.
    DefaultUnion (DefaultUnionVal p) (Path' p) (Path' p)
  | -- | The path inside an @%override@ that sets a limit, after directives
    -- are expanded: with @max-alternatives@, compilation allows the
    -- relational unions in it to make that many alternatives; with
    -- @max-repairs@, residuation allows that many candidate repairs. It
    -- only exists in the Prenormal phase.
    WithLimits (WithLimitsVal p) PathLimits (Path' p)
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

infixl 6 ::&

-- | How a union chooses when distribution copies it.
data UnionKind
  = -- | @|@: every copy chooses a side independently
    Relational
  | -- | @++@: one side is chosen for the whole path, and copies share it
    Shared
  deriving (Eq, Ord, Show, Generic, Lift)

-- | How many alternatives compiling a path may make.
data AlternativesLimit = AtMostAlternatives Int | UnlimitedAlternatives
  deriving (Eq, Ord, Show, Generic, Lift)

-- | How many candidate repairs residuating a path may make.
data RepairsLimit = AtMostRepairs Int | UnlimitedRepairs
  deriving (Eq, Ord, Show, Generic, Lift)

-- | The limits an @%override@ sets; 'Nothing' keeps the outer one.
data PathLimits = PathLimits
  { -- | how many alternatives the relational unions inside may make
    maxAlternatives :: Maybe AlternativesLimit,
    -- | how many candidate repairs residuation may make
    maxRepairs :: Maybe RepairsLimit
  }
  deriving (Eq, Ord, Show, Generic, Lift)

-- | Settings an @%override@ changes; 'Nothing' keeps the outer one.
data OverrideSettings = OverrideSettings
  { -- | the union @+@ is lowered to
    plus :: Maybe UnionKind,
    -- | how many alternatives the relational unions inside may make
    maxAlternatives :: Maybe AlternativesLimit,
    -- | how many candidate repairs residuation may make
    maxRepairs :: Maybe RepairsLimit
  }
  deriving (Eq, Ord, Show, Generic, Lift)

-- | The limits of an override, if it sets one.
overrideLimits :: OverrideSettings -> Maybe PathLimits
overrideLimits settings
  | isNothing settings.maxAlternatives && isNothing settings.maxRepairs =
      Nothing
  | otherwise =
      Just
        PathLimits
          { maxAlternatives = settings.maxAlternatives,
            maxRepairs = settings.maxRepairs
          }

-- | The settings of an @%override@ as written: @key = value@ each.
showLimits :: PathLimits -> [String]
showLimits limits =
  [ "max-alternatives = " <> case l of
      AtMostAlternatives n -> show n
      UnlimitedAlternatives -> "unlimited"
  | Just l <- [limits.maxAlternatives]
  ]
    ++ [ "max-repairs = " <> case l of
           AtMostRepairs n -> show n
           UnlimitedRepairs -> "unlimited"
       | Just l <- [limits.maxRepairs]
       ]

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

-- | @+@ only exists before directives are expanded.
type family DefaultUnionVal (p :: PathPhase) :: Type where
  DefaultUnionVal 'Partial = ()
  DefaultUnionVal 'WithDirectives = ()
  DefaultUnionVal 'Prenormal = Void

-- | 'WithLimits' only exists after directives are expanded.
type family WithLimitsVal (p :: PathPhase) :: Type where
  WithLimitsVal 'Partial = Void
  WithLimitsVal 'WithDirectives = Void
  WithLimitsVal 'Prenormal = ()

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
    -- nodes, e.g. `""/(@ & %targets(#foo/*))` resolves the transitions
    -- which end in a node tagged `#foo`
    -- This can also be used to bake the current location into a later location
    -- in a path e.g. `*/%targets(@)` resolves only transitions from the
    -- current location to itself.
    Targets (Path' p)
  | -- | A splice of a Haskell expression that resolves to a Path
    -- This is used by the QuasiQuoter, the CLI does not support this
    Splice String
  | -- | Triggers the import of a URI to the graph, resolving to the NID of
    -- the imported resource
    HttpResource URI
  | -- | @%override({plus = shared}, p)@: expand @p@ with these settings
    -- changed; an inner override wins
    Override OverrideSettings (Path' p)
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
  Override settings p ->
    showString "%override({"
      . showString (intercalate ", " (overrideSettingsList settings))
      . showString "}, "
      . shows p
      . showString ")"
  where
    overrideSettingsList settings =
      [ "plus = " <> case k of
          Shared -> "shared"
          Relational -> "relational"
      | Just k <- [settings.plus]
      ]
        ++ foldMap showLimits (overrideLimits settings)

instance
  (Path'Constraints Show p) =>
  Show (Directive p)
  where
  showsPrec _ = showsDirective

type family DirectiveVal (p :: PathPhase) :: Type where
  DirectiveVal 'Partial = Directive 'Partial
  DirectiveVal 'WithDirectives = Directive 'WithDirectives
  DirectiveVal 'Prenormal = Void

-- | Expand directives, then lower each @+@ to a union kind: the one given
-- (or fail in @g@), or the one the innermost @%override@ around it sets. A directive expands to a path that may
-- contain @+@ (e.g. @%targets@), which is lowered like any other.
handleDirectivesWith ::
  (Monad g) =>
  g UnionKind ->
  (SourceRange -> Directive 'WithDirectives -> g (Path' 'WithDirectives)) ->
  Path' 'WithDirectives ->
  g (Path' 'Prenormal)
handleDirectivesWith plus interpretDirective = \case
  One -> pure One
  Zero -> pure Zero
  Wild -> pure Wild
  RegexMatch r -> pure (RegexMatch r)
  Literal x -> pure (Literal x)
  Absolute nid -> pure (Absolute nid)
  ExcludingNIDs nids -> pure (ExcludingNIDs nids)
  Backwards' p -> Backwards' <$> go p
  l ::/ r -> (::/) <$> go l <*> go r
  Union k l r -> Union k <$> go l <*> go r
  DefaultUnion () l r -> Union <$> plus <*> go l <*> go r
  l ::& r -> (::&) <$> go l <*> go r
  Directive _ (Override settings p) ->
    maybe id (WithLimits ()) (overrideLimits settings)
      <$> handleDirectivesWith
        (maybe plus pure settings.plus)
        interpretDirective
        p
  Directive ann directive -> go =<< interpretDirective ann directive
  where
    go = handleDirectivesWith plus interpretDirective

handleDirectivesQ ::
  -- | the kind of each @+@
  Q UnionKind ->
  -- | function returning a TH expression that has type @Path 'Prenormal@
  (SourceRange -> Directive 'WithDirectives -> Q Exp) ->
  Path' 'WithDirectives ->
  Q Exp
handleDirectivesQ plus interpretDirective = \case
  One -> [|One|]
  Zero -> [|Zero|]
  Wild -> [|Wild|]
  RegexMatch r -> [|RegexMatch r|]
  Literal x -> [|Literal x|]
  Absolute nid -> [|Absolute nid|]
  ExcludingNIDs nids -> [|ExcludingNIDs nids|]
  Backwards' p -> [|Backwards' $(go p)|]
  l ::/ r -> [|$(go l) ::/ $(go r)|]
  Union k l r -> [|Union k $(go l) $(go r)|]
  DefaultUnion () l r -> do
    k <- plus
    [|Union k $(go l) $(go r)|]
  l ::& r -> [|$(go l) ::& $(go r)|]
  Directive _ (Override settings p) ->
    let inner =
          handleDirectivesQ
            (maybe plus pure settings.plus)
            interpretDirective
            p
     in case overrideLimits settings of
          Nothing -> inner
          Just l -> [|WithLimits () l $inner|]
  Directive ann directive -> interpretDirective ann directive
  where
    go = handleDirectivesQ plus interpretDirective

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
      Union k l r -> Union k <$> go l <*> go r
      DefaultUnion () l r -> DefaultUnion () <$> go l <*> go r
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
      Override settings p -> Override settings <$> go p

-- | helper for producing the necessary constraints for a Path'
type Path'Constraints ::
  (Type -> Constraint) -> PathPhase -> Constraint
type Path'Constraints c p =
  ( c (DirectiveVal p),
    c (HoleVal p),
    c (NIDVal p),
    c (DefaultUnionVal p),
    c (WithLimitsVal p)
  )

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
showsPath d (Union k l r) =
  showParen (d > 5) $
    showsPrec 6 l . showString (unionSymbol k) . showsPrec 6 r
  where
    unionSymbol Relational = " | "
    unionSymbol Shared = " ++ "
showsPath _ (WithLimits _ l p) =
  showString "%override({"
    . showString (intercalate ", " (showLimits l))
    . showString "}, "
    . shows p
    . showString ")"
showsPath d (DefaultUnion _ l r) =
  showParen (d > 5) $
    showsPrec 6 l . showString " + " . showsPrec 6 r
showsPath _ (Directive _ d) = shows d
showsPath _ (Hole err) = shows err

instance (Path'Constraints Show p) => Show (Path' p) where
  showsPrec = showsPath @p

deriving instance (Path'Constraints Eq p) => Eq (Path' p)

deriving instance (Path'Constraints Ord p) => Ord (Path' p)
