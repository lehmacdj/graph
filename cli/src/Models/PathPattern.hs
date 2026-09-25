-- | Route patterns: the normal form of a path. See
-- @docs/path-pattern-ir.md@ for the design.
--
-- A pattern is an ordered list of routes. Each route walks from a bound root
-- through hops to one output. Routes meet at shared vertices; a shared prefix
-- reuses the same 'EdgeId's. The pattern graph ('patternGraph') is derived
-- from the routes and is only an index for matching.
module Models.PathPattern
  ( -- * Types
    VarId (..),
    EdgeId (..),
    PVertex (..),
    NodeTest (..),
    Matcher (..),
    Direction (..),
    Hop (..),
    Route (..),
    PatternPath (..),
    PathQuery (..),
    EdgeSpec (..),
    PEdge (..),
    PAug (..),

    -- * Operations
    anyNode,
    meetTests,
    testAccepts,
    matcherAccepts,
    vertexTest,
    routeEnd,
    patternVertices,
    patternVars,
    patternEdges,
    edgeOrder,
    canonicalize,
    patternGraph,

    -- * Helpers (also the 'Show' syntax)
    query,
    pat,
    patWith,
    route,
    current,
    fixed,
    fixedN,
    var,
    except,
    exactly,
    out,
    inc,
    outW,
    inW,
    outM,
    inM,
    smallNIDIndex,
  )
where

import Data.Map.Strict qualified as Map
import Models.Edge
import Models.Graph (Graph', emptyGraph, insertEdge, insertNode, mapGraph)
import Models.NID
import Models.Node
import MyPrelude

newtype VarId = VarId Int
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

newtype EdgeId = EdgeId Int
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Show VarId where
  showsPrec d (VarId i) = showParen (d > 10) $ showString "VarId " . shows i

instance Show EdgeId where
  showsPrec d (EdgeId i) = showParen (d > 10) $ showString "EdgeId " . shows i

-- | A position in a pattern. 'Current' is bound by the caller, 'Fixed' is a
-- specific node, and 'Var' is any other position.
data PVertex = Current | Fixed NID | Var VarId
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

instance CompactNodeShow PVertex where
  type Augmentation PVertex = Void
  minimumNidLength settings = \case
    Fixed n -> minimumNidLength settings n
    _ -> 1
  nshowSettings settings = \case
    Current -> "current"
    Fixed n -> nshowSettings settings n
    Var (VarId i) -> "$" <> tshow i

-- | 'Except' with an empty set accepts any node.
data NodeTest = Exactly NID | Except (Set NID)
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

data Matcher = MLiteral Text | MWild | MRegex CheckedRegex
  deriving stock (Eq, Ord, Generic)

instance NFData Matcher where
  rnf = \case
    MLiteral t -> rnf t
    MWild -> ()
    MRegex r -> r `seq` ()

-- | The direction a hop is traversed in. 'In' follows an edge backwards.
data Direction = Out | In
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data Hop = Hop
  { edge :: EdgeId,
    direction :: Direction,
    matcher :: Matcher,
    target :: PVertex
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

data Route = Route {root :: PVertex, hops :: [Hop]}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Tests are only stored when they constrain something: 'Fixed' vertices
-- implicitly test 'Exactly' their NID and absent vertices accept any node.
data PatternPath = PatternPath
  { tests :: Map PVertex NodeTest,
    routes :: NonEmpty Route
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Alternatives written with @+@.
newtype PathQuery = PathQuery {alternatives :: Set PatternPath}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

-- | Everything about one pattern edge, oriented in traversal direction.
data EdgeSpec = EdgeSpec
  { source :: PVertex,
    direction :: Direction,
    matcher :: Matcher,
    target :: PVertex
  }
  deriving stock (Eq, Ord, Show, Generic)

-- * Tests and matchers

anyNode :: NodeTest
anyNode = Except mempty

meetTests :: NodeTest -> NodeTest -> Maybe NodeTest
meetTests (Exactly a) (Exactly b) = Exactly a <$ guard (a == b)
meetTests (Exactly a) (Except s) = Exactly a <$ guard (a `notMember` s)
meetTests (Except s) (Exactly a) = Exactly a <$ guard (a `notMember` s)
meetTests (Except s) (Except t) = Just (Except (s <> t))

testAccepts :: NodeTest -> NID -> Bool
testAccepts (Exactly a) n = a == n
testAccepts (Except s) n = n `notMember` s

matcherAccepts :: Matcher -> Text -> Bool
matcherAccepts = \case
  MLiteral t -> (== t)
  MWild -> const True
  MRegex r -> has (textRegexing r)

-- | The effective test of a vertex, including the implicit test of 'Fixed'.
vertexTest :: PatternPath -> PVertex -> NodeTest
vertexTest _ (Fixed n) = Exactly n
vertexTest p v = findWithDefault anyNode v p.tests

-- * Structure

routeEnd :: Route -> PVertex
routeEnd r = maybe r.root (.target) (lastMay r.hops)

-- | Every vertex, in order of first appearance.
patternVertices :: PatternPath -> [PVertex]
patternVertices p = ordNub (concatMap routeVertices (toList p.routes))
  where
    routeVertices r = r.root : map (.target) r.hops

patternVars :: PatternPath -> [VarId]
patternVars p = [v | Var v <- patternVertices p]

-- | Every pattern edge. A shared prefix appears once.
patternEdges :: PatternPath -> Map EdgeId EdgeSpec
patternEdges p = mapFromList (concatMap routeEdges (toList p.routes))
  where
    routeEdges r =
      zipWith
        (\s h -> (h.edge, EdgeSpec s h.direction h.matcher h.target))
        (r.root : map (.target) r.hops)
        r.hops

-- | Edge ids in order of first appearance.
edgeOrder :: PatternPath -> [EdgeId]
edgeOrder p = ordNub [h.edge | r <- toList p.routes, h <- r.hops]

-- | Renumber variables and edges in order of first appearance, so that
-- equal canonical patterns are syntactically equal.
canonicalize :: PatternPath -> PatternPath
canonicalize p =
  PatternPath
    { tests = Map.mapKeys vertex p.tests,
      routes = fmap goRoute p.routes
    }
  where
    varNames :: Map VarId VarId
    varNames = mapFromList (zip (patternVars p) (map VarId [0 ..]))
    edgeNames :: Map EdgeId EdgeId
    edgeNames = mapFromList (zip (edgeOrder p) (map EdgeId [0 ..]))
    vertex = \case
      Var v -> Var (findWithDefault v v varNames)
      other -> other
    goRoute r = Route (vertex r.root) (map goHop r.hops)
    goHop (Hop e dir m t) = Hop (findWithDefault e e edgeNames) dir m (vertex t)

-- * Derived pattern graph

-- | The transition type of the pattern graph. The id keeps parallel pattern
-- edges distinct, e.g. the two independent @*@ edges of @(* & *)/\@@.
data PEdge = PEdge {edgeId :: EdgeId, matcher :: Matcher}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

newtype PAug = PAug {test :: NodeTest}
  deriving stock (Eq, Ord, Show, Generic)

instance DefaultAugmentation PAug where
  defaultAugmentation = PAug anyNode

-- | The pattern as a graph, with edges in stored (not traversal)
-- orientation.
patternGraph :: PatternPath -> Graph' PVertex PEdge PAug
patternGraph p =
  mapGraph (\n -> PAug (vertexTest p n.nid)) $
    foldl'
      (flip insertEdge)
      (foldl' (\g v -> insertNode (emptyNode v) g) unit vertices)
      (map toEdge (mapToList (patternEdges p)))
  where
    unit = emptyGraph :: Graph' PVertex PEdge ()
    vertices = patternVertices p
    toEdge (e, spec) = case spec.direction of
      Out -> Edge spec.source (PEdge e spec.matcher) spec.target
      In -> Edge spec.target (PEdge e spec.matcher) spec.source

-- * Helpers

query :: [PatternPath] -> PathQuery
query = PathQuery . setFromList

pat :: [Route] -> PatternPath
pat = patWith []

patWith :: [(PVertex, NodeTest)] -> [Route] -> PatternPath
patWith ts rs = case rs of
  [] -> error "pat: a pattern needs at least one route"
  r : more -> PatternPath (mapFromList ts) (r :| more)

route :: PVertex -> [Hop] -> Route
route = Route

current :: PVertex
current = Current

fixed :: NID -> PVertex
fixed = Fixed

fixedN :: Int -> PVertex
fixedN = Fixed . smallNID

var :: Int -> PVertex
var = Var . VarId

except :: [Int] -> NodeTest
except = Except . setFromList . map smallNID

exactly :: Int -> NodeTest
exactly = Exactly . smallNID

outM, inM :: Int -> Matcher -> PVertex -> Hop
outM e = Hop (EdgeId e) Out
inM e = Hop (EdgeId e) In

out, inc :: Int -> Text -> PVertex -> Hop
out e = outM e . MLiteral
inc e = inM e . MLiteral

outW, inW :: Int -> PVertex -> Hop
outW e = outM e MWild
inW e = inM e MWild

-- | Inverse of 'smallNID' for small values, used to print NIDs readably.
smallNIDIndex :: NID -> Maybe Int
smallNIDIndex n = lookup n smallNIDs

smallNIDs :: Map NID Int
smallNIDs = mapFromList [(smallNID i, i) | i <- [0 .. 3843]]
{-# NOINLINE smallNIDs #-}

-- * Show, as helper expressions

showsNID :: Int -> NID -> ShowS
showsNID d n = case smallNIDIndex n of
  Just i -> shows i
  Nothing ->
    showParen (d > 10) $
      showString "unsafeNID " . shows (nidRepresentation n)

instance Show PVertex where
  showsPrec d = \case
    Current -> showString "current"
    Fixed n -> case smallNIDIndex n of
      Just i -> showParen (d > 10) $ showString "fixedN " . shows i
      Nothing -> showParen (d > 10) $ showString "fixed " . showsNID 11 n
    Var (VarId i) -> showParen (d > 10) $ showString "var " . shows i

instance Show NodeTest where
  showsPrec d = \case
    Exactly n -> case smallNIDIndex n of
      Just i -> showParen (d > 10) $ showString "exactly " . shows i
      Nothing ->
        showParen (d > 10) $ showString "Exactly " . showsNID 11 n
    Except s
      | all (isJust . smallNIDIndex) s ->
          showParen (d > 10) $
            showString "except "
              . shows (mapMaybe smallNIDIndex (toList s))
      | otherwise -> showParen (d > 10) $ showString "Except " . shows s

instance Show Matcher where
  showsPrec d = \case
    MLiteral t -> showParen (d > 10) $ showString "MLiteral " . shows t
    MWild -> showString "MWild"
    MRegex r -> showParen (d > 10) $ showString "MRegex " . shows r

instance Show Hop where
  showsPrec d (Hop (EdgeId e) dir m t) = showParen (d > 10) $ case m of
    MLiteral l ->
      showString name
        . showString " "
        . shows e
        . showString " "
        . shows l
        . showString " "
        . showsPrec 11 t
    MWild ->
      showString name
        . showString "W "
        . shows e
        . showString " "
        . showsPrec 11 t
    MRegex _ ->
      showString name
        . showString "M "
        . shows e
        . showString " "
        . showsPrec 11 m
        . showString " "
        . showsPrec 11 t
    where
      name = case dir of
        Out -> "out"
        In -> "inc"

instance Show Route where
  showsPrec d (Route r hs) =
    showParen (d > 10) $
      showString "route " . showsPrec 11 r . showString " " . shows hs

instance Show PatternPath where
  showsPrec d p =
    showParen (d > 10) $
      if null p.tests
        then showString "pat " . shows (toList p.routes)
        else
          showString "patWith "
            . shows (mapToList p.tests)
            . showString " "
            . shows (toList p.routes)

instance Show PathQuery where
  showsPrec d (PathQuery ps) =
    showParen (d > 10) $ showString "query " . shows (toList ps)
