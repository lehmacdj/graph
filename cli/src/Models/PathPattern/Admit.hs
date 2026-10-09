-- | Which paths @mk@ builds, and in which order it builds what is left
-- (docs/path-route-ir.md section 5).
--
-- Nothing here looks at the graph. A path is refused ('Refusal') for its
-- shape alone: its size as written ('expansionEstimate'), or what its
-- compiled routes ask for ('refusals'). For every other path the
-- operations of an alternative have an order ('planOf') in which each
-- comes after every operation that could create an edge it might follow,
-- so that they are done once, in that order, and none has to wait or be
-- done again.
module Models.PathPattern.Admit
  ( Refusal (..),
    Request (..),
    Step (..),
    Shrinking (..),
    refusalCode,
    expansionLimit,
    expansionEstimate,
    checkExpansion,
    shrinking,
    refusals,
    Operation (..),
    Plan (..),
    planOf,
  )
where

import Data.Graph (flattenSCC, stronglyConnComp)
import Data.Map.Lazy qualified as LazyMap
import Data.Map.Strict qualified as Map
import Models.NID
import Models.Path.Simple qualified as P
import Models.PathPattern
import MyPrelude

-- | Why a path is not built, whatever the graph holds. The paths in a
-- refusal are routes of the alternative from their start up to the step
-- meant, as they would be typed; M, V and C also name the alternative and
-- its steps, so that a message can say which written step each is a copy
-- of.
data Refusal
  = -- | X: the estimate of how many route steps the path has, written
    -- out, is more than 'expansionLimit': the estimate (which stops
    -- counting just above it). What would bring it within the limit
    -- depends on which unions were typed ('shrinking').
    ExpansionTooLarge Int
  | -- | R: a route comes back to an unnamed node it has visited: the
    -- route up to the first visit, and up to the return
    ReturnsToMeetingPoint P.Path P.Path
  | -- | M: a @*@ or regex step is one of several steps that place an
    -- unnamed node: that step, and the other steps into the node
    WildcardJoin PatternPath Step [Step]
  | -- | V: an alternative has more than one @*@ or regex step: the steps
    SeveralWildcards PatternPath [Step]
  | -- | C: operations that each might follow an edge another of them
    -- creates, or come after one that might, in a circle
    CircularSuppliers PatternPath [Request]
  deriving stock (Eq, Show, Generic)

-- | Which rewrites of a path that is too large would bring its estimate
-- within the limit, as its message advises. A union that was not typed
-- (what a @%targets(…)@ becomes) is left as it is in both.
data Shrinking = Shrinking
  { -- | The path written with its @+@ and @++@ unions outside the rest of
    -- it, which means the same, as in @a/(c & d) ++ b/(c & d)@ for
    -- @(a ++ b)/(c & d)@: every choice of their sides, as the sides of
    -- one union.
    movedOut :: Bool,
    -- | One side of each union kept, whichever sides they are.
    oneSide :: Bool
  }
  deriving stock (Eq, Show, Generic)

-- | A step of an alternative as a refusal names it: its route up to it,
-- and the step.
data Step = Step {route :: P.Path, edge :: EdgeId}
  deriving stock (Eq, Show, Generic)

-- | An operation as a refusal names it: the steps taken together;
-- several are the steps into one meeting point.
newtype Request = Request {steps :: [Step]}
  deriving stock (Eq, Show, Generic)

-- | The raw code of a refusal, as an error message ends with it.
refusalCode :: Refusal -> Text
refusalCode = \case
  ExpansionTooLarge _ -> "ExpansionTooLarge"
  ReturnsToMeetingPoint _ _ -> "ReturnsToMeetingPoint"
  WildcardJoin {} -> "WildcardJoin"
  SeveralWildcards _ _ -> "SeveralWildcards"
  CircularSuppliers _ _ -> "CircularSuppliers"

-- * The size of a path as written

-- | How many route steps, over all alternatives, a path may be estimated
-- to have and still be built.
expansionLimit :: Int
expansionLimit = 256

-- | An estimate, from the path as written, of how many route steps it
-- compiles to over all its alternatives; an empty route counts one. It
-- stops counting at one more than 'expansionLimit'. It takes one pass
-- over the written path and does not compile it.
--
-- The estimate is deliberately high: every union is counted as if each
-- conjunct next to it had a copy of its own, also where the compiler
-- shares it. So @(a ++ b)/(c & d & e & f & g)@ counts more than
-- @a/(c & d & e & f & g) ++ b/(c & d & e & f & g)@, which compiles to
-- the same.
expansionEstimate :: P.Path -> Int
expansionEstimate = estimate (const False)

-- | 'expansionEstimate', with the unions at the places for which the
-- first argument holds counted as the larger of their sides: what the
-- path comes to at most when one side of each of them is kept.
estimate :: (P.Place -> Bool) -> P.Path -> Int
estimate oneOf p0 =
  let (routes, alternatives, steps) = go [] p0
   in (routes `times` alternatives) `times` max 1 steps
  where
    cap = expansionLimit + 1
    -- both factors are at most the cap, so nothing overflows
    times a b = min cap (a * b)
    plus a b = min cap (a + b)
    power a n = foldl' (\acc _ -> if acc == cap then acc else acc `times` a) 1 [1 .. min cap n]
    -- the most routes of an alternative, the alternatives, and the most
    -- steps of a route
    go :: P.Place -> P.Path -> (Int, Int, Int)
    go place = \case
      P.One -> (1, 1, 0)
      P.Absolute _ -> (1, 1, 0)
      P.ExcludingNIDs _ -> (1, 1, 0)
      P.Zero -> (0, 0, 0)
      P.Wild -> (1, 1, 1)
      P.Literal _ -> (1, 1, 1)
      P.RegexMatch _ -> (1, 1, 1)
      P.Backwards p -> go (P.operand 0 place) p
      P.WithLimits _ _ p -> go (P.operand 0 place) p
      l P.:/ r ->
        let (r1, a1, s1) = go (P.operand 0 place) l
            (r2, a2, s2) = go (P.operand 1 place) r
         in (r1 `times` r2, power a1 r2 `times` power a2 r1, s1 `plus` s2)
      l P.:& r ->
        let (r1, a1, s1) = go (P.operand 0 place) l
            (r2, a2, s2) = go (P.operand 1 place) r
         in (r1 `plus` r2, a1 `times` a2, max s1 s2)
      P.Union _ l r ->
        let (r1, a1, s1) = go (P.operand 0 place) l
            (r2, a2, s2) = go (P.operand 1 place) r
         in ( max r1 r2,
              if oneOf place then max a1 a2 else a1 `plus` a2,
              max s1 s2
            )

-- | X, before a path is compiled.
checkExpansion :: P.Path -> Maybe Refusal
checkExpansion p =
  let n = expansionEstimate p
   in if n > expansionLimit then Just (ExpansionTooLarge n) else Nothing

-- | Which rewrites bring the estimate of a path within the limit, for the
-- unions at the places for which the first argument holds (those that
-- were typed).
-- Each rewrite is tried on every choice of the sides of those unions, a
-- pass over the path for each, as long as there are at most 'maxChoices'.
-- With more, the unions are not moved out, and one side of each is kept
-- only if the estimate with every union counted as its larger side is
-- within the limit: that is at least the estimate of every choice, but
-- can be much more, since it takes the largest part of each side apart
-- (a choice with the most routes of one union and the most steps of
-- another, which no side may have).
shrinking :: (P.Place -> Bool) -> P.Path -> Shrinking
shrinking written p = Shrinking {movedOut, oneSide}
  where
    within q = expansionEstimate q <= expansionLimit
    movedOut = case take (maxChoices + 1) (sidesChosen sharedTyped [] p) of
      sides@(c : rest@(_ : _))
        | length sides <= maxChoices -> within (foldl' (P.:++) c rest)
      _ -> False
    oneSide =
      any written (unionsIn [] p) && case take (maxChoices + 1) (sidesChosen (const . written) [] p) of
        sides
          | length sides <= maxChoices -> all within sides
          | otherwise -> estimate written p <= expansionLimit
    sharedTyped place = \case
      P.Union P.Shared _ _ -> written place
      _ -> False
    maxChoices = 1024
    -- the places of the unions
    unionsIn place = \case
      P.Union _ l r -> place : unionsIn (P.operand 0 place) l <> unionsIn (P.operand 1 place) r
      P.Backwards q -> unionsIn (P.operand 0 place) q
      P.WithLimits _ _ q -> unionsIn (P.operand 0 place) q
      l P.:/ r -> unionsIn (P.operand 0 place) l <> unionsIn (P.operand 1 place) r
      l P.:& r -> unionsIn (P.operand 0 place) l <> unionsIn (P.operand 1 place) r
      _ -> []
    -- the path with each union for which the test holds (of its place and
    -- the union) replaced by one of its sides, in every way
    sidesChosen :: (P.Place -> P.Path -> Bool) -> P.Place -> P.Path -> [P.Path]
    sidesChosen chosen place = \case
      u@(P.Union _ l r)
        | chosen place u -> sidesChosen chosen (P.operand 0 place) l <> sidesChosen chosen (P.operand 1 place) r
      P.Union k l r -> P.Union k <$> sidesChosen chosen (P.operand 0 place) l <*> sidesChosen chosen (P.operand 1 place) r
      l P.:/ r -> (P.:/) <$> sidesChosen chosen (P.operand 0 place) l <*> sidesChosen chosen (P.operand 1 place) r
      l P.:& r -> (P.:&) <$> sidesChosen chosen (P.operand 0 place) l <*> sidesChosen chosen (P.operand 1 place) r
      P.Backwards q -> P.Backwards <$> sidesChosen chosen (P.operand 0 place) q
      P.WithLimits a b q -> P.WithLimits a b <$> sidesChosen chosen (P.operand 0 place) q
      q -> [q]

-- * The order of operations

-- | One thing a construction does: place an unnamed node, by all the
-- steps into it at once, or connect two nodes that are known (written, or
-- placed before).
data Operation = Operation
  { -- | the vertex the steps go into
    target :: PVertex,
    steps :: [EdgeId],
    -- | whether both ends are known, so that no node is chosen
    connects :: Bool,
    -- | SAME PACE: how far the steps are from the start of their routes,
    -- the furthest counting (of the routes that share a step, and of
    -- the steps into one node)
    distance :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | The operations of an alternative and the order they are done in.
data Plan = Plan
  { operations :: [Operation],
    -- | how far each step is from the start of its route (the furthest,
    -- for a step that several routes share), from 1
    distances :: Map EdgeId Int,
    -- | The operations in the order they are done: those of one batch
    -- are done together, all looking at the graph and at what the
    -- batches before created. An operation is in a later batch than
    -- every operation it must come after: the steps before its own on
    -- their routes, the placing of the node it starts from, and every
    -- possible supplier.
    batches :: [[Operation]],
    -- | The groups of operations that must each come after another of
    -- the group (C): with any, the alternative is refused.
    circles :: [[Operation]]
  }
  deriving stock (Eq, Show, Generic)

-- | The plan of an alternative, from its routes alone; the current node
-- is the one it is built from, and counts as written.
--
-- A SUPPLIER of an operation is another operation that could create an
-- edge the first might follow: one of its steps has a literal label that
-- a step of the first accepts, is no further from its route's start than
-- the first (same pace), and nothing written rules out that its edge
-- starts where the first's step stands and ends where the first may go.
-- An operation that itself needs the first to have been done (it is
-- further along its routes, or starts from the node it places) is no
-- supplier of it. A connection with a literal label follows nothing, so
-- it has no suppliers.
--
-- Operations that are the same ordinary request (one step each, into an
-- unnamed node, with the same literal label, direction, test and
-- distance, and none further along a route than another) may be each
-- other's suppliers: they are done together. Every other circle of
-- "comes after" is a circle of the plan.
--
-- With m steps and L hops over all routes this takes at most
-- O(m (m + L)) time and O(m²) space: every step is compared with the
-- steps whose label it accepts, at most all of them, and what an
-- operation comes after along the routes is followed for each such pair.
planOf :: NID -> PatternPath -> Plan
planOf cur p = Plan {operations, distances, batches, circles}
  where
    specs = patternEdges p
    order = edgeOrder p
    routes = toList p.routes
    specOf e = fromMaybe (error "mk: no such step") (lookup e specs)
    written = \case
      Current -> Just cur
      Fixed n -> Just n
      Var _ -> Nothing
    literal e = case (specOf e).matcher of
      MLiteral l -> Just l
      _ -> Nothing

    distances :: Map EdgeId Int
    distances =
      Map.fromListWith
        max
        [(h.edge, i) | r <- routes, (i, h) <- zip [1 :: Int ..] r.hops]
    distanceOf e = findWithDefault 0 e distances
    -- the steps before each step on the routes it is on
    previous :: Map EdgeId (Set EdgeId)
    previous =
      Map.fromListWith
        (<>)
        [ (h.edge, setFromList (map (.edge) before))
        | r <- routes,
          (before, h) <- zip (inits r.hops) r.hops
        ]
    comesAfter e h = h `member` findWithDefault mempty e previous
    -- The steps into each variable that place it: all of them, but for a
    -- step that comes after another of them on a route (that one comes
    -- back to the node, which R refuses).
    placing :: Map VarId [EdgeId]
    placing =
      map firsts $
        Map.fromListWith
          (flip (<>))
          [(v, [e]) | e <- order, Var v <- [(specOf e).target]]
      where
        firsts es =
          case [e | e <- es, not (any (\h -> h /= e && e `comesAfter` h) es)] of
            [] -> es
            first' -> first'

    operations :: [Operation]
    operations =
      [ Operation (Var v) es False (foldl' max 0 (map distanceOf es))
      | v <- patternVars p,
        Just es <- [lookup v placing]
      ]
        <> [ Operation (specOf e).target [e] True (distanceOf e)
           | e <- order,
             e `notMember` placed
           ]
      where
        placed = setFromList (concat (toList placing)) :: Set EdgeId
    numbered = zip [0 :: Int ..] operations
    opOf :: Map Int Operation
    opOf = mapFromList numbered
    owner :: Map EdgeId Int
    owner = mapFromList [(e, i) | (i, op) <- numbered, e <- op.steps]
    ownerOf e = fromMaybe (error "mk: a step of no operation") (lookup e owner)

    -- What an operation comes after for the routes alone: the steps
    -- before its own, and the placing of the node a step starts from.
    base :: Map Int (Set Int)
    base =
      Map.fromListWith (<>) $
        [(i, mempty) | (i, _) <- numbered]
          <> [ (b, singletonSet a)
             | r <- routes,
               (before, after) <- zip r.hops (drop 1 r.hops),
               let a = ownerOf before.edge,
               let b = ownerOf after.edge,
               a /= b
             ]
          <> [ (i, singletonSet placer)
             | (i, op) <- numbered,
               e <- op.steps,
               Var v <- [(specOf e).source],
               first' : _ <- [findWithDefault [] v placing],
               let placer = ownerOf first',
               placer /= i
             ]
    baseOf i = findWithDefault mempty i base
    ancestors :: LazyMap.Map Int (Set Int)
    ancestors =
      LazyMap.fromList
        [(i, closure mempty (toList (baseOf i))) | (i, _) <- numbered]
      where
        closure seen [] = seen
        closure seen (j : rest)
          | j `member` seen = closure seen rest
          | otherwise = closure (insertSet j seen) (toList (baseOf j) <> rest)
    ancestorsOf i = LazyMap.findWithDefault mempty i ancestors

    accepts :: PVertex -> NID -> Bool
    accepts v n = maybe True (== n) (written v) && testAccepts (vertexTest p v) n
    -- the steps with each literal label, and all of them
    literalSteps :: [(Text, EdgeId)]
    literalSteps = [(l, e) | e <- order, Just l <- [literal e]]
    byLabel :: Map Text [EdgeId]
    byLabel = Map.fromListWith (flip (<>)) [(l, [e]) | (l, e) <- literalSteps]
    -- The operations that a step of this one might follow an edge of:
    -- only steps with a label that its own step accepts are looked at,
    -- so a path that writes no label twice takes time in its length.
    suppliersOf :: Operation -> Set Int
    suppliersOf receiver =
      setFromList
        [ ownerOf ce
        | r <- map specOf receiver.steps,
          ce <- case r.matcher of
            MLiteral l -> findWithDefault [] l byLabel
            m -> [e | (l, e) <- literalSteps, matcherAccepts m l],
          distanceOf ce <= receiver.distance,
          let c = specOf ce,
          let (near, far) =
                if c.direction == r.direction
                  then (c.source, c.target)
                  else (c.target, c.source),
          and [x == y | Just x <- [written r.source], Just y <- [written near]],
          all (accepts receiver.target) (written far)
        ]
    arcs :: Map Int (Set Int)
    arcs =
      Map.fromListWith (<>) $
        mapToList base
          <> [ (i, singletonSet j)
             | (i, receiver) <- numbered,
               not (receiver.connects && all (isJust . literal) receiver.steps),
               j <- toList (suppliersOf receiver),
               i /= j,
               i `notMember` ancestorsOf j
             ]
    arcsOf i = findWithDefault mempty i arcs

    -- the same ordinary request
    sameRequest :: Operation -> Maybe (Direction, Text, NodeTest, Int)
    sameRequest op = case op.steps of
      [e]
        | not op.connects,
          Just l <- literal e ->
            Just ((specOf e).direction, l, vertexTest p op.target, op.distance)
      _ -> Nothing
    groups :: [[Int]]
    groups =
      map flattenSCC (stronglyConnComp [(i, i, toList (arcsOf i)) | (i, _) <- numbered])
    isCircle = \case
      [i] -> i `member` arcsOf i
      group' ->
        let members = setFromList group' :: Set Int
            requests = map (sameRequest . operation) group'
         in case requests of
              Just request : rest ->
                any (/= Just request) rest
                  || any (\i -> not (null (ancestorsOf i `intersection` members))) group'
              _ -> True
    operation i = fromMaybe (error "mk: no such operation") (lookup i opOf)
    circles = [map operation (sort group') | group' <- groups, isCircle group']

    -- Each group is in the batch after the last batch of a group it
    -- comes after.
    groupOf :: Map Int Int
    groupOf = mapFromList [(i, g) | (g, group') <- zip [0 ..] groups, i <- group']
    level :: LazyMap.Map Int Int
    level =
      LazyMap.fromList
        [ ( g,
            foldl' max 0 $
              [ 1 + LazyMap.findWithDefault 0 g' level
              | i <- group',
                j <- toList (arcsOf i),
                Just g' <- [lookup j groupOf],
                g' /= g
              ]
          )
        | (g, group') <- zip [0 :: Int ..] groups
        ]
    batches :: [[Operation]]
    batches =
      map (map operation . sort) . toList $
        Map.fromListWith
          (<>)
          [ (LazyMap.findWithDefault 0 g level, group')
          | (g, group') <- zip [0 :: Int ..] groups
          ]

-- * Refusals

-- | Why the alternatives of a path are not built: R, M, V and C of each
-- alternative, in that order. None: every alternative is built. R takes
-- one pass over the hops of the routes (with a map of the vertices
-- visited), M one over the operations and V one over the steps; C is the
-- plan ('planOf'), which costs the most.
refusals :: NID -> PathQuery -> [Refusal]
refusals cur q = concatMap (alternativeRefusals cur) (toList q.alternatives)

alternativeRefusals :: NID -> PatternPath -> [Refusal]
alternativeRefusals cur p = returning <> wildcardJoins <> severalWildcards <> circular
  where
    specs = patternEdges p
    order = edgeOrder p
    plan = planOf cur p
    step e = Step (stepPath p e) e
    wild e = case lookup e specs of
      Just spec -> case spec.matcher of
        MLiteral _ -> False
        _ -> True
      Nothing -> False
    -- R: the first return of the first route that has one. A route is
    -- gone along once, with the vertices it has visited.
    returning =
      take
        1
        [ ReturnsToMeetingPoint (prefixPath p r before) (prefixPath p r (j + 1))
        | r <- toList p.routes,
          (j, before) <- take 1 (returns (singletonMap r.root 0) (zip [0 ..] r.hops))
        ]
    -- the hops that go into a variable visited before, each with how
    -- many hops the route had taken when it first stood there
    returns :: Map PVertex Int -> [(Int, Hop)] -> [(Int, Int)]
    returns _ [] = []
    returns visited ((j, h) : rest) = case (h.target, lookup h.target visited) of
      (Var _, Just before) -> (j, before) : returns visited rest
      _ -> returns (Map.insertWith (\_ old -> old) h.target (j + 1) visited) rest
    -- M: a step that is no literal label among the steps placing a node
    wildcardJoins =
      [ WildcardJoin p (step e) [step e' | e' <- op.steps, e' /= e]
      | op <- plan.operations,
        not op.connects,
        length op.steps > 1,
        e <- take 1 (filter wild op.steps)
      ]
    -- V
    severalWildcards =
      [ SeveralWildcards p (map step es)
      | let es = filter wild order,
        length es > 1
      ]
    -- C
    circular =
      [ CircularSuppliers p [Request (map step op.steps) | op <- circle]
      | circle <- plan.circles
      ]

-- | The first route with a step, up to that step.
stepPath :: PatternPath -> EdgeId -> P.Path
stepPath p e =
  fromMaybe P.One . headMay $
    [ prefixPath p r (i + 1)
    | r <- toList p.routes,
      (i, h) <- zip [0 ..] r.hops,
      h.edge == e
    ]

-- | The first hops of a route as a path: each step, with what is written
-- of the node it goes to (@\@N@, or @!{…}@).
prefixPath :: PatternPath -> Route -> Int -> P.Path
prefixPath p r n = case rootPart <> concatMap hopParts (take n r.hops) of
  [] -> P.One
  part : parts -> foldl' (P.:/) part parts
  where
    rootPart = case r.root of
      Fixed m -> [P.Absolute m]
      _ -> []
    hopParts h = (case h.direction of Out -> step h; In -> P.Backwards (step h)) : nodePart h.target
    step h = case h.matcher of
      MLiteral l -> P.Literal l
      MWild -> P.Wild
      MRegex regex -> P.RegexMatch regex
    nodePart v = case (v, vertexTest p v) of
      (Fixed m, _) -> [P.Absolute m]
      (_, Exactly m) -> [P.Absolute m]
      (_, Except ns) | not (null ns) -> [P.ExcludingNIDs ns]
      _ -> []
