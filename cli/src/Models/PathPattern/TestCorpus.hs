-- | Surface queries and small graphs used to compare the compiler and
-- matcher against independent oracles. Ported from the query corpus of the
-- @codex/path-normal-form@ experiment, without @%float@/@%unanchored@.
module Models.PathPattern.TestCorpus
  ( corpusQueries,
    corpusCompileQueries,
    corpusEquivalences,
    corpusLargeEquivalences,
    corpusGraphs,
    corpusCurrent,
    corpusNode,
    fewAlternatives,
  )
where

import Models.Edge (Edge' (..))
import Models.Graph (Graph, emptyGraph, insertEdges, insertNode, maybeNodeLookup)
import Models.NID
import Models.Node (Node, emptyNode)
import Models.Path.Simple qualified as P
import Models.Path.TH (path)
import Models.PathPattern (PatternPath)
import Models.PathPattern.Compile (Item, alternativeRoutes)
import MyPrelude

-- | Whether a path is small enough for a test to compile it and look at
-- all of what it compiles to. The tests on random paths leave out the
-- paths that are not: nothing else limits how large a path may be
-- (docs/path-cost-limits.md), and a random path can take hours to
-- compile.
--
-- Small enough is: by a rough count read off the path, its routes choose
-- the sides of its unions in at most @4 * n@ ways ('estimatedWays'); and
-- it compiles to at most @n@ alternatives, found among at most @4 * n@
-- ways to choose (an alternative is listed once for every way to choose
-- that comes to it; they are listed lazily, and a path with more is given
-- up when one too many is listed).
--
-- Neither part bounds the time compiling takes. The first is there
-- because the second alone was held up for over two seconds, or for good,
-- by 28 of 3,700 paths of three generators of
-- "Models.PathPattern.CompileSpec" (with @n@ 200). With it the slowest of
-- them takes 0.6 s, and 49 of the 3,377 that compile quickly are left
-- out. No clock is read, so a test looks at the same paths every time.
fewAlternatives :: Int -> P.Path -> Bool
fewAlternatives n p =
  estimatedWays p <= fromIntegral (4 * n)
    && go mempty (0 :: Int) (alternativeRoutes p)
  where
    go :: Set PatternPath -> Int -> [([[Item Int]], PatternPath)] -> Bool
    go seen listed = \case
      [] -> True
      (_, pattern') : rest ->
        let seen' = insertSet pattern' seen
         in listed < 4 * n && length seen' <= n && go seen' (listed + 1) rest

-- | A rough count of the ways the routes of a path can choose the sides of
-- its unions, read off the path as it is written, without compiling it.
--
-- A @|@ union of @k@ sides that @m@ routes go through counts @k ^ m@, and
-- a @++@ union counts once for each side. The routes through a part of a
-- chain are those through the chain, times the routes of the other parts
-- of its stretch (the parts between two joins of the chain): a group has
-- the routes of its conjuncts together, and a union those of the side
-- that has most.
--
-- This is for 'fewAlternatives' and nothing else. It is mostly far too
-- high, since it does not know that routes through one copy of a union
-- choose alike or that choices contradict each other; and it is not known
-- to be an upper bound.
estimatedWays :: P.Path -> Double
estimatedWays = ways 1
  where
    ways :: Double -> P.Path -> Double
    ways m p =
      product
        [ part (m * product [routes e' | (j, e') <- zip [0 :: Int ..] stretch, j /= i]) e
        | stretch <- stretches (parts p),
          (i, e) <- zip [0 ..] stretch
        ]
    part m = \case
      e@(_ P.:& _) -> product (map (ways m) (conjuncts e))
      e@(P.Union P.Relational _ _) ->
        let sides' = sides P.Relational e
         in fromIntegral (length sides') ** m * product (map (ways m) sides')
      e@(P.Union P.Shared _ _) -> sum (map (ways m) (sides P.Shared e))
      P.Backwards e -> part m e
      _ -> 1
    routes :: P.Path -> Double
    routes = \case
      l P.:/ r -> routes l * routes r
      l P.:& r -> routes l + routes r
      P.Union _ l r -> max (routes l) (routes r)
      P.Backwards e -> routes e
      _ -> 1
    -- the parts of a chain, and the chain cut at its joins
    parts = \case
      l P.:/ r -> parts l <> parts r
      P.Backwards e -> parts e
      e -> [e]
    stretches xs = case break isJoin xs of
      (stretch, []) -> [stretch]
      (stretch, _ : rest) -> stretch : stretches rest
    isJoin = \case
      P.One -> True
      P.Absolute _ -> True
      P.ExcludingNIDs _ -> True
      _ -> False
    conjuncts = \case
      l P.:& r -> conjuncts l <> conjuncts r
      e -> [e]
    sides kind = \case
      P.Union kind' l r | kind' == kind -> sides kind l <> sides kind r
      e -> [e]

-- | Pairs of paths that must compile to the same canonical pattern.
corpusEquivalences :: [(P.Path, P.Path)]
corpusEquivalences =
  [ ([path| a/@/b/@ |], [path| a/b/@ |]),
    ([path| ((a & b)/@ & @)/@ |], [path| (a & b & @)/@ |]),
    ([path| (@ & (a & b)/@)/@ |], [path| (@ & a & b)/@ |]),
    ([path| ((@ & a)/@ & b)/@ |], [path| (@ & a & b)/@ |]),
    ([path| (@ & (@ & a)/@)/@ |], [path| (@ & @ & a)/@ |]),
    ([path| a/@/(b & (c & d)/@)/@ |], [path| a/@/(b & c & d)/@ |]),
    ([path| @/@/(a & b) |], [path| a & b |]),
    ([path| a/@/(@/@/b & @/@/a) |], [path| a/@/(b & a) |]),
    ([path| a/@/@ |], [path| a/@ |]),
    ([path| a/@/((b & c)/@) |], [path| a/@/(b & c)/@ |]),
    ( [path| (a/@000000000001 & b)/@ |],
      [path| (a & b/@000000000001)/@ |]
    ),
    ([path| a ++ a/@ |], [path| a |]),
    -- an @ alternative at a sequence boundary joins, like a lone @
    ( [path| (@ ++ a)/@000000000002 |],
      [path| @/@000000000002 ++ a/@000000000002 |]
    ),
    ( [path| (a ++ (b ++ @))/@000000000001 |],
      [path| a/@000000000001 ++ b/@000000000001 ++ @/@000000000001 |]
    ),
    ( [path| (@ ++ b)/(%never ++ @000000000002) |],
      [path| @/@000000000002 ++ b/@000000000002 |]
    ),
    ([path| (a & b)/(@ ++ a) |], [path| (a & b)/@ ++ (a & b)/a |]),
    -- ...so a later continuation shares the prefix in the @ alternative
    ( [path| a/(@ ++ b)/(a & b) |],
      [path| a/@/(a & b) ++ a/b/(a & b) |]
    ),
    ([path| ~(a/b) |], [path| ~b/~a |]),
    -- each copy of an unjoined prefix or suffix chooses its own side
    ( [path| (a | b)/(a & b) |],
      [path| (a | b)/a & (a | b)/b |]
    ),
    ( [path| (a & b)/(a | b) |],
      [path| a/(a | b) & b/(a | b) |]
    ),
    ( [path| (a | b)/(a & b) |],
      [path| a/a & a/b | a/a & b/b | b/a & a/b | b/a & b/b |]
    ),
    -- ...but a joined prefix is one walk, so it chooses once
    ( [path| (a | b)/@/(a & b) |],
      [path| a/@/(a & b) | b/@/(a & b) |]
    ),
    ( [path| x/@/(a | b)/(a & b) |],
      [path| x/@/((a | b)/a & (a | b)/b) |]
    ),
    -- each copy chooses any side; the copies that choose a side with a
    -- join share one instance of it
    ( [path| (x/@ | a | b)/(a & b) |],
      [path| x/@/(a & b) | x/@/a & (a | b)/b | (a | b)/a & x/@/b
             | (a | b)/a & (a | b)/b |]
    ),
    ( [path| (a & b)/(@ | a | b) |],
      [path| (a & b)/@ | a/@ & b/(a | b) | a/(a | b) & b/@
             | a/(a | b) & b/(a | b) |]
    ),
    ( [path| (a & b)/(@ | (a | b)/@) |],
      [path| (a & b)/@ | (a/(a | b) & b/(a | b))/@ | a/@ & b/(a | b)/@
             | a/(a | b)/@ & b/@ |]
    ),
    ( [path| ((a & b)/@ | a)/(a & b) |],
      [path| (a & b)/@/(a & b) | (a & b)/@/a & a/b | a/a & (a & b)/@/b
             | a/a & a/b |]
    ),
    -- a side written twice is two sides: each copy chooses one, and
    -- the copies that choose different ones share no walk
    ( [path| (a/@ | a/@)/(a & b) |],
      [path| a/@/(a & b) | a/@/a & a/@/b |]
    ),
    -- a %never side is no side, and a union with one side is that side
    ([path| (a/@ | %never)/(a & b) |], [path| a/@/(a & b) |]),
    -- on single labels, | is a regex alternation
    ([path| a | b |], [path| a ++ b |]),
    -- a shared union is chosen once for the whole path
    ( [path| (a ++ b)/(a & b) |],
      [path| a/(a & b) ++ b/(a & b) |]
    ),
    ( [path| (a & b)/(a ++ b) |],
      [path| (a & b)/a ++ (a & b)/b |]
    ),
    ( [path| x/(a ++ b)/(a & b) |],
      [path| x/a/(a & b) ++ x/b/(a & b) |]
    )
  ]

-- | Like 'corpusEquivalences', with more routes than matching and
-- residuation can check by brute force quickly.
corpusLargeEquivalences :: [(P.Path, P.Path)]
corpusLargeEquivalences =
  [ ( [path| x/(a | b)/(a & b) |],
      [path| x/(a | b)/a & x/(a | b)/b |]
    ),
    -- a shared union is chosen once also inside a relational one, or
    -- around one
    ( [path| ((a ++ b) | x)/(a & b) |],
      [path| (a | x)/(a & b) ++ (b | x)/(a & b) |]
    ),
    ( [path| ((a | b) ++ x)/(a & b) |],
      [path| (a | b)/(a & b) ++ x/(a & b) |]
    ),
    ( [path| ((a | b) & a)/(a & b) |],
      [path| (a | b)/a & (a | b)/b & a/a & a/b |]
    ),
    ( [path| ((a & b) | a)/((a & b) | b) |],
      -- the prefix chooses and each copy of the suffix chooses, or the
      -- suffix chooses and each copy of the prefix chooses; an
      -- alternative is its routes, listed by the conjuncts they go
      -- through, whichever union chose first
      [path| (a & b)/(a & b) | (a & b)/b
             | a/(a & b) | a/b | a/(a & b) & b/b | a/b & b/(a & b)
             | a/a & a/b & b/a |]
    )
  ]

-- | 'corpusQueries', and queries with more routes, for compilation only.
corpusCompileQueries :: [P.Path]
corpusCompileQueries =
  corpusQueries
    ++ concatMap (\(a, b) -> [a, b]) corpusLargeEquivalences
    ++ [ [path| (a & b)/((@ | a)/(a & b)) |],
         [path| (a | b)/(a | b)/(a & b) |]
       ]

corpusQueries :: [P.Path]
corpusQueries =
  concatMap (\(a, b) -> [a, b]) corpusEquivalences
    ++ [ [path| a/(a & b) |],
         [path| a/@/(a & b) |],
         [path| (a & b)/@ & a |],
         [path| a/@/(a & b)/@ |],
         [path| (@ & a)/@ |],
         [path| (a & @)/@ |],
         [path| a/(@ & b)/@ |],
         [path| a/(b & @)/@ |],
         [path| a/@000000000002/(a & b) |],
         [path| @000000000002/~a |],
         [path| a/@/(@ & b) |],
         [path| a & a/@ |],
         [path| a & a |],
         [path| (a & a)/@ |],
         [path| * & * |],
         [path| (* & *)/@ |],
         [path| */@/(a & b) |],
         [path| */(a & b) |],
         [path| a/@/(b/@/(a & b) & b) |],
         [path| (a & b)/@/a/@/(a & b) |],
         [path| (a/@000000000001 & b/@000000000002)/@ |],
         [path| !{@000000000001}/(@ & a)/@ |],
         [path| a/!{@000000000002}/(@ & b)/@ |],
         [path| (a & (@ & b)/@)/@ |],
         [path| (a & (b/@/(@ & a)/@))/@ |],
         [path| a/@/(@ & b)/@ |],
         [path| (a & ((@ & b)/@ & a)/@)/@ |],
         [path| a/@/(b & (@ & a)/@)/@ |],
         [path| a/@/((@ & b)/@/a & b)/@ |],
         [path| a/@/(* & ~a) |],
         [path| ~(a/@/(a & b)) |],
         [path| @000000000002 & a |],
         [path| (a/b & a)/@ |],
         [path| (a & b)/(a & b) |],
         [path| a ++ b/a |],
         P.RegexMatch [re|a"b|]
           P.:/ P.One
           P.:/ (P.Backwards P.Wild P.:& P.RegexMatch [re|^b$|]),
         P.RegexMatch [re|^a$|] P.:/ P.Wild,
         [path| (a | ~b)/(a & (b | @)) |],
         [path| (a | a/@000000000002)/(a & b) |],
         [path| */(a & b) |],
         [path| (a & b)/(* | %never) |]
       ]

-- | Every graph over a subset of nodes 1 and 2 with edges labelled a or b.
corpusGraphs :: [Graph Text ()]
corpusGraphs =
  [ insertEdges edges (withNodes ns)
  | ns <- subsequences [1, 2 :: Int],
    edges <-
      subsequences
        [ Edge (smallNID s) t (smallNID d)
        | s <- ns,
          d <- ns,
          t <- ["a", "b"]
        ]
  ]
  where
    withNodes :: [Int] -> Graph Text ()
    withNodes =
      foldl' (\g n -> insertNode (emptyNode (smallNID n)) g) emptyGraph

-- | The nodes of a graph for a command at the corpus's current node, which
-- exists whether the graph has it or not: @mk@ creates no node that it is
-- told by its id, so it has to stand on one that exists.
corpusNode :: Graph Text () -> NID -> Maybe (Node Text ())
corpusNode g n = maybeNodeLookup n g <|> (emptyNode n <$ guard (n == corpusCurrent))

corpusCurrent :: NID
corpusCurrent = smallNID 1
