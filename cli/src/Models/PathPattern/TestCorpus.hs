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
  )
where

import Models.Edge (Edge' (..))
import Models.Graph (Graph, emptyGraph, insertEdges, insertNode)
import Models.NID
import Models.Node (emptyNode)
import Models.Path.Simple qualified as P
import Models.Path.TH (path)
import MyPrelude

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
    -- a union with one side, or a %never side, is that side
    ([path| (a/@ | a/@)/(a & b) |], [path| a/@/(a & b) |]),
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
      -- the prefix chooses first and each copy of the suffix chooses, or
      -- the suffix chooses first and each copy of the prefix chooses
      [path| (a & b)/(a & b) | (a & b)/a & (a & b)/b | (a & b)/b
             | a/(a & b) | a/b | a/(a & b) & b/b | a/b & b/(a & b)
             | (a & b)/a & a/b | a/a & (a & b)/b |]
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

corpusCurrent :: NID
corpusCurrent = smallNID 1
