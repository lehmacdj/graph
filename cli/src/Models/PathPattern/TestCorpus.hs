-- | Surface queries and small graphs used to compare the compiler and
-- matcher against independent oracles. Ported from the query corpus of the
-- @codex/path-normal-form@ experiment, without @%float@/@%unanchored@.
module Models.PathPattern.TestCorpus
  ( corpusQueries,
    corpusEquivalences,
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
    ([path| a + a/@ |], [path| a |]),
    -- an @ alternative at a sequence boundary joins, like a lone @
    ( [path| (@ + a)/@000000000002 |],
      [path| @/@000000000002 + a/@000000000002 |]
    ),
    ( [path| (a + (b + @))/@000000000001 |],
      [path| a/@000000000001 + b/@000000000001 + @/@000000000001 |]
    ),
    ( [path| (@ + b)/(%never + @000000000002) |],
      [path| @/@000000000002 + b/@000000000002 |]
    ),
    ([path| (a & b)/(@ + a) |], [path| (a & b)/@ + (a & b)/a |]),
    -- ...so a later continuation shares the prefix in the @ alternative
    ( [path| a/(@ + b)/(a & b) |],
      [path| a/@/(a & b) + a/b/(a & b) |]
    ),
    ([path| ~(a/b) |], [path| ~b/~a |])
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
         [path| a + b/a |],
         P.RegexMatch [re|a"b|]
           P.:/ P.One
           P.:/ (P.Backwards P.Wild P.:& P.RegexMatch [re|^b$|]),
         P.RegexMatch [re|^a$|] P.:/ P.Wild
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
