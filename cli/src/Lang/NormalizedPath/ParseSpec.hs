module Lang.NormalizedPath.ParseSpec where

import Lang.NormalizedPath.Parse
import Lang.Parsing (transition)
import Lang.ParsingSpec
import Models.NID
import Models.NormalizedPath
import MyPrelude hiding (union)
import TestPrelude hiding (union)

test_pNormalizedPath :: TestTree
test_pNormalizedPath =
  testGroup
    "pNormalizedPath"
    [ -- Literal characters and wildcards
      "[a]" `parsesTo` singletonBranch (DPOutgoing (DPLiteral "a")),
      "[*]" `parsesTo` singletonBranch (DPOutgoing DPWild),
      "[~a]" `parsesTo` singletonBranch (DPIncoming (DPLiteral "a")),
      "[~*]" `parsesTo` singletonBranch (DPIncoming DPWild),
      -- PointlikeDeterministicPath
      "@" `parsesTo` singletonPointlike (PointlikeDeterministicPath JoinPoint mempty),
      "@1" `parsesTo` singletonPointlike (PointlikeDeterministicPath (Specific (smallNID 1)) mempty),
      "@[a]" `parsesTo` singletonPointlike (PointlikeDeterministicPath JoinPoint (singletonSet (DPOutgoing (DPLiteral "a")))),
      "@[~a]" `parsesTo` singletonPointlike (PointlikeDeterministicPath JoinPoint (singletonSet (DPIncoming (DPLiteral "a")))),
      "@[a & *]" `parsesTo` singletonPointlike (PointlikeDeterministicPath JoinPoint (setFromList [DPOutgoing (DPLiteral "a"), DPOutgoing DPWild])),
      "@[a & ~*]" `parsesTo` singletonPointlike (PointlikeDeterministicPath JoinPoint (setFromList [DPOutgoing (DPLiteral "a"), DPIncoming DPWild])),
      "@1[a]" `parsesTo` singletonPointlike (PointlikeDeterministicPath (Specific (smallNID 1)) (singletonSet (DPOutgoing (DPLiteral "a")))),
      "@42[a & b]" `parsesTo` singletonPointlike (PointlikeDeterministicPath (Specific (smallNID 42)) (setFromList [DPOutgoing (DPLiteral "a"), DPOutgoing (DPLiteral "b")])),
      -- sequences
      "[a/|b]" `parsesTo` singletonBranch (DPSequence (setFromList [DPOutgoing (DPLiteral "a")]) unanchored (setFromList [DPOutgoing (DPLiteral "b")])),
      "[a/|~b]" `parsesTo` singletonBranch (DPSequence (setFromList [DPOutgoing (DPLiteral "a")]) unanchored (setFromList [DPIncoming (DPLiteral "b")])),
      "[a/@|b]" `parsesTo` singletonBranch (DPSequence (setFromList [DPOutgoing (DPLiteral "a")]) joinPoint (setFromList [DPOutgoing (DPLiteral "b")])),
      "[~a/@|b]" `parsesTo` singletonBranch (DPSequence (setFromList [DPIncoming (DPLiteral "a")]) joinPoint (setFromList [DPOutgoing (DPLiteral "b")])),
      "[(a & b)/@|c]" `parsesTo` singletonBranch (DPSequence (setFromList [DPOutgoing (DPLiteral "a"), DPOutgoing (DPLiteral "b")]) joinPoint (setFromList [DPOutgoing (DPLiteral "c")])),
      "[a/@|(b & c)]" `parsesTo` singletonBranch (DPSequence (setFromList [DPOutgoing (DPLiteral "a")]) joinPoint (setFromList [DPOutgoing (DPLiteral "b"), DPOutgoing (DPLiteral "c")])),
      "[a/|b]" `parsesTo` singletonBranch (DPSequence (setFromList [DPOutgoing (DPLiteral "a")]) unanchored (setFromList [DPOutgoing (DPLiteral "b")])),
      "[a/|(b/|c)]" `parsesTo` singletonBranch (DPSequence (setFromList [DPOutgoing (DPLiteral "a")]) unanchored (setFromList [DPSequence (setFromList [DPOutgoing (DPLiteral "b")]) unanchored (setFromList [DPOutgoing (DPLiteral "c")])])),
      "[(b/|c)/|a]" `parsesTo` singletonBranch (DPSequence (setFromList [DPSequence (setFromList [DPOutgoing (DPLiteral "b")]) unanchored (setFromList [DPOutgoing (DPLiteral "c")])]) unanchored (setFromList [DPOutgoing (DPLiteral "a")])),
      "[a/@1|b]" `parsesTo` singletonBranch (DPSequence (setFromList [DPOutgoing (DPLiteral "a")]) (specific (smallNID 1)) (setFromList [DPOutgoing (DPLiteral "b")])),
      "[a/@1[a]|b]" `parsesTo` singletonBranch (DPSequence (setFromList [DPOutgoing (DPLiteral "a")]) (PointlikeDeterministicPath (Specific (smallNID 1)) (singletonSet (DPOutgoing (DPLiteral "a")))) (setFromList [DPOutgoing (DPLiteral "b")])),
      "[a/@1[~a]|b]" `parsesTo` singletonBranch (DPSequence (setFromList [DPOutgoing (DPLiteral "a")]) (PointlikeDeterministicPath (Specific (smallNID 1)) (singletonSet (DPIncoming (DPLiteral "a")))) (setFromList [DPOutgoing (DPLiteral "b")])),
      -- RootedDeterministicPath
      "[@<a & @1<b & c]>@"
        `parsesTo` singletonRooted
          ( RootedDeterministicPath
              ( mapFromList
                  [ (Pointlike joinPoint, singletonSet (DPOutgoing (DPLiteral "a"))),
                    (Pointlike $ specific (smallNID 1), singletonSet (DPOutgoing (DPLiteral "b"))),
                    (Pointlike unanchored, singletonSet (DPOutgoing (DPLiteral "c")))
                  ]
              )
              joinPoint
          ),
      "[@[*]<a & b]>@"
        `parsesTo` singletonRooted
          ( RootedDeterministicPath
              ( mapFromList
                  [ (Pointlike $ PointlikeDeterministicPath JoinPoint (singletonSet (DPOutgoing DPWild)), singletonSet (DPOutgoing (DPLiteral "a"))),
                    (Pointlike unanchored, singletonSet (DPOutgoing (DPLiteral "b")))
                  ]
              )
              joinPoint
          ),
      "[a & b]" `parsesTo` singletonRooted (RootedDeterministicPath (singletonMap (Pointlike unanchored) (setFromList [DPOutgoing (DPLiteral "a"), DPOutgoing (DPLiteral "b")])) unanchored),
      "[@[*]<a & b]>@"
        `parsesTo` singletonRooted
          ( RootedDeterministicPath
              ( mapFromList
                  [ (Pointlike $ PointlikeDeterministicPath JoinPoint (singletonSet (DPOutgoing DPWild)), singletonSet (DPOutgoing (DPLiteral "a"))),
                    (Pointlike unanchored, singletonSet (DPOutgoing (DPLiteral "b")))
                  ]
              )
              joinPoint
          ),
      "[@<a]"
        `parsesTo` singletonRooted
          ( RootedDeterministicPath
              (singletonMap (Pointlike joinPoint) (singletonSet (DPOutgoing (DPLiteral "a"))))
              unanchored
          ),
      "[a] + [b]" `parsesTo` branches [DPOutgoing (DPLiteral "a"), DPOutgoing (DPLiteral "b")],
      "[a] + [b] + [c]" `parsesTo` branches [DPOutgoing (DPLiteral "a"), DPOutgoing (DPLiteral "b"), DPOutgoing (DPLiteral "c")],
      "!" `parsesTo` NormalizedPath mempty,
      "@ + [a]"
        `parsesTo` NormalizedPath
          ( setFromList
              [ Rooted
                  ( RootedDeterministicPath
                      (singletonMap (Pointlike unanchored) (singletonSet (DPOutgoing (DPLiteral "a"))))
                      unanchored
                  ),
                Pointlike joinPoint
              ]
          ),
      -- Complex combinations
      "[a] + @1[b]"
        `parsesTo` NormalizedPath
          ( setFromList
              [ Rooted
                  ( RootedDeterministicPath
                      (singletonMap (Pointlike unanchored) (singletonSet (DPOutgoing (DPLiteral "a"))))
                      unanchored
                  ),
                Pointlike (PointlikeDeterministicPath (Specific (smallNID 1)) (singletonSet (DPOutgoing (DPLiteral "b"))))
              ]
          ),
      -- Parse failures
      parseFails "@[",
      parseFails "@a",
      parseFails "@1[a",
      parseFails "a/|",
      parseFails "/|b",
      parseFails "[@<a>",
      parseFails "a + + b",
      parseFails "~",
      parseFails "~@",
      -- we require [] around all RootedDeterministicPaths
      parseFails "a + b",
      parseFails "a"
    ]
  where
    parsesTo :: String -> NormalizedPath Anchor String -> TestTree
    parsesTo input expected =
      testCase ("parse: " ++ show input) $
        testParserParses (pNormalizedPath transition <* eof) (unpack input) expected

    parseFails input =
      testCase ("parse fails: " ++ show input) $
        testParserFails (pNormalizedPath transition <* eof) input

singletonBranch :: DPBranch Anchor String -> NormalizedPath Anchor String
singletonBranch branch =
  NormalizedPath . singletonSet . Rooted $
    RootedDeterministicPath
      (singletonMap (Pointlike unanchored) (singletonSet branch))
      unanchored

branches :: [DPBranch Anchor String] -> NormalizedPath Anchor String
branches bs =
  NormalizedPath . setFromList $
    [ Rooted (RootedDeterministicPath (singletonMap (Pointlike unanchored) (singletonSet b)) unanchored)
      | b <- bs
    ]

singletonPointlike :: PointlikeDeterministicPath Anchor String -> NormalizedPath Anchor String
singletonPointlike = NormalizedPath . singletonSet . Pointlike

singletonRooted :: RootedDeterministicPath Anchor String -> NormalizedPath Anchor String
singletonRooted = NormalizedPath . singletonSet . Rooted
