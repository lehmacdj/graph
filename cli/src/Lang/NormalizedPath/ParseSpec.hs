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
      "a" `parsesTo` singletonBranch (DPLiteral "a"),
      "*" `parsesTo` singletonBranch DPWild,
      -- PointlikeDeterministicPath
      "@" `parsesTo` singletonPointlike (PointlikeDeterministicPath JoinPoint mempty),
      "@1" `parsesTo` singletonPointlike (PointlikeDeterministicPath (Specific (smallNID 1)) mempty),
      "@[a]" `parsesTo` singletonPointlike (PointlikeDeterministicPath JoinPoint (singletonSet (DPLiteral "a"))),
      "@[a & *]" `parsesTo` singletonPointlike (PointlikeDeterministicPath JoinPoint (setFromList [DPLiteral "a", DPWild])),
      "@1[a]" `parsesTo` singletonPointlike (PointlikeDeterministicPath (Specific (smallNID 1)) (singletonSet (DPLiteral "a"))),
      "@42[a & b]" `parsesTo` singletonPointlike (PointlikeDeterministicPath (Specific (smallNID 42)) (setFromList [DPLiteral "a", DPLiteral "b"])),
      -- sequences
      "[a/@|b]" `parsesTo` singletonBranch (DPSequence (setFromList [DPLiteral "a"]) joinPoint (setFromList [DPLiteral "b"])),
      "a/@|b" `parsesTo` singletonBranch (DPSequence (setFromList [DPLiteral "a"]) joinPoint (setFromList [DPLiteral "b"])),
      "(a & b)/@|c" `parsesTo` singletonBranch (DPSequence (setFromList [DPLiteral "a", DPLiteral "b"]) joinPoint (setFromList [DPLiteral "c"])),
      "a/@|(b & c)" `parsesTo` singletonBranch (DPSequence (setFromList [DPLiteral "a"]) joinPoint (setFromList [DPLiteral "b", DPLiteral "c"])),
      "a/|b" `parsesTo` singletonBranch (DPSequence (setFromList [DPLiteral "a"]) unanchored (setFromList [DPLiteral "b"])),
      "a/|(b/|c)" `parsesTo` singletonBranch (DPSequence (setFromList [DPLiteral "a"]) unanchored (setFromList [DPSequence (setFromList [DPLiteral "b"]) unanchored (setFromList [DPLiteral "c"])])),
      "(b/|c)/|a" `parsesTo` singletonBranch (DPSequence (setFromList [DPSequence (setFromList [DPLiteral "b"]) unanchored (setFromList [DPLiteral "c"])]) unanchored (setFromList [DPLiteral "a"])),
      "a/@1|b" `parsesTo` singletonBranch (DPSequence (setFromList [DPLiteral "a"]) (specific (smallNID 1)) (setFromList [DPLiteral "b"])),
      "a/@1[a]|b" `parsesTo` singletonBranch (DPSequence (setFromList [DPLiteral "a"]) (PointlikeDeterministicPath (Specific (smallNID 1)) (singletonSet (DPLiteral "a"))) (setFromList [DPLiteral "b"])),
      -- RootedDeterministicPath
      "[@<a & @1<b & c]>@"
        `parsesTo` singletonRooted
          ( RootedDeterministicPath
              ( mapFromList
                  [ (joinPoint, singletonSet (DPLiteral "a")),
                    (specific (smallNID 1), singletonSet (DPLiteral "b")),
                    (unanchored, singletonSet (DPLiteral "c"))
                  ]
              )
              joinPoint
          ),
      "[@[*]<a & b]>@"
        `parsesTo` singletonRooted
          ( RootedDeterministicPath
              ( mapFromList
                  [ (PointlikeDeterministicPath JoinPoint (singletonSet DPWild), singletonSet (DPLiteral "a")),
                    (unanchored, singletonSet (DPLiteral "b"))
                  ]
              )
              joinPoint
          ),
      "[a & b]" `parsesTo` singletonRooted (RootedDeterministicPath (singletonMap unanchored (setFromList [DPLiteral "a", DPLiteral "b"])) unanchored),
      "[@[*]<a & b]>@"
        `parsesTo` singletonRooted
          ( RootedDeterministicPath
              ( mapFromList
                  [ (PointlikeDeterministicPath JoinPoint (singletonSet DPWild), singletonSet (DPLiteral "a")),
                    (unanchored, singletonSet (DPLiteral "b"))
                  ]
              )
              joinPoint
          ),
      "[@<a]"
        `parsesTo` singletonRooted
          ( RootedDeterministicPath
              (singletonMap joinPoint (singletonSet (DPLiteral "a")))
              unanchored
          ),
      "a + b" `parsesTo` branches [DPLiteral "a", DPLiteral "b"],
      "a + b + c" `parsesTo` branches [DPLiteral "a", DPLiteral "b", DPLiteral "c"],
      "!" `parsesTo` NormalizedPath mempty,
      "@ + a"
        `parsesTo` NormalizedPath
          ( setFromList
              [ Rooted
                  ( RootedDeterministicPath
                      (singletonMap unanchored (singletonSet (DPLiteral "a")))
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
                      (singletonMap unanchored (singletonSet (DPLiteral "a")))
                      unanchored
                  ),
                Pointlike (PointlikeDeterministicPath (Specific (smallNID 1)) (singletonSet (DPLiteral "b")))
              ]
          ),
      -- Parse failures
      parseFails "@[",
      parseFails "@a",
      parseFails "@1[a",
      parseFails "a/|",
      parseFails "/|b",
      parseFails "[@<a>",
      parseFails "a + + b"
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
      (singletonMap unanchored (singletonSet branch))
      unanchored

branches :: [DPBranch Anchor String] -> NormalizedPath Anchor String
branches bs =
  NormalizedPath . setFromList $
    [ Rooted (RootedDeterministicPath (singletonMap unanchored (singletonSet b)) unanchored)
    | b <- bs
    ]

singletonPointlike :: PointlikeDeterministicPath Anchor String -> NormalizedPath Anchor String
singletonPointlike = NormalizedPath . singletonSet . Pointlike

singletonRooted :: RootedDeterministicPath Anchor String -> NormalizedPath Anchor String
singletonRooted = NormalizedPath . singletonSet . Rooted
