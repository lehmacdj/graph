module Lang.ParsingSpec where

import Data.Either (isLeft)
import Lang.Parsing
import TestPrelude
import Text.Megaparsec

testParserParses :: (Eq a, Show a) => Parser a -> String -> a -> TestTree
testParserParses parser string expected =
  testCase ("parse: " ++ show string) $
    Right expected @=? parse parser "<test>" string

testParserFails :: (Eq a, Show a) => Parser a -> String -> TestTree
testParserFails parser string =
  testCase ("doesn't parse: " ++ show string) $
    isLeft (parse parser "<test>" string) @? "parser didn't fail"

-- TODO: add test case for commandFrom/command, see commit
-- 77f5da460bd1d6b28fa29c27049f16dfbdd685b1 for bug that requires fix
