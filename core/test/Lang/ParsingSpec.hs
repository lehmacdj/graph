module Lang.ParsingSpec where

import Data.Either (isLeft)
import Lang.Parsing
import TestPrelude
import Text.Megaparsec

testParserParses :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
testParserParses parser string expected =
  Right expected @=? parse parser "<test>" string

testParserFails :: (Eq a, Show a) => Parser a -> String -> Assertion
testParserFails parser string =
  isLeft (parse parser "<test>" string) @? "parser didn't fail"

unit_command_emptyStillRequiresSpace :: Assertion
unit_command_emptyStillRequiresSpace = testParserFails (command "") "1"

unit_command_eofTerminated :: Assertion
unit_command_eofTerminated = testParserParses (command "t") "t" "t"

unit_command_unterminated :: Assertion
unit_command_unterminated = testParserFails (command "t") "tf"

unit_command_incomplete :: Assertion
unit_command_incomplete = testParserFails (command "tf") "t"

unit_command_spaceTerminated :: Assertion
unit_command_spaceTerminated = testParserParses (command "t blech") "t" "t"
