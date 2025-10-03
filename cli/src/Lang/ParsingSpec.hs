module Lang.ParsingSpec (module Lang.ParsingSpec, eof) where

import Lang.Parsing
import TestPrelude

testParserParses :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
testParserParses parser string expected =
  case runParserTest parser string of
    Right actual -> actual @=? expected
    Left err -> assertFailure $ "expected: " ++ show expected ++ "\nbut parser failed with:\n" ++ errorBundlePretty err

testParserFails :: (Eq a, Show a) => Parser a -> String -> Assertion
testParserFails parser string =
  case runParserTest parser string of
    Right x -> assertFailure $ "expected parser to fail, but it succeeded producing: " ++ show x
    Left _ -> pure ()

debugParser :: (Eq a, Show a) => Parser a -> String -> IO ()
debugParser parser string = do
  case runParserTest parser string of
    Right x -> say $ "Parser succeeded with: " ++ tshow x
    Left err -> say $ "Parser failed with:\n" ++ pack (errorBundlePretty err)

unit_command_emptyStillRequiresSpace :: Assertion
unit_command_emptyStillRequiresSpace = testParserFails (command "") "1"

unit_command_eofTerminated :: Assertion
unit_command_eofTerminated = testParserParses (command "t") "t" "t"

unit_command_unterminated :: Assertion
unit_command_unterminated = testParserFails (command "t") "tf"

unit_command_incomplete :: Assertion
unit_command_incomplete = testParserFails (command "tf") "t"

unit_command_spaceTerminated :: Assertion
unit_command_spaceTerminated = testParserParses (command "t") "t blech" "t"
