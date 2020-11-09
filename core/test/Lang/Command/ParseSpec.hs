module Lang.Command.ParseSpec where

import Data.Either (isLeft)
import Lang.APath
import Lang.Command
import Lang.Command.Parse
import TestPrelude

test_parseCommand :: TestTree
test_parseCommand =
  testGroup
    "parseCommand"
    [ "t hello/world #"
        `parsesTo` Tag (Relative (Literal "hello" :/ Literal "world")) (Relative One),
      "tag \"hello/world\" #"
        `parsesTo` Tag (Relative (Literal "hello/world")) (Relative One),
      -- regression test: this used to parse to t ouch hello-world; incorrectly
      -- not requiring a space between t and ouch
      parseFails "touch hello-world"
    ]
  where
    parsesTo string expected =
      testCase ("parse: " ++ show string) $
        Right expected @=? parseCommand string
    parseFails string =
      testCase ("doesn't parse: " ++ show string) $
        isLeft (parseCommand string) @? "parseCommand didn't fail"
