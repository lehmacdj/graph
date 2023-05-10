module Lang.Command.ParseSpec where

import Data.Either (isLeft)
import Lang.Command
import Lang.Command.Parse
import Lang.Path
import TestPrelude

test_parseCommand :: TestTree
test_parseCommand =
  testGroup
    "parseCommand"
    [ "t hello/world #"
        `parsesTo` Tag (Literal "hello" :/ Literal "world") One,
      "tag \"hello/world\" #"
        `parsesTo` Tag (Literal "hello/world") One,
      "tag hello-world #; tag foo-bar #"
        `parsesTo` Seq (twoElemList (Tag (Literal "hello-world") One) (Tag (Literal "foo-bar") One) []),
      "at * {nid; si}"
        `parsesTo` At Wild (Seq (twoElemList NodeId ShowImage [])),
      "{at * nid; si}"
        `parsesTo` Seq (twoElemList (At Wild NodeId) ShowImage []),
      -- these tests would be nice to be able to support eventually but don't
      -- because I hacked ; into the parser haphazardly requiring mandatory
      -- braces surrounding it
      "at * nid; si"
        `parsesTo` Seq (twoElemList (At Wild NodeId) ShowImage []),
      "{nid}" `parsesTo` NodeId,
      "{nid; si}" `parsesTo` Seq (twoElemList NodeId ShowImage []),
      -- regression test: this used to parse to t ouch hello-world; incorrectly
      -- not requiring a space between t and ouch
      parseFails "touch hello-world",
      parseFails " t hello/world #;"
    ]
  where
    parsesTo string expected =
      testCase ("parse: " ++ show string) $
        Right expected @=? parseCommand string
    parseFails string =
      testCase ("doesn't parse: " ++ show string) $
        isLeft (parseCommand string) @? "parseCommand didn't fail"
