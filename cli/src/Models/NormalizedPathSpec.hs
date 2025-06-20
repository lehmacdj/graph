module Models.NormalizedPathSpec where

import Lang.NormalizedPath.Parse
import Lang.Parsing
import Lang.Path.Parse
import Models.NormalizedPath
import TestPrelude

spec_normalizePath :: Spec
spec_normalizePath = describe "normalizePath" do
  let normalizesTo pathStr expectedStr =
        it (show pathStr <> " normalizes to " <> show expectedStr) do
          path <- parseForTest "path" (pPath' pSmallNID transition) pathStr
          expected <- parseForTest "normalized path" (pNormalizedPath transition) expectedStr
          normalizePath path `shouldBe` expected

  -- Basic path normalization tests
  "@" `normalizesTo` "@"
  "foo" `normalizesTo` "foo"
  "*" `normalizesTo` "*"

  -- Union tests
  "foo + bar" `normalizesTo` "foo + bar"
  "@ + foo" `normalizesTo` "@ + foo"

  -- Intersection tests
  "foo & bar" `normalizesTo` "[foo & bar]"
  "@ & foo" `normalizesTo` "@[foo]"
  "@ & foo & @/bar" `normalizesTo` "@[foo & bar]"

  -- Sequence tests
  "foo/bar" `normalizesTo` "foo /| bar"
  "foo/*" `normalizesTo` "foo /| *"
  "foo/@" `normalizesTo` "[foo]>@"
  "@/foo" `normalizesTo` "[@<foo]"
  "*/foo" `normalizesTo` "* /| foo"

  -- Complex combinations
  "(foo + bar)/baz" `normalizesTo` "foo /| baz + bar /| baz"
  "foo/(bar + baz)" `normalizesTo` "foo /| bar + foo /| baz"
  "(foo & bar)/baz" `normalizesTo` "(foo & bar) /| baz"
  "foo/(bar & baz)" `normalizesTo` "foo /| (bar & baz)"
  "foo/bar/baz" `normalizesTo` "foo /| (bar /| baz)"
  "(foo/bar)/baz" `normalizesTo` "foo /| (bar /| baz)"
  "foo/(bar/baz)" `normalizesTo` "foo /| bar /| baz"

  -- Mixed operations
  "foo/bar + baz" `normalizesTo` "foo/|bar + baz"
  "foo + bar/baz" `normalizesTo` "foo + bar/|baz"
  "foo/bar & baz" `normalizesTo` "[foo/|bar & baz]"
  "foo & bar/baz" `normalizesTo` "[foo & bar/|baz]"

  -- joining anchors
  "foo/@ & bar" `normalizesTo` "[foo & bar]>@"
  "@/foo & bar" `normalizesTo` "[@<foo & bar]"
  "@/foo & @1/bar" `normalizesTo` "[@<foo & @1<bar]"
  "@/foo & @1/bar/@2" `normalizesTo` "[@<foo & @1<bar]>@2"
  "@/foo & @/bar/@2" `normalizesTo` "[@<foo & @<bar]>@2"
  "@1/foo & @1/bar/@2" `normalizesTo` "[@1<foo & @1<bar]>@2"
  "baz/(@/foo & @1/bar)" `normalizesTo` "baz/@1|(foo & bar)"
  "foo/@/(@/bar & @2/baz)" `normalizesTo` "foo/@2|(bar & baz)"
  "@1 & @2" `normalizesTo` "!"
  "foo & !" `normalizesTo` "!"
  "! + foo" `normalizesTo` "foo"
