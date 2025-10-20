module Models.NormalizedPathSpec where

import Models.NormalizedPath
import Models.NormalizedPath.Parse
import Models.Path.Parse
import MyPrelude
import Utils.Parsing
import Utils.Testing

spec_normalizePath :: Spec
spec_normalizePath = describe "normalizePath" do
  let normalizesTo :: Text -> Text -> Spec
      normalizesTo pathStr expectedStr =
        it (show pathStr <> " normalizes to " <> show expectedStr) do
          path <- parseForTest "path" (convertDirectivesToErrors (pPath' pSmallNID)) pathStr
          expected <- parseForTest "normalized path" pNormalizedPath expectedStr
          normalizePath path `shouldBe` expected

  -- Basic path normalization tests
  "@" `normalizesTo` "@"
  "foo" `normalizesTo` "[foo]"
  "*" `normalizesTo` "[*]"

  -- merging anchors
  "@ & @" `normalizesTo` "@"
  "@1 & @" `normalizesTo` "@1"
  "@ & @1" `normalizesTo` "@1"
  "@1 & @2" `normalizesTo` "%never"
  "!{@1} & @2" `normalizesTo` "@2"
  "!{@1, @2} & @2" `normalizesTo` "%never"
  "!{@1, @2} & @" `normalizesTo` "!{@1, @2}"
  "!{@1, @2} & !{@3, @4}" `normalizesTo` "!{@1, @2, @3, @4}"

  -- Union tests
  "foo + bar" `normalizesTo` "[foo] + [bar]"
  "@ + foo" `normalizesTo` "@ + [foo]"

  -- Intersection tests
  "foo & bar" `normalizesTo` "[foo & bar]"
  "@ & foo" `normalizesTo` "@[foo]"
  "@ & foo & @/bar" `normalizesTo` "@[foo & bar]"

  -- Sequence tests
  "foo/bar" `normalizesTo` "[foo /| bar]"
  "foo/*" `normalizesTo` "[foo /| *]"
  "foo/@" `normalizesTo` "[foo]>@"
  "@/foo" `normalizesTo` "[@<foo]"
  "*/foo" `normalizesTo` "[* /| foo]"

  -- Complex combinations
  "(foo + bar)/baz" `normalizesTo` "[foo /| baz] + [bar /| baz]"
  "foo/(bar + baz)" `normalizesTo` "[foo /| bar] + [foo /| baz]"
  "(foo & bar)/baz" `normalizesTo` "[(foo & bar) /| baz]"
  "foo/(bar & baz)" `normalizesTo` "[foo /| (bar & baz)]"
  "foo/bar/baz" `normalizesTo` "[foo /| (bar /| baz)]"
  "(foo/bar)/baz" `normalizesTo` "[foo /| (bar /| baz)]"
  "foo/(bar/baz)" `normalizesTo` "[foo /| bar /| baz]"

  -- Mixed operations
  "foo/bar + baz" `normalizesTo` "[foo/|bar] + [baz]"
  "foo + bar/baz" `normalizesTo` "[foo] + [bar/|baz]"
  "foo/bar & baz" `normalizesTo` "[foo/|bar & baz]"
  "foo & bar/baz" `normalizesTo` "[foo & bar/|baz]"

  -- joining anchors
  "foo/@ & bar" `normalizesTo` "[foo & bar]>@"
  "@/foo & bar" `normalizesTo` "[@<foo & bar]"
  "@/foo & @1/bar" `normalizesTo` "[@<foo & @1<bar]"
  "@/foo & @1/bar/@2" `normalizesTo` "[@<foo & @1<bar]>@2"
  "@/foo & @/bar/@2" `normalizesTo` "[@<foo & @<bar]>@2"
  "@1/foo & @1/bar/@2" `normalizesTo` "[@1<foo & @1<bar]>@2"
  "baz/(@/foo & @1/bar)" `normalizesTo` "[baz/@1|(foo & bar)]"
  "foo/@/(@/bar & @2/baz)" `normalizesTo` "[foo/@2|(bar & baz)]"
  "@1 & @2" `normalizesTo` "%never"
  "foo & %never" `normalizesTo` "%never"
  "%never + foo" `normalizesTo` "[foo]"

  -- nested rooted paths
  "(@1/a & @2/b)/@/(c & d)" `normalizesTo` "[[@1<a & @2<b]>@<(c & d)]"
  "(@1/a & @2/b)/@/c & @3/d" `normalizesTo` "[[@1<a & @2<b]>@<c & @3<d]"

  -- inverse / backlinks
  "~foo" `normalizesTo` "[~foo]"
  "~(foo + bar)" `normalizesTo` "[~foo] + [~bar]"
  "~(foo & bar)" `normalizesTo` "[~foo & ~bar]"
  "~foo & bar/baz" `normalizesTo` "[~foo & bar /| baz]"
  "~(foo/bar)" `normalizesTo` "[~foo /| ~bar]"
  -- this is surprisingly similar to @[x /@| y] but the target is different
  "@/x/@ & @/~y/@" `normalizesTo` "[@<x & @<~y]>@"

  -- inverse/backlinks with loops
  -- there's no super principled way to distinguish between cases like the
  -- below; the normalization algorithm makes a deterministic but arbitrary
  -- choice based on the Ord instance of the involved things
  "~@" `normalizesTo` "@"
  "~(@ & foo)" `normalizesTo` "@[foo]"
  "@ & ~foo" `normalizesTo` "@[foo]"
  "~(@ & foo/bar)" `normalizesTo` "@[bar /| foo]"
  "@ & ~foo/bar" `normalizesTo` "@[~bar /| foo]"
  "@ & foo/~bar" `normalizesTo` "@[bar /| ~foo]"
  "~(@ & ~foo/bar)" `normalizesTo` "@[bar /| ~foo]"
  "(@ & w/~x & y/~z)" `normalizesTo` "@[w /| ~x & y /| ~z]"
  "(@ & ~w/~x & y/~z)" `normalizesTo` "@[x /| w & y /| ~z]"

spec_leastConstrainedNormalizedPath :: Spec
spec_leastConstrainedNormalizedPath = describe "leastConstrainedNormalizedPath" do
  let whenLeastConstrainedIsEquivalentTo :: Text -> Text -> Spec
      whenLeastConstrainedIsEquivalentTo p expected =
        it (show p <> " when least constrained is equivalent to " <> show expected) do
          p' <- parseForTest "normalized path" pNormalizedPath p
          expected' <- parseForTest "normalized path" pNormalizedPath expected
          leastConstrainedNormalizedPath p' `shouldBe` leastNodesNormalizedPath expected'
  "[(a & b)/|(c & d)]"
    `whenLeastConstrainedIsEquivalentTo` "[@<a/@|c & @<a/@|d & @<b/@|c & @<b/@|d]>@"
  "[(a & b)/|((c & d)/|(e & f))]"
    `whenLeastConstrainedIsEquivalentTo` "[@<a/@|c/@|e & @<a/@|c/@|f & @<a/@|d/@|e & @<a/@|d/@|f & @<b/@|c/@|e & @<b/@|c/@|f & @<b/@|d/@|e & @<b/@|d/@|f]>@"
  -- reassociating shouldn't affect the final ordering
  "[((a & b)/|(c & d))/|(e & f)]"
    `whenLeastConstrainedIsEquivalentTo` "[@<a/@|c/@|e & @<a/@|c/@|f & @<a/@|d/@|e & @<a/@|d/@|f & @<b/@|c/@|e & @<b/@|c/@|f & @<b/@|d/@|e & @<b/@|d/@|f]>@"
  "[z/@|(a & b)/|(c & d)]"
    `whenLeastConstrainedIsEquivalentTo` "[@<z/@|(a/@|c & a/@|d & b/@|c & b/@|d)]>@"
  "[(a & b)/@|(c & d)]" `whenLeastConstrainedIsEquivalentTo` "[@<(a & b)/@|(c & d)]>@"
  "[[@1<a & @2<b]<(c & d)]"
    `whenLeastConstrainedIsEquivalentTo` "[@1<a/@|c & @1<a/@|d & @2<b/@|c & @2<b/@|d]>@"
