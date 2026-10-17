-- | What is wrong with text that does not parse, as data: where each
-- problem is and what the parser says of it, for a caller that shows
-- the problems in the text (syntax feedback) rather than printing them
-- under it ('errorBundlePretty').
module Utils.Parsing.Problems
  ( SyntaxProblem (..),
    syntaxProblems,
    spec_syntaxProblems,
  )
where

import Data.Text qualified as T
import Models.Path.Parse (parsePath)
import MyPrelude
import Utils.Parsing.Common
import Utils.Testing

-- | A problem with a stretch of the text. Offsets count characters
-- (Unicode code points) from the start of the text.
data SyntaxProblem = SyntaxProblem
  { start :: Int,
    -- | Where the stretch ends, exclusive: the start for a problem at a
    -- place, such as an unexpected end of the text.
    end :: Int,
    -- | What the parser says of it, as 'errorBundlePretty' says it after
    -- the place.
    message :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | The problems of a parse that failed, in the order the parser found
-- them.
syntaxProblems :: ParseErrorBundle' -> NonEmpty SyntaxProblem
syntaxProblems bundle = problem <$> bundleErrors bundle
  where
    posState = bundlePosState bundle
    input = pstateInput posState
    problem e =
      SyntaxProblem
        { start = errorOffset e,
          end = max (errorOffset e) (endOf e),
          message = T.stripEnd (pack (parseErrorTextPretty e))
        }
    endOf = \case
      TrivialError o (Just (Tokens ts)) _ -> o + length ts
      FancyError o errors ->
        case [r | ErrorCustom c <- toList errors, Just r <- [rangeOf c]] of
          r : _ -> offsetOf r.endPos
          [] -> o
      e -> errorOffset e
    -- the first offset at or after the position: positions count a tab
    -- to the next tab stop, so a column is not an offset
    inputEnd = pstateOffset posState + length input
    positions :: [(Int, SourcePos)]
    positions = fst (attachSourcePos id [0 .. inputEnd] posState)
    offsetOf pos =
      maybe inputEnd fst $
        find (\(_, p) -> lineColumn p >= lineColumn pos) positions
    lineColumn p = (sourceLine p, sourceColumn p)

-- | The stretch that a problem the parsers here report is about.
rangeOf :: CustomParseError -> Maybe SourceRange
rangeOf = \case
  IllegalDirective {startPos, endPos} -> Just (SourceRange startPos endPos)
  IncompleteFullNID {sourceRange} -> Just sourceRange
  SpliceInCommand {sourceRange} -> Just sourceRange
  MixedUnionOperators {sourceRange} -> Just sourceRange
  InvalidOverrideSetting {sourceRange} -> Just sourceRange

spec_syntaxProblems :: Spec
spec_syntaxProblems = do
  let problems t = either (toList . syntaxProblems) (const []) (parsePath t)
      stretches t = [(p.start, p.end) | p <- problems t]
  it "has none for a path that parses" $
    problems "a/b & %last" `shouldBe` []
  it "places an unexpected end of the text" $
    stretches "a/" `shouldBe` [(2, 2)]
  it "places an unexpected character" do
    stretches "a/)" `shouldBe` [(2, 3)]
    map (.message) (problems "a/)")
      `shouldSatisfy` all ("unexpected ')'\nexpecting " `isPrefixOf`)
  it "gives the stretch of an incomplete id" do
    stretches "@abc" `shouldBe` [(0, 4)]
    map (.message) (problems "@abc")
      `shouldSatisfy` all ("(IncompleteFullNID)" `isSuffixOf`)
  it "gives every problem it found" $
    stretches "@abc/@de" `shouldBe` [(0, 4), (5, 8)]
  it "gives the stretch of the second of mixed union operators" $
    stretches "a + b | c" `shouldBe` [(6, 7)]
  it "counts characters, not columns, after a tab" $
    stretches "\t@abc" `shouldBe` [(1, 5)]
