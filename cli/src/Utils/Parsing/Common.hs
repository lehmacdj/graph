module Utils.Parsing.Common
  ( module X,
    module Utils.Parsing.Common,
  )
where

import Models.NID
import MyPrelude
import Text.Megaparsec hiding (runParser, runParser')
import Text.Megaparsec as MP (runParser)
import Text.Megaparsec as X hiding (getOffset, getSourcePos, runParser, runParser', try)
import Text.Megaparsec.State as X (initialPosState)

data CustomParseError
  = -- | A directive where only a plain path is allowed. Commands accept
    -- directives (expanded when they run); this is for parsers that need a
    -- plain path, such as completion's 'Models.LegacyPartialPath'.
    IllegalDirective
      { startPos :: SourcePos,
        endPos :: SourcePos
      }
  | IncompleteFullNID
      { sourceRange :: SourceRange,
        base62Chars :: Text
      }
  | -- | A @%{…}@ splice in a command: splices only work in the path
    -- quasiquoter, so the CLI rejects them before running anything.
    SpliceInCommand
      { sourceRange :: SourceRange,
        spliceExpr :: String
      }
  | -- | Two different union operators (@+@, @|@, @++@) at the same level
    -- without parentheses, e.g. @a + b | c@.
    MixedUnionOperators
      { sourceRange :: SourceRange,
        firstOperator :: Text,
        secondOperator :: Text
      }
  | -- | A setting @%override@ does not know, or a value it does not accept
    InvalidOverrideSetting
      { sourceRange :: SourceRange,
        problem :: Text
      }
  deriving (Eq, Ord, Show)

instance ShowErrorComponent CustomParseError where
  showErrorComponent (IllegalDirective start end) =
    "Directives (%targets, %history, %last, URLs) are not allowed here; "
      <> "remove the one at "
      <> showSpan start end
      <> ". (IllegalDirective)"
  showErrorComponent (IncompleteFullNID range nidStr) =
    "Incomplete NID '@"
      <> unpack nidStr
      <> "' at "
      <> showSpan range.startPos range.endPos
      <> ": a NID needs all "
      <> show nidDigits
      <> " base62 characters. (IncompleteFullNID)"
  showErrorComponent (MixedUnionOperators range first second) =
    "The union operators "
      <> unpack first
      <> " and "
      <> unpack second
      <> " are mixed without parentheses; add parentheses around one of "
      <> "them, e.g. (a "
      <> unpack first
      <> " b) "
      <> unpack second
      <> " c, near the "
      <> unpack second
      <> " at "
      <> showSpan range.startPos range.endPos
      <> ". (MixedUnionOperators)"
  showErrorComponent (InvalidOverrideSetting range problem) =
    unpack problem
      <> " at "
      <> showSpan range.startPos range.endPos
      <> ". (InvalidOverrideSetting)"
  showErrorComponent (SpliceInCommand range expr) =
    "The splice directive %{"
      <> expr
      <> "} only works in the path quasiquoter, not the CLI; remove the one at "
      <> showSpan range.startPos range.endPos
      <> ". (SpliceInCommand)"

-- | A source span for error messages, e.g. @line 1, columns 4-8@. The end
-- position is exclusive.
showSpan :: SourcePos -> SourcePos -> String
showSpan start end
  | sourceLine start == sourceLine end =
      "line "
        <> show (unPos (sourceLine start))
        <> ", columns "
        <> show (unPos (sourceColumn start))
        <> "-"
        <> show (max (unPos (sourceColumn start)) (unPos (sourceColumn end) - 1))
  | otherwise = sourcePosPretty start <> " to " <> sourcePosPretty end

newtype ParserOptions = ParserOptions
  { useFakeSourceRanges :: Bool
  }
  deriving (Eq, Show, Generic)

defaultParserOptions :: ParserOptions
defaultParserOptions = ParserOptions False

type Parser = ReaderT ParserOptions (Parsec CustomParseError Text)

type ParseError' = ParseError Text CustomParseError

type ParseErrorBundle' = ParseErrorBundle Text CustomParseError

data SourceRange = SourceRange
  { startPos :: SourcePos,
    endPos :: SourcePos
  }
  deriving (Eq, Ord, Show, Generic, Lift)

testOffset :: Int
testOffset = 0

testPos :: SourcePos
testPos = initialPos "<test>"

testRange :: SourceRange
testRange = SourceRange testPos testPos

runParser' ::
  ParserOptions ->
  Parser a ->
  -- | The file name used in error messages
  String ->
  -- | The input to parse
  Text ->
  Either (ParseErrorBundle Text CustomParseError) a
runParser' opts parser = MP.runParser (runReaderT parser opts)

runParser ::
  Parser a ->
  -- | The file name used in error messages
  String ->
  -- | The input to parse
  Text ->
  Either (ParseErrorBundle Text CustomParseError) a
runParser = runParser' defaultParserOptions

runParserTest ::
  Parser a ->
  -- | The input to parse
  Text ->
  Either (ParseErrorBundle Text CustomParseError) a
runParserTest p =
  runParser'
    (defaultParserOptions {useFakeSourceRanges = True})
    p
    "<test>"
