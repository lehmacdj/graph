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
  = IllegalDirective
      { startPos :: SourcePos,
        endPos :: SourcePos
      }
  | IncompleteFullNID
      { sourceRange :: SourceRange,
        base62Chars :: Text
      }
  deriving (Eq, Ord, Show)

instance ShowErrorComponent CustomParseError where
  showErrorComponent (IllegalDirective start end) =
    "Illegal directive " ++ " at " ++ show start ++ "-" ++ show end
  showErrorComponent (IncompleteFullNID pos nidStr) =
    "Incomplete NID, was expecting full NID with "
      <> show nidDigits
      <> " base62 characters, but had only '@"
      <> unpack nidStr
      <> "' at "
      <> show pos

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
