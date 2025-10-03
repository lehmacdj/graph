module Lang.Parsing.Common where

import MyPrelude
import Text.Megaparsec

data CustomParseError
  = IllegalDirective
  { startPos :: SourcePos,
    endPos :: SourcePos
  }
  deriving (Eq, Ord, Show)

instance ShowErrorComponent CustomParseError where
  showErrorComponent (IllegalDirective start end) =
    "Illegal directive " ++ " at " ++ show start ++ "-" ++ show end

-- | TODO: switch to Text input type
type Parser = Parsec CustomParseError String

type ParseError' = ParseError String CustomParseError
