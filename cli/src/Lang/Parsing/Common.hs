module Lang.Parsing.Common where

import Models.Path
import MyPrelude
import Text.Megaparsec

data CustomParseError
  = IllegalDirective
  { directive :: Directive,
    startPos :: SourcePos,
    endPos :: SourcePos
  }
  deriving (Eq, Ord, Show)

instance ShowErrorComponent CustomParseError where
  showErrorComponent (IllegalDirective dir start end) =
    "Illegal directive " ++ show dir ++ " at " ++ show start ++ "-" ++ show end

-- | TODO: switch to Text input type
type Parser = Parsec CustomParseError String

type ParseError' = ParseError String CustomParseError
