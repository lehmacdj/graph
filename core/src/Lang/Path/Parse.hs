-- | Megaparsec support for parsing strings that make up paths.
-- Generic over the custom error type, it doesn't throw any.
module Lang.Path.Parse where

import Control.Monad.Combinators.Expr
import Data.Functor
import Lang.Parsing
import Lang.Path
import Text.Megaparsec

pathTerm :: Parser t -> Parser (Path t)
pathTerm pTransition =
  (symbol "#" $> One)
    <|> (symbol "*" $> Wild)
    <|> (Literal <$> pTransition)
    <|> parens (pPath pTransition)

binary :: String -> (Path t -> Path t -> Path t) -> Operator Parser (Path t)
binary name f = InfixL (f <$ symbol name)

table :: [[Operator Parser (Path t)]]
table =
  [ [binary "/" (:/)],
    [binary "&" (:&)],
    [binary "+" (:+)]
  ]

pPath :: Parser t -> Parser (Path t)
pPath pTransition = makeExprParser (pathTerm pTransition) table
