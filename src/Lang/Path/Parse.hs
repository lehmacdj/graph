-- | Megaparsec support for parsing strings that make up paths.
-- Generic over the custom error type, it doesn't throw any.
module Lang.Path.Parse where

import Text.Megaparsec
import Control.Monad.Combinators.Expr

import Data.Void

import Lang.Path
import Lang.Parsing

pPath :: Parser (Path t)
pPath = undefined
