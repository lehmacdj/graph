-- | Megaparsec support for parsing strings that make up paths.
-- Generic over the custom error type, it doesn't throw any.
module Lang.Path.Parse where

import Text.Megaparsec

import Data.Void

import Lang.Path

type Parser e = Parsec e String

pPath :: Parser e (Path t)
pPath = undefined
