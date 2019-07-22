module Lang.APath.Parse where

import Text.Megaparsec

import Lang.Path.Parse
import Lang.Parsing
import Lang.APath

pAPath :: Parser t -> Parser (APath t)
pAPath pTransition =
  try (Absolute <$> (nodeId <* symbol ":") <*> pPath pTransition)
  <|> Relative <$> pPath pTransition
