module Lang.APath.Parse where

import Lang.APath
import Lang.Parsing
import Lang.Path.Parse
import MyPrelude hiding (try)
import Text.Megaparsec

pAPath :: Parser t -> Parser (APath t)
pAPath pTransition =
  try (Absolute <$> (nodeId <* symbol ":") <*> pPath pTransition)
    <|> Relative <$> pPath pTransition
