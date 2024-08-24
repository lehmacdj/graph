module Lang.Path.Partial where

import Lang.Parsing
import Lang.Path
import Lang.Path.Parse
import MyPrelude hiding (many, try)
import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char

-- | from Text.Megaparsec
initialState :: String -> s -> MP.State s e
initialState name i =
  MP.State
    { stateInput = i,
      stateOffset = 0,
      statePosState =
        PosState
          { pstateInput = i,
            pstateOffset = 0,
            pstateSourcePos = initialPos name,
            pstateTabWidth = defaultTabWidth,
            pstateLinePrefix = ""
          },
      stateParseErrors = []
    }

-- | Try to parse a path, returning a partial path that is the last one
-- in the input string, this partial path will be the input for completion
getPartialPath :: String -> Maybe PartialPath
getPartialPath i = case parse pLastPartialPath "<completion>" i of
  Left _ -> Nothing
  Right r -> Just r

pPathSegment :: Parser (Path String)
pPathSegment = pathTerm transition

-- | a list of path segments that are interpreted as being separated by
-- concatenation, followed by a string that represents a partial transition or something else
data PartialPath
  = PartialPath [Path String] String
  | MissingSlash [Path String]
  deriving (Show, Eq, Ord)

pPartialPath :: Parser PartialPath
pPartialPath = do
  prepath' <-
    try (Left <$> pPathSegment `sepBy` symbol "/")
      <|> (Right <$> pPathSegment `endBy` symbol "/")
  case prepath' of
    Right prepath -> pure $ PartialPath prepath ""
    Left prepath -> case unsnoc prepath of
      Just (path, Literal end) -> pure $ PartialPath path end
      Just _ -> pure $ MissingSlash prepath
      Nothing -> pure $ PartialPath [] ""

pLastPartialPath :: Parser PartialPath
pLastPartialPath =
  try (c *> pPartialPath <* eof)
    <|> c *> pPath transition *> pPartialPath <* eof
  where
    c = lexeme (many (oneOf (":.-_" :: String) <|> alphaNumChar) <* lookAhead s)

consUnless :: Bool -> a -> [a] -> [a]
consUnless False = (:)
consUnless True = flip const

-- | optimization possibility for better autocompletion, we could also
-- handle & and trim the possible auto completions based on &'s that are
-- present before.
-- Unfortunately the semantics make this near impossible so it probably best
-- to ignore this
--
-- THIS NEEDS TO TAKE A REVERSED INPUT STRING AS AN ARGUMENT AND RETURNS A
-- UNREVERSED STRING THAT SHOULD BE FED TO THE PARSER
takeRelevantFromEnd :: String -> String
takeRelevantFromEnd i = go (0 :: Int) i False ""
  where
    go _ "" _ acc = acc
    go n (')' : rest) d acc = go (n + 1) rest d (consUnless d ')' acc)
    go n ('(' : rest) d acc
      | n == 0 = go 0 rest False acc
      | otherwise = go (n - 1) rest d (consUnless d '(' acc)
    go 0 ('+' : rest) _ acc = go 0 rest True acc
    go 0 ('&' : rest) _ acc = go 0 rest True acc
    go n (x : rest) d acc = go n rest d (consUnless d x acc)
