module Models.LegacyPartialPath
  ( getLegacyPartialPath,
    takeRelevantFromEnd,
    LegacyPartialPath (..),
    test_takeRelevantFromEnd,
    test_pLegacyPartialPath,
  )
where

import Graph.SystemNodes (tagsNID)
import Models.Path.Parse
import Models.Path.Simple
import MyPrelude hiding (many, try)
import Text.Megaparsec (try)
import Text.Megaparsec.Char
import Utils.Parsing
import Utils.Testing

-- | Try to parse a path, returning a partial path that is the last one
-- in the input string, this partial path will be the input for completion
getLegacyPartialPath :: Text -> Maybe LegacyPartialPath
getLegacyPartialPath i = case runParser pLastLegacyPartialPath "<completion>" i of
  Left _ -> Nothing
  Right r -> Just r

pPathSegment :: Parser Path
pPathSegment = convertDirectivesToErrors (pathTerm ttransition)

-- | a list of path segments that are interpreted as being separated by
-- concatenation, followed by a string that represents a partial transition or something else
data LegacyPartialPath
  = LegacyPartialPath [Path] String
  | MissingSlash [Path]
  deriving (Show, Eq, Ord)

pLegacyPartialPath :: Parser LegacyPartialPath
pLegacyPartialPath = do
  prepath' <-
    try (Right [Absolute tagsNID] <$ (symbol "#" *> eof))
      <|> try (Left <$> pPathSegment `sepBy` symbol "/")
      <|> (Right <$> pPathSegment `endBy` symbol "/")
  case prepath' of
    Right prepath -> pure $ LegacyPartialPath prepath ""
    Left prepath -> case unsnoc prepath of
      Just (path, Literal end) -> pure $ LegacyPartialPath path (unpack end)
      -- this is the case when parsing a tag-like path like "#foobar"
      Just ([], tn@(Absolute _) :/ Literal end) -> pure $ LegacyPartialPath [tn] (unpack end)
      -- if the last thing in the path isn't a literal, we can't complete it
      -- and the only thing that could come next is a slash
      Just _ -> pure $ MissingSlash prepath
      Nothing -> pure $ LegacyPartialPath [] ""

pLastLegacyPartialPath :: Parser LegacyPartialPath
pLastLegacyPartialPath =
  try (c *> pLegacyPartialPath <* eof)
    <|> (c *> pPath ttransition *> pLegacyPartialPath <* eof)
  where
    c = lexeme (many (oneOf (":.-_" :: String) <|> alphaNumChar) <* lookAhead s)

consUnless :: Bool -> a -> [a] -> [a]
consUnless False = (:)
consUnless True = const id

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

test_pLegacyPartialPath :: TestTree
test_pLegacyPartialPath =
  testGroup
    "pLegacyPartialPath"
    [ "#" `parsesTo` LegacyPartialPath [Absolute tagsNID] "",
      "#foo" `parsesTo` LegacyPartialPath [Absolute tagsNID] "foo",
      "(a + b)" `parsesTo` MissingSlash [Literal "a" :+ Literal "b"],
      "a/b/" `parsesTo` LegacyPartialPath [Literal "a", Literal "b"] "",
      "a/b" `parsesTo` LegacyPartialPath [Literal "a"] "b"
    ]
  where
    parsesTo i = testCase ("parse: " ++ show i) . testParserParses pLegacyPartialPath i

test_takeRelevantFromEnd :: TestTree
test_takeRelevantFromEnd =
  testGroup
    "takeRelevantFromEnd"
    [ takeRelevantFromEnd' "a/(b + c" "a/ c",
      takeRelevantFromEnd' "a/(b + c/d" "a/ c/d",
      takeRelevantFromEnd' "(a + b)/e + (c + d)" " (c + d)",
      takeRelevantFromEnd' "(a + b)/(e + (c + d)" "(a + b)/ (c + d)",
      takeRelevantFromEnd' "a & b/" " b/",
      takeRelevantFromEnd' "a & b/(a + b & c/(d + e" " b/ c/ e",
      takeRelevantFromEnd' "@200 + @100/" " @100/",
      takeRelevantFromEnd' "#foo + #bar/" " #bar/"
    ]
  where
    takeRelevantFromEnd' i e = testCase i $ e @=? (takeRelevantFromEnd . reverse $ i)
