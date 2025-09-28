module Lang.Path.Partial
  ( getPartialPath,
    takeRelevantFromEnd,
    PartialPath (..),
    test_takeRelevantFromEnd,
    test_pPartialPath,
  )
where

import Graph.SystemNodes (tagsNID)
import Lang.Parsing
import Lang.ParsingSpec
import Lang.Path.Parse
import Models.Path.Simple
import MyPrelude hiding (many, try)
import TestPrelude hiding (many, try)
import Text.Megaparsec
import Text.Megaparsec.Char

-- | Try to parse a path, returning a partial path that is the last one
-- in the input string, this partial path will be the input for completion
getPartialPath :: String -> Maybe PartialPath
getPartialPath i = case parse pLastPartialPath "<completion>" i of
  Left _ -> Nothing
  Right r -> Just r

pPathSegment :: Parser (Path String)
pPathSegment = convertDirectivesToErrors (pathTerm transition)

-- | a list of path segments that are interpreted as being separated by
-- concatenation, followed by a string that represents a partial transition or something else
data PartialPath
  = PartialPath [Path String] String
  | MissingSlash [Path String]
  deriving (Show, Eq, Ord)

pPartialPath :: Parser PartialPath
pPartialPath = do
  prepath' <-
    try (Right [Absolute tagsNID] <$ (symbol "#" *> eof))
      <|> try (Left <$> pPathSegment `sepBy` symbol "/")
      <|> (Right <$> pPathSegment `endBy` symbol "/")
  case prepath' of
    Right prepath -> pure $ PartialPath prepath ""
    Left prepath -> case unsnoc prepath of
      Just (path, Literal end) -> pure $ PartialPath path end
      -- this is the case when parsing a tag-like path like "#foobar"
      Just ([], tn@(Absolute _) :/ Literal end) -> pure $ PartialPath [tn] end
      -- if the last thing in the path isn't a literal, we can't complete it
      -- and the only thing that could come next is a slash
      Just _ -> pure $ MissingSlash prepath
      Nothing -> pure $ PartialPath [] ""

pLastPartialPath :: Parser PartialPath
pLastPartialPath =
  try (c *> pPartialPath <* eof)
    <|> c
    *> pPath transition
    *> pPartialPath
    <* eof
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

test_pPartialPath :: TestTree
test_pPartialPath =
  testGroup
    "pPartialPath"
    [ "#" `parsesTo` PartialPath [Absolute tagsNID] "",
      "#foo" `parsesTo` PartialPath [Absolute tagsNID] "foo",
      "(a + b)" `parsesTo` MissingSlash [Literal "a" :+ Literal "b"],
      "a/b/" `parsesTo` PartialPath [Literal "a", Literal "b"] "",
      "a/b" `parsesTo` PartialPath [Literal "a"] "b"
    ]
  where
    parsesTo i = testCase ("parse: " ++ show i) . testParserParses pPartialPath i

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
