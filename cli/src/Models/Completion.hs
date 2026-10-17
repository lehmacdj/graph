-- | Completion of the word before the cursor, apart from the graph: what
-- is typed there and what can replace it.
--
-- The candidates that the graph decides, the nodes whose id begins with
-- what is typed and the labels that continue a path, come from
-- 'CompletionLookups', which the caller gives: the REPL reads them from
-- the graph it has open ("Executable.GraphEditor.Completion"), and
-- 'withoutGraph' has none of them, for a caller that has no graph. What
-- needs no graph is offered either way: the @/@ that is all that can
-- follow a term that is not a label, and nothing for a path that no
-- command runs or that has a directive in it.
module Models.Completion
  ( Completions (..),
    Candidate (..),
    CompletionLookups (..),
    complete,
    withoutGraph,
    completeWithoutGraph,
    spec_complete,
  )
where

import Control.Monad.State.Strict qualified as S
import Models.LegacyPartialPath
import Models.NID
import Models.Path.Simple
import Models.PathPattern.Compile (checkCopies)
import MyPrelude
import Utils.Base62 (isBase62Char)
import Utils.Testing

-- | What can replace the word before the cursor.
data Candidate = Candidate
  { text :: Text,
    -- | Whether the word is done with it: a caller may add a space after
    -- the only candidate that is (haskeline's @isFinished@).
    finished :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | The candidates for the word before the cursor, each to replace the
-- characters of the word that are typed.
data Completions = Completions
  { -- | How many of the characters just before the cursor each candidate
    -- replaces.
    replaced :: Int,
    candidates :: [Candidate]
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | What completion needs from a graph.
data CompletionLookups m = CompletionLookups
  { -- | The nodes whose id begins with these base62 characters.
    nodesWithPrefix :: Text -> m [NID],
    -- | The labels that continue this path from the current node to at
    -- least one match. The path is one that commands run ('checkCopies').
    labelsAfter :: Path -> m [Text]
  }

-- | Lookups for a caller without a graph: no nodes and no labels.
withoutGraph :: (Applicative m) => CompletionLookups m
withoutGraph =
  CompletionLookups
    { nodesWithPrefix = \_ -> pure [],
      labelsAfter = \_ -> pure []
    }

-- | The completions that need no graph.
completeWithoutGraph :: Text -> Completions
completeWithoutGraph = runIdentity . complete withoutGraph

-- | The completions of the word before the cursor, given the text before
-- the cursor (on the command line, the command and its arguments before
-- the path that is completed).
--
-- A word that is @\@@ and the start of an id is completed to the nodes
-- whose id begins with it, each written with its @\@@, which replaces the
-- one that is typed; when there are none, or the word is not such a
-- start, it is completed as the end of a path.
complete :: (Monad m) => CompletionLookups m -> Text -> m Completions
complete lookups before = case nidPrefixBefore before of
  Nothing -> pathCompletions
  Just prefix ->
    lookups.nodesWithPrefix prefix >>= \case
      [] -> pathCompletions
      nids ->
        pure
          Completions
            { replaced = length prefix + 1,
              candidates = [Candidate (tshow n) False | n <- nids]
            }
  where
    pathCompletions = case pathSiteBefore before of
      NoPathSite -> pure noCompletions
      -- not finished: a label follows it, not a space
      AfterTerm -> pure (Completions 0 [Candidate "/" False])
      LabelAfter p partial -> do
        labels <- lookups.labelsAfter p
        pure
          Completions
            { replaced =
                if partial `isSuffixOf` before then length partial else 0,
              candidates =
                [Candidate l False | l <- labels, partial `isPrefixOf` l]
            }

noCompletions :: Completions
noCompletions = Completions 0 []

-- | The base62 characters after an @\@@ at the end of the text, when they
-- are no more than an id has.
nidPrefixBefore :: Text -> Maybe Text
nidPrefixBefore before =
  case span isBase62Char (take (nidDigits + 1) (reverse (unpack before))) of
    (reversedPrefix, '@' : _) -> Just (pack (reverse reversedPrefix))
    _ -> Nothing

-- | What the end of the path before the cursor is, for completion.
data PathSite
  = -- | Nothing can be completed: no path, one that does not parse, has a
    -- directive in it or is one that no command runs.
    NoPathSite
  | -- | A term that is not a label, which only @/@ can follow.
    AfterTerm
  | -- | A label that is typed after a path: labels that continue the path
    -- and begin with it.
    LabelAfter Path Text
  deriving stock (Eq, Show)

pathSiteBefore :: Text -> PathSite
pathSiteBefore before =
  case getLegacyPartialPath (pack relevant) of
    Nothing -> NoPathSite
    Just (MissingSlash _) -> AfterTerm
    Just (LegacyPartialPath segments partial) ->
      let p = case segments of
            [] -> One
            p0 : rest -> foldl' (:/) p0 rest
       in -- a prefix that no command runs completes to nothing
          case checkCopies p of
            Right _ -> LabelAfter p (pack partial)
            Left _ -> NoPathSite
  where
    relevant = takeRelevantFromEnd (reverse (unpack before))

spec_complete :: Spec
spec_complete = do
  -- lookups that record what was looked up; the origin has the labels
  -- bar, baz and qux, and a has c
  let labels = \case
        One -> ["bar", "baz", "qux"]
        Literal "a" -> ["c"]
        _ -> []
      graph nids =
        CompletionLookups
          { nodesWithPrefix = \prefix -> do
              S.modify (<> ["nodes " <> prefix])
              pure (filter ((prefix `isPrefixOf`) . nidRepresentation) nids),
            labelsAfter = \p -> do
              S.modify (<> ["labels " <> tshow p])
              pure (labels p)
          }
      completing nids before = S.runState (complete (graph nids) before) []
      -- the text before the cursor after taking each candidate
      completedTo nids before =
        let c = fst (completing nids before)
         in [dropEnd c.replaced before <> x.text | x <- c.candidates]
      offers before replaced texts =
        it (show before) $
          fst (completing [] before)
            `shouldBe` Completions replaced [Candidate t False | t <- texts]
  describe "labels" do
    offers "ls " 0 ["bar", "baz", "qux"]
    offers "ls ba" 2 ["bar", "baz"]
    offers "ls a/" 0 ["c"]
    offers "ls a/c" 1 ["c"]
    offers "mv a q" 1 ["qux"]
    offers "mv a x" 1 []
    offers "ls x & ba" 2 ["bar", "baz"]
    it "looks up the path before the label" $
      snd (completing [] "ls a/c")
        `shouldBe` ["labels " <> tshow (Literal "a" :: Path)]
  describe "without a graph" do
    it "offers / after a term that is not a label" do
      completeWithoutGraph "ls (a & b)"
        `shouldBe` Completions 0 [Candidate "/" False]
      completeWithoutGraph "ls @"
        `shouldBe` Completions 0 [Candidate "/" False]
    it "offers no label" $
      completeWithoutGraph "ls a/b" `shouldBe` Completions 1 []
  describe "nothing to complete" do
    -- and no labels are looked up
    let nothing before = it (show before) do
          let (completions, looked) = completing [smallNID 1] before
          completions `shouldBe` noCompletions
          filter ("labels " `isPrefixOf`) looked `shouldBe` []
    -- a directive
    nothing "ls %last/a"
    nothing "ls %history(0)/"
    -- an incomplete id that no node has
    nothing "ls @x"
    -- a quote that is not closed
    nothing "ls \"a b"
    -- a prefix that no command runs: it would copy a meeting point
    nothing "ls (a & b)/(((c & y) | k)/@/e & d)/"
  describe "ids" do
    -- 000000000001, 000000000002 and 000000001000
    let n1 = smallNID 1
        n2 = smallNID 2
        far = smallNID (62 ^ (3 :: Int))
    -- the candidates are whole ids with their @, and replace the @ that
    -- is typed too
    it "offers the nodes whose id begins with what is typed" do
      completing [n1, n2, far] "ls @000000000"
        `shouldBe` ( Completions 10 [Candidate (tshow n) False | n <- [n1, n2]],
                     ["nodes 000000000"]
                   )
      fst (completing [n1] "ls @")
        `shouldBe` Completions 1 [Candidate (tshow n1) False]
    it "leaves one @ before the id it completes" do
      completedTo [n1] "ls @0" `shouldBe` ["ls @000000000001"]
      completedTo [n1] "ls a/@" `shouldBe` ["ls a/@000000000001"]
    it "completes a path when no node's id begins with it" $
      completing [far] "ls @0000000000"
        `shouldBe` (noCompletions, ["nodes 0000000000"])
    it "completes the @ of a path when there is no node" $
      completing [] "ls @"
        `shouldBe` (Completions 0 [Candidate "/" False], ["nodes "])
    it "does not take more characters than an id has for one" $
      completing [n1] "ls @0000000000001"
        `shouldBe` (noCompletions, [])
