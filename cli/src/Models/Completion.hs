-- | Completion of the word before the cursor, apart from the graph: what
-- is typed there and what can replace it.
--
-- The candidates that the graph decides, the nodes whose id begins with
-- what is typed and the labels that continue a path, come from
-- 'CompletionLookups', which the caller gives: the REPL reads them from
-- the graph it has open ("Executable.GraphEditor.Completion"), and
-- 'withoutGraph' has none of them, for a caller that has no graph. What
-- needs no graph is offered either way: the names of commands
-- ('commandNames'), the names after @%@ ('percentNames'), the @/@ that is
-- all that can follow a term that is not a label, and nothing for a path
-- that no command runs or that has a directive in it.
module Models.Completion
  ( Completions (..),
    Candidate (..),
    CompletionLookups (..),
    complete,
    completeCommandLine,
    withoutGraph,
    completeWithoutGraph,
    spec_complete,
  )
where

import Control.Monad.State.Strict qualified as S
import Data.Char (isAlpha, isSpace)
import Models.Command.Parse (commandNames, startsCommand)
import Models.LegacyPartialPath
import Models.NID
import Models.Path.Parse (parsePath, percentNames)
import Models.Path.Simple
import Models.PathPattern.Compile (checkCopies)
import MyPrelude
import Text.Megaparsec (bundleErrors, errorOffset)
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
-- the cursor: a command line up to the cursor.
--
-- A word where a command begins ('startsCommand') is completed to the
-- names of commands that begin with it ('commandNames'), each finished.
-- A word that is @%@ and the start of a name after it is completed to the
-- names that begin with it ('percentNames'), each with the @(@ of its
-- arguments if it takes any. A word that is @\@@ and the start of an id
-- is completed to the nodes whose id begins with it, each written with
-- its @\@@, which replaces the one that is typed; when there are none, or
-- the word is not such a start, it is completed as the end of a path.
complete :: (Monad m) => CompletionLookups m -> Text -> m Completions
complete = completeCommandLine commandNames

-- | 'complete' for a command line whose commands have these names: the
-- REPL's has @:q@ too.
completeCommandLine ::
  (Monad m) => [Text] -> CompletionLookups m -> Text -> m Completions
completeCommandLine names lookups before
  | Just word <- commandNameBefore before =
      pure
        Completions
          { replaced = length word,
            candidates = [Candidate n True | n <- names, word `isPrefixOf` n]
          }
  | otherwise = case nidPrefixBefore before of
      Nothing
        | Just name <- percentNameBefore before ->
            pure
              Completions
                { replaced = length name + 1,
                  candidates =
                    [ Candidate ("%" <> n <> bool "" "(" arguments) False
                    | (n, arguments) <- percentNames,
                      name `isPrefixOf` n
                    ]
                }
        | otherwise -> pathCompletions
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

-- | The word at the end of the text, when a command begins where it
-- does.
commandNameBefore :: Text -> Maybe Text
commandNameBefore before =
  guard (startsCommand (pack (reverse reversedStart)))
    $> pack (reverse reversedWord)
  where
    (reversedWord, reversedStart) = break endsWord (reverse (unpack before))
    endsWord c = isSpace c || c `elem` (";{}" :: String)

-- | The letters after a @%@ at the end of the text, when the @%@ starts a
-- term: where a label typed instead of it would be a label of its own, as
-- path completion reads the text ('pathSiteBefore'). So not in a label, a
-- quote, a regex or a URL, nor in a path with a directive (such as in a
-- directive's arguments).
percentNameBefore :: Text -> Maybe Text
percentNameBefore before =
  case span isAlpha (reverse (unpack before)) of
    (reversedName, '%' : reversedStart)
      | startsTerm (pack (reverse reversedStart)) ->
          Just (pack (reverse reversedName))
    _ -> Nothing
  where
    startsTerm start
      | LabelAfter _ "x" <- pathSiteBefore (start <> "x") = True
      | otherwise = False

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
    -- a space ends the argument before it: a new path starts
    offers "mv a " 0 ["bar", "baz", "qux"]
    offers "mv q " 0 ["bar", "baz", "qux"]
    offers "mv a  " 0 ["bar", "baz", "qux"]
    offers "mv a\t" 0 ["bar", "baz", "qux"]
    offers "mv a b " 0 ["bar", "baz", "qux"]
    offers "ls  " 0 ["bar", "baz", "qux"]
    -- but space can follow a /: the path goes on
    offers "mv a/ " 0 ["c"]
    offers "mv a / c" 1 ["c"]
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
  describe "names of commands" do
    let names before = map (.text) (completeWithoutGraph before).candidates
    it "offers the names that begin with what is typed, finished" do
      completeWithoutGraph "rm"
        `shouldBe` Completions 2 [Candidate t True | t <- ["rm", "rmnf"]]
      names ":" `shouldBe` [":debug", ":d", ":import", ":i"]
      names "" `shouldBe` commandNames
    it "offers them where a command begins" do
      names "nid; n" `shouldBe` ["nid"]
      names "{nid;n" `shouldBe` ["nid"]
      names "at (a & b) n" `shouldBe` ["nid"]
      names "at a {nid; fs" `shouldBe` ["fsck"]
    it "offers none where a command does not begin" do
      names "ls n" `shouldBe` []
      names "ls %override({p" `shouldBe` []
    it "offers no path where a command begins" $
      fst (completing [] "x") `shouldBe` Completions 1 []
    it "offers the names that it is given" $
      runIdentity (completeCommandLine [":q", "nid"] withoutGraph ":")
        `shouldBe` Completions 1 [Candidate ":q" True]
  describe "names after %" do
    it "offers the names that begin with what is typed" do
      completeWithoutGraph "ls %"
        `shouldBe` Completions
          1
          [ Candidate t False
          | t <- ["%never", "%history(", "%last", "%targets(", "%override("]
          ]
      completeWithoutGraph "ls a/%ta"
        `shouldBe` Completions 3 [Candidate "%targets(" False]
      completeWithoutGraph "ls (b & %la"
        `shouldBe` Completions 3 [Candidate "%last" False]
      completeWithoutGraph "ls %x" `shouldBe` Completions 2 []
    it "offers only names that the path parser takes after %" $
      -- a bare name parses as a path; one with arguments is read up to
      -- its end, where the arguments are missing
      for_ (completeWithoutGraph "ls %").candidates \c ->
        let accepted = case parsePath c.text of
              Right _ -> True
              Left e ->
                "(" `isSuffixOf` c.text
                  && map errorOffset (toList (bundleErrors e))
                    == [length c.text]
         in (c.text, accepted) `shouldBe` (c.text, True)
    it "offers no name in a label or a quote" do
      let offered before = map (.text) (completeWithoutGraph before).candidates
      offered "ls a%l" `shouldBe` []
      offered "ls \"%l" `shouldBe` []
      offered "ls \"foo %l" `shouldBe` []
    it "offers no name in a regex or a URL" do
      let offered before = map (.text) (completeWithoutGraph before).candidates
      offered "ls !{regex %l" `shouldBe` []
      offered "ls https://example.com/?q=%l" `shouldBe` []
    it "offers names in a path after another argument" $
      completeWithoutGraph "mv a %la"
        `shouldBe` Completions 3 [Candidate "%last" False]
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
