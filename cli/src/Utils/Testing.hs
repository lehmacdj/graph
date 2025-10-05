module Utils.Testing
  ( module X,
    withTempGraph,
    withEmptyTempGraph,
    representedByJson,
    runTests,
    runSpec,
    connect,
    edge,
    edge',
    node,
    disconnectedGraph,
    stronglyConnectedGraph,
    parseForTest,
    testParserParses,
    testParserFails,
  )
where

import DAL.Serialization (initializeGraph)
import Data.Aeson
import Models.Connect
import Models.Edge
import Models.Graph (Graph, emptyGraph, insertEdges, insertNodes)
import Models.NID
import Models.NID as X (smallNID)
import Models.Node (Node, emptyNode)
import MyPrelude hiding (assert)
import System.Directory (copyFile)
import System.Directory.Tree (AnchoredDirTree ((:/)))
import System.Directory.Tree qualified as DT
import Test.Hspec as X (Spec, describe, it)
import Test.Hspec.Expectations as X
import Test.Tasty as X
import Test.Tasty.HUnit as X
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.QuickCheck as X
import Utils.Parsing.Common

-- | initializes a graph that is either empty, or based on a template graph
-- at the specified location
withTempGraph ::
  -- | Template graph (or Nothing for an empty graph)
  Maybe FilePath ->
  (FilePath -> IO a) ->
  IO a
withTempGraph m_template action = do
  withSystemTempDirectory "test.g" $ \tmpDir -> do
    case m_template of
      Just template -> do
        _ :/ readTree <- DT.build template
        whenNonNull (DT.failures readTree) \failures ->
          sayErr $ "reading template failed with: " <> tshow failures
        -- we need to overwrite the basename to @.@ so that we don't have an
        -- excess directory layer in the output
        let treeToWrite = tmpDir :/ (readTree & DT._name .~ ".")
        _ :/ writtenTree <- DT.writeDirectoryWith (flip copyFile) treeToWrite
        whenNonNull (DT.failures writtenTree) \failures ->
          sayErr $ "copying template failed with: " <> tshow failures
      Nothing -> initializeGraph tmpDir
    action tmpDir

withEmptyTempGraph :: (FilePath -> IO a) -> IO a
withEmptyTempGraph = withTempGraph Nothing

representedByJson ::
  (ToJSON a, FromJSON a, Eq a, Show a) => a -> LByteString -> TestTree
representedByJson x j =
  testGroup
    (show x ++ " has correct json representation")
    [ testCase "serializes" $ encode x `shouldBe` j,
      testCase "deserializes" $ eitherDecode j `shouldBe` Right x
    ]

-- | Convenience export for running tests in GHCi with a more ergonomic name
runTests :: TestTree -> IO ()
runTests = defaultMain

runSpec :: Spec -> IO ()
runSpec s = defaultMain =<< testSpec "<interactive>" s

connect :: Int -> Connect ()
connect n = Connect () (smallNID n)

edge :: Int -> Int -> Edge ()
edge i o = Edge (smallNID i) () (smallNID o)

edge' :: Int -> a -> Int -> Edge a
edge' i t o = Edge (smallNID i) t (smallNID o)

node :: (ValidTransition t) => Int -> Node t ()
node i = emptyNode (smallNID i)

disconnectedGraph :: [Int] -> Graph () ()
disconnectedGraph nids = insertNodes (node <$> nids) emptyGraph

stronglyConnectedGraph :: [Int] -> Graph () ()
stronglyConnectedGraph nids =
  disconnectedGraph nids & insertEdges (uncurry edge <$> allAnyOrderPairs nids)

parseForTest :: String -> Parser a -> Text -> IO a
parseForTest whatToParse parser input =
  case runParser (parser <* eof) ("<" <> whatToParse <> ">") input of
    Left err -> do
      expectationFailure $ "Failed to parse: " <> errorBundlePretty err
      error "unreachable"
    Right result -> pure result

testParserParses :: (Eq a, Show a) => Parser a -> Text -> a -> Assertion
testParserParses parser string expected =
  case runParserTest parser string of
    Right actual -> actual @=? expected
    Left err -> assertFailure $ "expected: " ++ show expected ++ "\nbut parser failed with:\n" ++ errorBundlePretty err

testParserFails :: (Eq a, Show a) => Parser a -> Text -> Assertion
testParserFails parser string =
  case runParserTest parser string of
    Right x -> assertFailure $ "expected parser to fail, but it succeeded producing: " ++ show x
    Left _ -> pure ()
