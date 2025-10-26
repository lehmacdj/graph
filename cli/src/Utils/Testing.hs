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
    goldenTestBinary,
    goldenTest,
    goldenTestShow,
  )
where

import DAL.Serialization (initializeGraph)
import Data.Aeson
import GHC.Stack (CallStack, SrcLoc (..), callStack, getCallStack)
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
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit as X
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.QuickCheck as X hiding (Fixed (..), label)
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

parseForTest ::
  (HasCallStack) => String -> Parser a -> Text -> IO a
parseForTest whatToParse parser input =
  case runParser (parser <* eof) ("<" <> whatToParse <> ">") input of
    Left err -> do
      expectationFailure $ "Failed to parse: " <> errorBundlePretty err
      error "unreachable"
    Right result -> pure result

testParserParses ::
  (Eq a, Show a, HasCallStack) => Parser a -> Text -> a -> Assertion
testParserParses parser string expected =
  case runParserTest parser string of
    Right actual -> actual @=? expected
    Left err ->
      assertFailure $
        ("expected: " ++ show expected ++ "\n")
          ++ ("but parser failed with:\n" ++ errorBundlePretty err)

testParserFails ::
  (Eq a, Show a, HasCallStack) => Parser a -> Text -> Assertion
testParserFails parser string =
  case runParserTest parser string of
    Right x ->
      assertFailure $
        "expected parser to fail, but it succeeded producing: " ++ show x
    Left _ -> pure ()

-- | Creates a golden test for binary data by comparing the ByteString with a
-- file in a subtree of test/ relative to the module the function is called from.
--
-- For example, a test defined in the module @Utils.Testing@ created like:
-- @goldenTestBinary "asdf" someValue@
-- would have its golden file located at @test/Utils/Testing/asdf@
--
-- The test name will be the basename of the test file (the first argument).
goldenTestBinary :: (HasCallStack) => String -> ByteString -> TestTree
goldenTestBinary testName actualBytes =
  goldenVsString testName (goldenPathFor callStack testName) (pure $ fromStrict actualBytes)

-- | Creates a golden test for Text by comparing the Text with a
-- file in a subtree of test/ relative to the module the function is called from.
--
-- For example, a test defined in the module @Models.Example@ created like:
-- @goldenTest "asdf" someText@
-- would have its golden file located at @test/Models/Example/asdf@
goldenTest :: (HasCallStack) => String -> Text -> TestTree
goldenTest testName actualText =
  goldenTestBinary testName (encodeUtf8 actualText)

-- | Creates a golden test for any showable value by comparing its shown representation
-- with a file in a subtree of test/ relative to the module the function is called from.
--
-- For example, a test defined in the module @Models.Example@ created like:
-- @goldenTestShow "asdf" someValue@
-- would have its golden file located at @test/Models/Example/asdf@
goldenTestShow :: (HasCallStack, Show a) => String -> a -> TestTree
goldenTestShow testName value =
  goldenTest testName (tshow value)

-- | Construct the golden file path for a test, based on the calling module
goldenPathFor :: CallStack -> String -> FilePath
goldenPathFor cs testName =
  let callingModule = getCallingModule cs
      modPath = map (\c -> if c == '.' then '/' else c) callingModule
   in "test" </> modPath </> testName

-- | Extract the module name from the call stack.
-- We skip entries that are from Utils.Testing itself to find the actual calling module.
getCallingModule :: CallStack -> String
getCallingModule cs =
  case filter (not . isUtilsTestingModule . srcLocModule . snd) (getCallStack cs) of
    (_, loc) : _ -> srcLocModule loc
    [] -> error "goldenTest: unable to determine calling module from callstack"
  where
    isUtilsTestingModule modName = modName == "Utils.Testing"
