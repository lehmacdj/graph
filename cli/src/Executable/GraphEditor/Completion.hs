-- | Completion stuff for the interface
module Executable.GraphEditor.Completion where

import Control.Arrow
import Control.Lens
import DAL.Serialization (nodesWithPrefix, serializeNodeEx)
import Effect.IOWrapper.GetTime
import Effectful.Provider.List
import Effectful.Readline (runReadlineNonInteractive)
import Error.UserError
import Error.Warn
import Graph.AppInterpreters
import Graph.GraphEditing
import Graph.NodeLocated
import Graph.PathPattern.Execution (completionTransitions)
import Models.Connect
import Models.LegacyPartialPath
import Models.NID
import Models.Node
import Models.Path.Simple qualified as Simple
import Models.PathPattern.Compile (checkAlternatives)
import MyPrelude
import System.Console.Haskeline
import System.Random (mkStdGen)
import Utils.Base62 (isBase62Char)
import Utils.Parsing
import Utils.Testing

commands :: [String]
commands = []

-- | takes a list of strings and a reversed input string, and produces
-- words from the list that complete the previous thing
mkCompleter :: [String] -> String -> [Completion]
mkCompleter xs x = map simpleCompletion' $ filter (x `isPrefixOf`) xs

getCommandCompletions :: String -> String -> IO [Completion]
getCommandCompletions x y
  -- unless the previous text is "" then we want to fail to provide any
  -- completions so that we fall back to another completion provider
  | y == "" = pure . mkCompleter commands . reverse $ x
  | otherwise = pure []

completeCommand :: (String, String) -> IO (String, [Completion])
completeCommand = completeWordWithPrev Nothing " " getCommandCompletions

quoteUnusualTransition :: String -> String
quoteUnusualTransition x
  | isUnusualTransition x = show x
  | otherwise = x

isUnusualTransition :: String -> Bool
isUnusualTransition = not . all isIdentChar

-- | simpleCompletion except the completion is not treated as finished,
-- thus no space is added after
simpleCompletion' :: String -> Completion
simpleCompletion' x = Completion x x False

-- TODO: fix this so that it is possible to complete transitions that use
-- special characters
-- currently, when completing something like @Hello/world/"!|@ (with the cursor
-- indicated by @|@, it isn't possible to complete a transition like @!!!@
-- because we don't appear to generate any options. Probably the partial
-- path parser needs to be improved to fix this.
mkTransitionCompleter :: [String] -> String -> [Completion]
mkTransitionCompleter xs x =
  mkCompleter (xs >>= validCompletions) x
  where
    validCompletions z
      | isUnusualTransition z = [show z]
      | x `isPrefixOf` z = [z]
      | otherwise = [show z, z]

failCompletionWithOriginalInputOnErrorOrWarning ::
  (IOE :> effs) =>
  -- | The original input
  String ->
  Eff (Warn UserError : Error UserError : effs) (String, [Completion]) ->
  Eff effs (String, [Completion])
failCompletionWithOriginalInputOnErrorOrWarning i =
  printWarnings
    >>> (`handleError` const (pure (i, [])))

-- this tries to parse as much of a path as possible, and then deduce what
-- the current word being completed is, it then tries to complete a
-- transition currently on as much as possible, then tries to complete from
-- that location
completePath :: Env -> (String, String) -> IO (String, [Completion])
completePath =
  completePathHandling (\i -> failCompletionWithOriginalInputOnErrorOrWarning i)

-- | 'completePath' with a choice of what becomes of the errors and warnings
-- of reading the graph, given the original input.
--
-- Damage that completing reads past is not among the warnings: only a
-- command line warns of it ('warningOfDamageReadPast'), which is what the
-- user runs next. A warning here would be written into the line that the
-- user is editing.
completePathHandling ::
  (String -> ErrorHandlingBehavior (String, [Completion])) ->
  Env ->
  (String, String) ->
  IO (String, [Completion])
completePathHandling handling env (i, _) =
  case getLegacyPartialPath (pack $ takeRelevantFromEnd i) of
    Nothing -> pure (i, [])
    Just (MissingSlash _) -> pure (i, [simpleCompletion "/"])
    Just (LegacyPartialPath pp end) ->
      runAppEffects
        runReadlineNonInteractive
        (handling i)
        interpretTimeAsIO
        filesystemBehaviorDryRun
        env
        $ do
          let p = case pp of
                [] -> Simple.One
                p0 : rest -> foldl' (Simple.:/) p0 rest
          nid <- currentLocation
          -- a prefix beyond its alternatives limit completes to nothing,
          -- rather than hanging on TAB
          octs <- case checkAlternatives p of
            Left _ -> pure []
            Right () ->
              map unpack . toList
                <$> provideList_ @GraphEditing (completionTransitions nid p)
          pure (fromMaybe i (stripPrefix (reverse end) i), mkCompleter octs end)

completeNID :: Env -> (String, String) -> IO (String, [Completion])
completeNID env (i, _) = do
  let (reversedNidPrefix, scraps) = span isBase62Char . take (nidDigits + 1) $ i
  case scraps of
    '@' : _ -> do
      base <- readIORef $ env ^. #filePath
      nids <- nodesWithPrefix base (reverse reversedNidPrefix)
      pure (fromMaybe i (stripPrefix reversedNidPrefix i), map (simpleCompletion' . show) nids)
    _ -> pure (i, [])

spec_completePath :: Spec
spec_completePath =
  it "completes past damage without a warning or an error" $
    withEmptyTempGraph \dir -> do
      -- the origin -b-> 1 -d-> 2, where the node 1 doesn't record b
      let n1 = smallNID 1
          n2 = smallNID 2
          write :: Node Text () -> IO ()
          write n = serializeNodeEx (n $> Nothing) dir
      write $ emptyNode nilNID & #outgoing .~ singletonSet (Connect "b" n1)
      write $ emptyNode n1 & #outgoing .~ singletonSet (Connect "d" n2)
      write $ emptyNode n2 & #incoming .~ singletonSet (Connect "d" n1)
      env <- initEnv dir (mkStdGen 0) defaultSettings
      said <- newIORef []
      let note :: (IOE :> r) => UserError -> Eff r ()
          note e = liftIO (modifyIORef said (show e :))
          recording :: String -> ErrorHandlingBehavior (String, [Completion])
          recording _ =
            interpret (\_ (Warn e) -> note e)
              >>> (`handleError` \e -> note e $> ("", []))
      -- the input is given reversed, as haskeline gives it
      (_, completions) <- completePathHandling recording env (reverse "ls b/", "")
      map replacement completions `shouldBe` ["d"]
      readIORef said `shouldReturn` []

completionFunction :: Env -> (String, String) -> IO (String, [Completion])
completionFunction env =
  completeCommand
    `fallbackCompletion` completeNID env
    `fallbackCompletion` completePath env
