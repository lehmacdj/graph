-- | Completion stuff for the interface
module Executable.GraphEditor.Completion where

import Control.Arrow
import Control.Concurrent (forkIO, forkIOWithUnmask, killThread, threadDelay)
import Control.Exception qualified as E
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
import Models.PathPattern.Compile (checkCopies)
import MyPrelude
import System.Console.Haskeline
import System.Random (mkStdGen)
import Utils.Base62 (isBase62Char)
import Utils.Parsing
import Utils.Testing
import Utils.Testing.TempGraph

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
          -- a prefix that no command runs completes to nothing; an @ that
          -- does nothing is not warned about here. Nothing here bounds how
          -- long a large prefix takes to compile
          -- (docs/path-cost-limits.md): 'completionFunction' gives up on
          -- one that isn't done in time ('completingWithin').
          octs <- case checkCopies p of
            Right _ ->
              map unpack . toList
                <$> provideList_ @GraphEditing (completionTransitions nid p)
            Left _ -> pure []
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
spec_completePath = do
  it "offers nothing for a path with a directive" $
    withEmptyTempGraph \dir -> do
      -- the origin -a-> 1 and -b-> 2, each consistent
      let n1 = smallNID 1
          n2 = smallNID 2
          write :: Node Text () -> IO ()
          write n = serializeNodeEx (n $> Nothing) dir
      write $
        emptyNode nilNID
          & #outgoing .~ setFromList [Connect "a" n1, Connect "b" n2]
      write $ emptyNode n1 & #incoming .~ singletonSet (Connect "a" nilNID)
      write $ emptyNode n2 & #incoming .~ singletonSet (Connect "b" nilNID)
      env <- initEnv dir (mkStdGen 0) defaultSettings
      let offered line =
            map replacement . snd
              <$> completePath env (reverse line, "")
      -- what is offered without a directive, for comparison
      offered "ls " `shouldReturn` ["a", "b"]
      for_ ["ls %last/a", "ls %history(0)/a", "ls a/%last/a", "ls %last/"] \line ->
        offered line `shouldReturn` []
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

-- | How long TAB may hold the prompt, in microseconds: about half a second.
completionAllowance :: Int
completionAllowance = 500000

-- | A completion function that offers nothing when its candidates are not
-- ready within the given time (in microseconds), instead of holding the
-- prompt until they are.
--
-- Nothing limits what a path compiles to (docs/path-cost-limits.md), so
-- compiling the prefix that is completed can take hours or never end. And
-- reading a node file waits for as long as the system keeps it from being
-- read: while another program writes it, while it is downloaded. While a
-- completion function runs the prompt reads nothing. Completion is advice,
-- so it is given up instead; this bounds no command.
--
-- The work is done in a thread of its own, and the caller only waits: for
-- the result, for the end of the time or for an interrupt (Ctrl-C while
-- completing), whichever is first. Waiting can always be ended. The work
-- can't: no exception reaches a thread that is in a call to the system,
-- as a coordinated read is, until the call returns. So when the caller
-- stops waiting the work is told to stop and is not waited for
-- ('abandoning'). Work that computes stops at once, as a command does at
-- Ctrl-C. Work that is in such a call stops when the call returns, which
-- is also when the call gives up what it held, and does nothing more. What
-- it would have answered is dropped, and so is what it throws. It cannot
-- change what the next command or the next completion finds either:
-- completion reads the graph and writes nothing.
--
-- The candidates are evaluated in full by the work, so that none of it is
-- left to whoever looks at them.
--
-- The thread of the haskeline session, which calls this, has asynchronous
-- exceptions masked (the REPL starts the session while it acquires it, and
-- a thread starts as masked as the one that forks it). Waiting is ended by
-- an exception even so; the work is unmasked explicitly, or it could not
-- be stopped while it computes.
completingWithin ::
  Int ->
  ((String, String) -> IO (String, [Completion])) ->
  (String, String) ->
  IO (String, [Completion])
completingWithin microseconds complete input@(i, _) = E.mask \restore -> do
  done <- newEmptyMVar
  work <- forkIOWithUnmask \unmask ->
    E.try @SomeException
      (unmask (complete input >>= evaluate . evaluatedCompletions))
      >>= putMVar done
  -- 'readMVar': a wait that takes nothing loses nothing when its time ends
  -- as the result arrives
  outcome <-
    restore (timeout microseconds (readMVar done))
      `E.onException` abandoning work
  case outcome of
    Just (Right result) -> pure result
    -- what the work itself threw, as when it was the caller's own
    Just (Left e) -> throwIO e
    Nothing -> abandoning work $> (i, [])
  where
    -- 'killThread' returns when the thread has got the exception, which a
    -- thread that is in a call to the system only does when the call
    -- returns: so that is waited for by a thread made for it
    abandoning work = void (forkIO (killThread work))

-- | What a completion function returned, with nothing left to evaluate.
evaluatedCompletions :: (String, [Completion]) -> (String, [Completion])
evaluatedCompletions result@(rest, completions) =
  rnf rest `seq` rnf (map fields completions) `seq` result
  where
    fields c = (replacement c, display c, isFinished c)

completionFunction :: Env -> (String, String) -> IO (String, [Completion])
completionFunction env =
  completingWithin completionAllowance $
    completeCommand
      `fallbackCompletion` completeNID env
      `fallbackCompletion` completePath env

spec_completionFunction :: Spec
spec_completionFunction = do
  -- the origin -b-> 1 -d-> 2; the input is given reversed, as haskeline
  -- gives it
  let withGraph test = withEmptyTempGraph \dir -> do
        let n1 = smallNID 1
            n2 = smallNID 2
            write :: Node Text () -> IO ()
            write n = serializeNodeEx (n $> Nothing) dir
            reached :: Node Text ()
            reached = emptyNode n1 & #incoming .~ singletonSet (Connect "b" nilNID)
        write $ emptyNode nilNID & #outgoing .~ singletonSet (Connect "b" n1)
        write $ reached & #outgoing .~ singletonSet (Connect "d" n2)
        write $ emptyNode n2 & #incoming .~ singletonSet (Connect "d" n1)
        test =<< initEnv dir (mkStdGen 0) defaultSettings
      -- one union that 3 ^ 7 routes go through: 2 ^ 2187 ways to choose
      -- its sides, which no machine lists
      endless =
        "(x | y)" <> concat (replicate 7 "/(a & b & c)" :: [String])
      replacements = fmap \(rest, completions) ->
        (rest, map replacement completions)
      -- A candidate that is returned at once and takes far longer to
      -- evaluate than a completion is given, in constant space, and that
      -- is done at some point. What a failing test abandons must not fill
      -- the memory (a list without end that is held did) or run for good.
      slowCandidate :: (String, String) -> IO (String, [Completion])
      slowCandidate _ =
        pure ("", [simpleCompletion (show (foldl' (+) 0 [1 .. 20000000000 :: Int]))])
      -- The haskeline session calls the completion function in a thread
      -- that has asynchronous exceptions masked (the REPL starts the
      -- session while it acquires it), so that is how it is called here.
      -- The allowance is for a busy machine, where half a second can come
      -- to several; it measures nothing.
      asTheSessionCalls :: IO a -> IO (Maybe a)
      asTheSessionCalls call = do
        calling <- async (mask_ call)
        timeout 60000000 (wait calling)
  it "completes a prefix that compiles at once" $ withGraph \env -> do
    result <- asTheSessionCalls (completionFunction env (reverse "ls b/", ""))
    replacements result `shouldBe` Just (reverse "ls b/", ["d"])
  it "offers nothing after a prefix that does not finish compiling" $
    withGraph \env -> do
      let input = (reverse ("ls " <> endless <> "/"), "")
          -- The compiling ends by itself here after ten seconds, and the
          -- completion is not called masked: if a mistake left the work
          -- running or unstoppable, it would fill the memory for as long as
          -- the tests run (23 GB in a minute, once). That the work is
          -- stopped when it is called masked is for the cases below, whose
          -- work is harmless.
          stopping i =
            timeout 10000000 (completePath env i >>= evaluate . evaluatedCompletions)
              >>= maybe (throwIO (userError "the work was not stopped")) pure
          complete = completingWithin completionAllowance stopping
      -- without the bound it is still at it after a second
      unbounded <- timeout 1000000 (stopping input)
      replacements unbounded `shouldBe` Nothing
      result <- timeout 60000000 (complete input)
      replacements result `shouldBe` Just (fst input, [])
      -- and the next completion is as it would have been
      next <- asTheSessionCalls (completionFunction env (reverse "ls b/", ""))
      replacements next `shouldBe` Just (reverse "ls b/", ["d"])
  it "gives up on candidates that are not evaluated in time" do
    result <-
      asTheSessionCalls $
        completingWithin 100000 slowCandidate ("sl", "")
    replacements result `shouldBe` Just ("sl", [])
  it "runs the work unmasked, so that it can be stopped" do
    let masking _ = do
          state <- E.getMaskingState
          pure (show state, [])
    result <-
      asTheSessionCalls (completingWithin 60000000 masking ("sl", ""))
    fmap fst result `shouldBe` Just (show E.Unmasked)
  -- Work that no exception reaches where it is, as a read that waits in a
  -- call to the system for another writer or for a download: it is there
  -- until the gate opens, would then go on for good, and says when it has
  -- ended.
  let withBlockedWork ::
        (((String, String) -> IO (String, [Completion])) -> MVar () -> IO ()) ->
        IO ()
      withBlockedWork test = do
        gate <- newEmptyMVar
        reached <- newEmptyMVar
        ended <- newEmptyMVar
        let blocked _ =
              ( do
                  putMVar reached ()
                  uninterruptibleMask_ (readMVar gate)
                  forever (threadDelay 1000)
              )
                `finally` putMVar ended ()
        -- while the gate is closed
        test blocked reached `finally` putMVar gate ()
        -- once the work is let go, it ends instead of going on
        timeout 60000000 (readMVar ended) `shouldReturn` Just ()
      -- thrown by a thread of its own: the throw only returns once the
      -- exception has arrived
      interrupting calling = void (forkIO (E.throwTo (asyncThreadId calling) Interrupt))
      interrupted = either (isJust . E.fromException @Interrupt) (const False)
  it "does not wait for work that cannot be stopped where it is" $
    withBlockedWork \blocked _ -> do
      result <-
        asTheSessionCalls (completingWithin 100000 blocked ("sl", ""))
      replacements result `shouldBe` Just ("sl", [])
  it "passes an interrupt on without waiting for such work" $
    withBlockedWork \blocked reached -> do
      -- with an hour to complete in: only the interrupt ends the wait
      calling <-
        async (mask_ (completingWithin 3600000000 blocked ("sl", "")))
      timeout 60000000 (readMVar reached) `shouldReturn` Just ()
      interrupting calling
      outcome <- timeout 60000000 (waitCatch calling)
      fmap interrupted outcome `shouldBe` Just True
