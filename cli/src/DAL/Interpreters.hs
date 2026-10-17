module DAL.Interpreters where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Exception qualified as E
import DAL.RawGraph
import Error.Interrupt
import Error.UserError
import MyPrelude
import System.Console.Haskeline qualified as Haskeline
import System.FileCoordination
import Utils.Testing.External

type RawGraphUserErrorIO = [RawGraph, Error UserError, IOE]

-- | Function for "unlifting" a computation that uses the RawGraph effect
runRawGraphUserErrorIO ::
  FilePath ->
  Eff RawGraphUserErrorIO a ->
  IO (Either UserError a)
runRawGraphUserErrorIO path =
  runEff . runError . runRawGraphWithPath path

-- | Gives an action in 'IO' a way to run effects, and makes user errors of
-- its failures ('unliftedFailures').
withUnliftIORawGraphUserError ::
  (Members [RawGraph, Error UserError, IOE] r) =>
  (UnliftIO (Eff RawGraphUserErrorIO) -> IO a) ->
  Eff r a
withUnliftIORawGraphUserError f = do
  graphPath <- getGraphFilePath
  unliftedFailures $
    f
      ( UnliftIO $ \action -> do
          result <- runRawGraphUserErrorIO graphPath action
          either E.throw pure result
      )

-- | Runs an action that was given a way to run effects in 'IO', with the
-- two failures that it has as user errors:
--
-- - A 'UserError' that it throws: that is how an effect that it ran fails.
-- - An error of file coordination. The action is one that coordinates the
--   access to several files itself, so the error of preparing for them, or
--   of one of their readers and writers, is thrown in it and not in an
--   effect. It becomes the user error that it is when a single file is read
--   or written ('embedCatchingErrors').
--
-- Anything else that it throws goes on as it is, and so does what stops it
-- from outside (an interrupt, the end of a time-out: 'fromExceptionVia').
unliftedFailures ::
  (Members [Error UserError, IOE] r) => IO a -> Eff r a
unliftedFailures action =
  fromException @UserError $
    action `E.catch` \(e :: NSErrorException) ->
      E.throwIO (FileCoordinationError e)

withUnliftIORawGraphUserError' ::
  (Members [RawGraph, Error UserError, IOE] r) =>
  (forall x. m x -> Eff RawGraphUserErrorIO x) ->
  (UnliftIO m -> IO a) ->
  Eff r a
withUnliftIORawGraphUserError' extraInterpreters f = do
  graphPath <- getGraphFilePath
  unliftedFailures $
    f
      ( UnliftIO $ \action -> do
          result <- runRawGraphUserErrorIO graphPath $ extraInterpreters action
          either E.throw pure result
      )

spec_withUnliftIORawGraphUserError :: Spec
spec_withUnliftIORawGraphUserError = do
  let inTempDir :: (FilePath -> IO ()) -> IO ()
      inTempDir = withSystemTempDirectory "unlifting"
      unlifting ::
        FilePath ->
        (UnliftIO (Eff RawGraphUserErrorIO) -> IO a) ->
        IO (Either UserError a)
      unlifting dir f =
        runRawGraphUserErrorIO dir (withUnliftIORawGraphUserError f)
      files dir =
        FilesToCoordinate
          { readingPaths = [(dir </> "read", False)],
            blanketReadingOptions = defaultReadingOptions,
            writingPaths = [(dir </> "written", False)],
            blanketWritingOptions = defaultWritingOptions
          }
      -- what a single read or write shows when file coordination fails it
      shown = map show . either Just (const Nothing)
      asASingleAccessShows =
        Just
          ( "the system did not give access to a file of the graph"
              <> " (file-coordination-error): the operation was cancelled"
              <> " (NSCocoaErrorDomain 3072)"
          )
      -- a reader or writer of several files runs in a thread of its own, as
      -- the ones of a change to the graph do
      inAnotherThread :: IO a -> IO a
      inAnotherThread = async >=> wait
      -- as Ctrl-C, a signal or a time-out arrive: thrown at the thread
      -- while it runs the action
      arriving :: (Exception e) => e -> IO ()
      arriving e = myThreadId >>= (`E.throwTo` e)
  describe "makes the user error of file coordination of its error" do
    it "when preparing for several files fails" $ inTempDir \dir -> do
      ran <- newIORef False
      result <- unlifting dir \_ ->
        coordinateAccessingFailing FailingToPrepare (files dir) \_ _ ->
          writeIORef ran True
      shown result `shouldBe` asASingleAccessShows
      readIORef ran `shouldReturn` False
    it "when a reader among several files fails" $ inTempDir \dir -> do
      result <- unlifting dir \_ ->
        coordinateAccessingFailing FailingEachAccess (files dir) \readers _ ->
          for_ readers \reader ->
            inAnotherThread $
              unwrappingReader reader defaultReadingOptions (const (pure ()))
      shown result `shouldBe` asASingleAccessShows
    it "when a writer among several files fails" $ inTempDir \dir -> do
      result <- unlifting dir \_ ->
        coordinateAccessingFailing FailingEachAccess (files dir) \_ writers ->
          for_ writers \writer ->
            inAnotherThread $
              unwrappingWriter writer defaultWritingOptions (const (pure ()))
      shown result `shouldBe` asASingleAccessShows
  it "gives the error of an effect that the action ran as it is" $
    inTempDir \dir -> do
      result <- unlifting dir \(UnliftIO run) ->
        coordinateAccessing (files dir) \readers _ ->
          for_ readers \reader ->
            inAnotherThread $
              unwrappingReader reader defaultReadingOptions \_ ->
                run (throwText "of the effect")
      shown result `shouldBe` Just "of the effect"
  describe "passes on what stops the action from outside" do
    it "the Ctrl-C of the REPL" $ inTempDir \dir ->
      unlifting dir (\_ -> arriving Haskeline.Interrupt)
        `shouldThrow` \Haskeline.Interrupt -> True
    it "an interrupt signal" $ inTempDir \dir ->
      unlifting dir (\_ -> arriving E.UserInterrupt)
        `shouldThrow` (== E.UserInterrupt)
    it "the killing of the thread" $ inTempDir \dir ->
      unlifting dir (\_ -> arriving E.ThreadKilled)
        `shouldThrow` (== E.ThreadKilled)
    it "a time-out" $ inTempDir \dir -> do
      -- the action would take a minute
      result <- timeout 10000 (unlifting dir (\_ -> threadDelay 60000000))
      void result `shouldBe` Nothing
