{-# LANGUAGE TemplateHaskell #-}

module DAL.RawGraph where

import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Exception qualified as E
import Effectful.Dispatch.Static (unsafeEff_)
import MyPrelude
import System.Posix.Files (fileID, fileSize, getFileStatus, modificationTimeHiRes)
import Utils.Testing.External

-- | Effect for the graph as files: the filepath of the graph, useful for
-- doing low level operations together with Graph.Serialize* modules, and
-- what is kept of the changes that the program makes to those files.
data RawGraph :: Effect where
  GetGraphFilePath :: RawGraph m FilePath
  GetGraphWrites :: RawGraph m GraphWrites

-- | What is kept of the changes that a program makes to the files of a
-- graph, where it makes them:
--
-- * How many files it has changed (written, copied or removed). Whoever
--   must say whether something was written compares the number before and
--   after ('graphFilesChanged'): an interrupted command line says what it
--   left behind by that, and not by what its commands are known to do.
-- * A lock that is held while files are being changed, so that the program
--   can end without cutting a change short ('noMoreGraphChanges').
-- * Which of the changes the user has been told of when a command line was
--   last stopped ('untoldGraphChanges'): the ones from the first of these
--   counts on, as they were when the count was the second. And the same of
--   the targets that @at@ named ('unnamedAtTargetChanges').
data GraphWrites = GraphWrites
  { changing :: MVar (),
    changed :: IORef Int,
    told :: IORef (Int, Int),
    named :: IORef (Int, Int)
  }

makeEffect ''RawGraph

newGraphWrites :: IO GraphWrites
newGraphWrites =
  GraphWrites <$> newMVar () <*> newIORef 0 <*> newIORef (0, 0) <*> newIORef (0, 0)

-- | Run what changes files of the graph and belongs together (the files of
-- one change): 'noMoreGraphChanges' waits for it to be over, and no
-- exception from outside (an interrupt, the end of the program at a signal)
-- is delivered before it is, so that it is never cut short between two of
-- its files. (Where the files are written through file coordination, on
-- macOS, none would be anyway: the thread that runs the change waits in a
-- call to the system. Elsewhere such an exception stopped the threads that
-- write the files in between two files.) The files that it changes are
-- counted one by one, with 'graphFileChanged'.
changingGraphFiles :: GraphWrites -> IO a -> IO a
changingGraphFiles writes = withMVar writes.changing . const . E.uninterruptibleMask_

-- | A file of the graph was changed. To be said where it was, in such a way
-- that nothing can stop the thread between the change and this
-- ('changingGraphFile', 'countingGraphFile'): an interrupt that arrives
-- while a change is written is delivered as soon as that returns, and the
-- change must be on record by then.
graphFileChanged :: GraphWrites -> IO ()
graphFileChanged writes = atomicModifyIORef' writes.changed \n -> (n + 1, ())

-- | Run what changes one file of the graph: a change of its own, which is
-- counted when it has been made. No exception from outside is delivered
-- from the start of the change until it is counted. (The change is a few
-- calls to the system, and none is delivered inside of those anyway.)
changingGraphFile :: GraphWrites -> IO a -> IO a
changingGraphFile writes change =
  changingGraphFiles writes . E.uninterruptibleMask_ $
    change <* graphFileChanged writes

-- | Count the file that an action changes, when it has, like
-- 'changingGraphFile': for an action that runs while 'changingGraphFiles'
-- holds the lock for it and others.
countingGraphFile :: (IOE :> es) => GraphWrites -> Eff es a -> Eff es a
countingGraphFile writes change =
  uninterruptibleMask_ (change <* liftIO (graphFileChanged writes))

-- | Run what has a file of the graph changed by another program (the editor
-- of @vi@ writes a node's data file): it counts as a change of that file
-- when the file is not as it was before (its time of modification, its size
-- or which file it is), also when the action is stopped from outside, since
-- the other program may have written the file by then.
changingGraphFileOutside ::
  (IOE :> es) => GraphWrites -> FilePath -> Eff es a -> Eff es a
changingGraphFileOutside writes path action = do
  was <- liftIO fileState
  action `finally` liftIO do
    now <- fileState
    when (now /= was) (graphFileChanged writes)
  where
    fileState =
      ( Just . (\s -> (modificationTimeHiRes s, fileSize s, fileID s))
          <$> getFileStatus path
      )
        `E.catch` \(_ :: IOException) -> pure Nothing

-- | How many files of the graph the program has changed so far.
graphFilesChanged :: (Members [RawGraph, IOE] es) => Eff es Int
graphFilesChanged = getGraphWrites >>= \writes -> readIORef writes.changed

-- | For whoever tells the user what a command line had changed when it was
-- stopped: how many files were changed since the count was the given one
-- that nobody has told the user of at this stop. They count as told from
-- now on. So when the parts of a line each say what they know (@at@ its
-- targets, a @;@ line its commands, the line as a whole that something was
-- changed), the inner ones first, every change is told of once.
untoldGraphChanges :: (Members [RawGraph, IOE] es) => Int -> Eff es Int
untoldGraphChanges = untoldIn (.told)

-- | 'untoldGraphChanges' for what @at@ says of its targets, kept apart: how
-- many of the changes since the given count no @at@ has named a target of at
-- this stop. So an @at@ around another one does not name again the target
-- that the inner one has named (@at (a | b) at \@ …@ named a twice), while
-- what the rest of a line says (that a command had run) does not name a
-- target and does not keep @at@ from naming it.
unnamedAtTargetChanges :: (Members [RawGraph, IOE] es) => Int -> Eff es Int
unnamedAtTargetChanges = untoldIn (.named)

untoldIn ::
  (Members [RawGraph, IOE] es) => (GraphWrites -> IORef (Int, Int)) -> Int -> Eff es Int
untoldIn record since = do
  writes <- getGraphWrites
  now <- readIORef writes.changed
  (from, upTo) <- readIORef (record writes)
  -- what was told at an earlier stop is not about this one
  let toldFrom = if upTo == now then from else now
  writeIORef (record writes) (min since toldFrom, now)
  pure (max 0 (min now toldFrom - since))

-- | Wait for the change that is being written, if one is, and let no other
-- start: for a program that is about to end and must not leave a change
-- half written. Whatever goes on to change the graph afterwards waits for
-- good.
noMoreGraphChanges :: GraphWrites -> IO ()
noMoreGraphChanges writes = takeMVar writes.changing

runRawGraphWithPath :: FilePath -> Eff (RawGraph : r) a -> Eff r a
runRawGraphWithPath p action = do
  -- a record of its own, which needs no IOE of whoever runs this
  writes <- unsafeEff_ newGraphWrites
  runRawGraphWith (pure p) writes action

-- | Interpret 'RawGraph' given how the path is found and the record of the
-- changes.
runRawGraphWith ::
  Eff r FilePath -> GraphWrites -> Eff (RawGraph : r) a -> Eff r a
runRawGraphWith path writes = interpret \_ -> \case
  GetGraphFilePath -> path
  GetGraphWrites -> pure writes

spec_graphWrites :: Spec
spec_graphWrites = do
  it "counts a file when it has been changed" do
    writes <- newGraphWrites
    changingGraphFile writes (pure ())
    changingGraphFile writes (pure ())
    readIORef writes.changed `shouldReturn` 2
  it "does not count a change that fails" do
    writes <- newGraphWrites
    changingGraphFile writes (throwIO (userError "disk on fire"))
      `shouldThrow` (== userError "disk on fire")
    readIORef writes.changed `shouldReturn` 0
  it "has a change on record before an interrupt that arrived in it is delivered" do
    -- the interrupt arrives while the change is made and is delivered when
    -- the change is over: by then it is counted
    writes <- newGraphWrites
    me <- myThreadId
    let change = void (forkIO (E.throwTo me E.UserInterrupt)) >> threadDelay 20000
    E.try (changingGraphFile writes change >> threadDelay 60000000)
      `shouldReturn` Left E.UserInterrupt
    readIORef writes.changed `shouldReturn` 1
  it "lets an interrupt in only when all the files of a change are written" do
    -- as when they are written by several threads at once, without file
    -- coordination, which waits in a call to the system; an interrupt that
    -- stopped those threads left a change half written
    writes <- newGraphWrites
    started <- newEmptyMVar
    let change =
          changingGraphFiles writes $ pooledForConcurrentlyN_ 4 [1 :: Int .. 40] \_ -> do
            void (tryPutMVar started ())
            threadDelay 5000
            graphFileChanged writes
    changer <- async change
    readMVar started
    cancel changer
    readIORef writes.changed `shouldReturn` 40
  let changes :: Int -> Eff [RawGraph, IOE] ()
      changes n = do
        writes <- getGraphWrites
        replicateM_ n (liftIO (changingGraphFile writes (pure ())))
      run :: Eff [RawGraph, IOE] a -> IO a
      run = runEff . runRawGraphWithPath "unused"
  it "tells of each change once, the inner part of a line first" $ run do
    outer <- graphFilesChanged
    changes 2
    inner <- graphFilesChanged
    changes 3
    liftIO . (`shouldBe` (3, 2, 0)) =<< do
      (,,)
        <$> untoldGraphChanges inner
        <*> untoldGraphChanges outer
        <*> untoldGraphChanges outer
  it "tells of nothing when nothing was changed" $ run do
    changes 2
    since <- graphFilesChanged
    liftIO . (`shouldBe` 0) =<< untoldGraphChanges since
  it "tells of what was changed after an earlier stop" $ run do
    first <- graphFilesChanged
    changes 2
    liftIO . (`shouldBe` 2) =<< untoldGraphChanges first
    second <- graphFilesChanged
    changes 1
    liftIO . (`shouldBe` 1) =<< untoldGraphChanges second
    -- and of nothing at a stop of a line that changed nothing
    third <- graphFilesChanged
    liftIO . (`shouldBe` 0) =<< untoldGraphChanges third
  it "waits for a change in progress before no more are let in" do
    writes <- newGraphWrites
    started <- newEmptyMVar
    finish <- newEmptyMVar
    finished <- newIORef False
    changer <-
      async . changingGraphFiles writes $
        putMVar started () >> readMVar finish >> writeIORef finished True
    readMVar started
    ending <- async (noMoreGraphChanges writes >> readIORef finished)
    -- it is still waiting while the change is being made
    timeout 100000 (wait ending) `shouldReturn` Nothing
    putMVar finish ()
    timeout 60000000 (wait ending) `shouldReturn` Just True
    wait changer
    -- and a change that starts afterwards does not get to change anything
    late <- async (changingGraphFile writes (pure ()))
    timeout 100000 (wait late) `shouldReturn` Nothing
    cancel late
    readIORef writes.changed `shouldReturn` 0
