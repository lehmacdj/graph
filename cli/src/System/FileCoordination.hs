{-# LANGUAGE CPP #-}

-- | This module provides a cross-platform file coordination API.
-- On macOS, it uses NSFileCoordinator to properly coordinate file access
-- with iCloud and other file presenters.
-- On other platforms, it provides a no-op implementation that directly
-- passes through to the provided actions.
module System.FileCoordination
  ( coordinateReading,
    coordinateWriting,
    coordinateReadingThenWriting,
    coordinateWritingAndWriting,
    FilesToCoordinate (..),
    WrappedReader (..),
    unwrappingReader,
    WrappedWriter (..),
    unwrappingWriter,
    coordinateAccessing,
    ReadingOptions (..),
    defaultReadingOptions,
    WritingOptions (..),
    defaultWritingOptions,
    NullResultException (..),
    NSErrorException (..),
    spec_coordinatedBodyThrows,
    spec_coordinatedPathKept,
    spec_peekNSError,
  )
where

import MyPrelude
import Utils.Testing.External

#ifdef darwin_HOST_OS

import Control.Exception qualified as E
import Control.Monad.Trans.Resource
import Foreign
import Foreign.C.String
import System.MacOS.NSFileCoordinator.RawBindings

#endif

-- * Common types shared between Darwin and non-Darwin implementations

-- | We don't provide forUploading, because there isn't a good way to use
-- it with the current API exposed by this module (and we probably won't because
-- it would require supporting concurrency).
data ReadingOptions = ReadingOptions
  { -- | This causes the file coordinator to skip notifying NSFilePresenters
    -- that they should save their changes.
    withoutChanges :: Bool,
    -- | This causes the file coordinator to resolve symbolic links.
    resolveSymbolicLinks :: Bool,
    -- | For files stored in iCloud, this prevents the file from being
    -- downloaded if it isn't already (some other metadata is also not
    -- downloaded, e.g. thumbnails)
    immediatelyAvailableMetadataOnly :: Bool
  }

defaultReadingOptions :: ReadingOptions
defaultReadingOptions =
  ReadingOptions
    { withoutChanges = False,
      resolveSymbolicLinks = False,
      immediatelyAvailableMetadataOnly = False
    }

data WritingOptions = WritingOptions
  { forDeleting :: Bool,
    forMoving :: Bool,
    forMerging :: Bool,
    forReplacing :: Bool,
    -- | If non metadata for the file is set it might not be saved.
    contentIndependentMetadataOnly :: Bool
  }

defaultWritingOptions :: WritingOptions
defaultWritingOptions =
  WritingOptions
    { forDeleting = False,
      forMoving = False,
      forMerging = False,
      forReplacing = False,
      contentIndependentMetadataOnly = False
    }

-- | This shouldn't be thrown unless something very fishy happens
data NullResultException = NullResultException
  deriving (Show, Exception)

-- | An error that file coordination gave: its domain and code as they are,
-- and what it means in words
data NSErrorException = NSErrorException
  { domain :: Text,
    code :: Int,
    description :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData, Exception)

#ifdef darwin_HOST_OS

-- * Utilities for Darwin

fromWritingOptions :: WritingOptions -> NSFileCoordinatorWritingOptions
fromWritingOptions WritingOptions {..} =
  [ justIfTrue forDeleting k_NSFileCoordinatorWritingForDeleting,
    justIfTrue forMoving k_NSFileCoordinatorWritingForMoving,
    justIfTrue forMerging k_NSFileCoordinatorWritingForMerging,
    justIfTrue forReplacing k_NSFileCoordinatorWritingForReplacing,
    justIfTrue contentIndependentMetadataOnly k_NSFileCoordinatorWritingContentIndependentMetadataOnly
  ]
    & catMaybes
    & foldl' (.|.) 0

fromReadingOptions :: ReadingOptions -> NSFileCoordinatorReadingOptions
fromReadingOptions ReadingOptions {..} =
  [ justIfTrue withoutChanges k_NSFileCoordinatorReadingWithoutChanges,
    justIfTrue resolveSymbolicLinks k_NSFileCoordinatorReadingResolvesSymbolicLink,
    justIfTrue immediatelyAvailableMetadataOnly k_NSFileCoordinatorReadingImmediatelyAvailableMetadataOnly
  ]
    & catMaybes
    & foldl' (.|.) 0

filePathFromNSURL :: Ptr NSURL -> IO FilePath
filePathFromNSURL url = do
  path <- p_NSURL_path url
  unpack <$> packNSString path

peekNSError :: Ptr NSError -> IO (Maybe NSErrorException)
peekNSError nsError = withEarlyReturnIO do
  when (nsError == nullPtr) $ returnEarly Nothing
  domain <- liftIO $ p_NSError_domain nsError >>= packNSString
  code <- liftIO $ p_NSError_code nsError
  description <- liftIO $ p_NSError_description nsError >>= packNSString
  pure . Just $ NSErrorException domain (fromIntegral code) description

throwNonNullNSError :: Ptr NSError -> IO ()
throwNonNullNSError nsError = peekNSError nsError >>= maybe (pure ()) throwIO

allocateNSURL :: FilePath -> Bool -> ResourceT IO (ReleaseKey, Ptr NSURL)
allocateNSURL path isDirectory = allocate
  -- I'm pretty sure nsURL_initFileURL copies the array so we don't need to
  -- worry about the array being deallocated before the NSURL is used
  (withCString path $ \pathPtr ->
    nsURL_initFileURL pathPtr (fromBool isDirectory))
  m_release

allocateNSArray :: Objc a => [Ptr a] -> ResourceT IO (ReleaseKey, Ptr NSArray)
allocateNSArray objects =
  allocate
    -- the NSArray copies the C array, so it may be freed once the NSArray is
    -- created
    (withArrayLen objects $ \len objectsPtr ->
      nsArray_initWithObjects (castPtr objectsPtr) (fromIntegral len))
    m_release

-- * Relatively raw darwin wrappers

-- | Where the body run by an accessor block leaves its result. The block is
-- a Haskell callback that Objective-C calls, and an exception can't pass
-- through the foreign frames: one that leaves the callback makes the runtime
-- end the process. So the callback catches everything and the caller of the
-- foreign function throws it again once that function has returned.
newtype AccessorResult a = AccessorResult (IORef (Maybe (Either SomeException a)))

newAccessorResult :: IO (AccessorResult a)
newAccessorResult = AccessorResult <$> newIORef Nothing

-- | Run the body of an accessor block; never throws.
runAccessorBody :: AccessorResult a -> IO a -> IO ()
runAccessorBody (AccessorResult ref) body =
  -- not the @try@ of MyPrelude: asynchronous exceptions must not leave the
  -- callback either
  E.try body >>= writeIORef ref . Just

-- | The result of the body, to be asked for once the foreign function that
-- was given the accessor block has returned.
accessorResult :: AccessorResult a -> IO a
accessorResult (AccessorResult ref) =
  readIORef ref >>= \case
    Nothing -> throwIO NullResultException
    Just (Left e) -> throwIO e
    Just (Right result) -> pure result

-- | Runs the action in an autorelease pool of its own, for the tests: the
-- Objective-C objects that it autoreleases are freed before its result is
-- returned, as they are sooner or later wherever it runs (when a pool
-- around it is left, or when its operating system thread ends).
inOwnAutoreleasePool :: IO a -> IO a
inOwnAutoreleasePool action = do
  result <- newAccessorResult
  inAutoreleasePool (runAccessorBody result action)
  accessorResult result

coordinateReading' ::
  Ptr NSFileCoordinator ->
  Ptr (Ptr NSError) ->
  Ptr NSURL ->
  ReadingOptions ->
  (FilePath -> IO a) ->
  IO a
coordinateReading' fileCoordinator errPtr url options action = do
  result <- newAccessorResult
  m_NSFileCoordinator_coordinateReadingItem
    fileCoordinator
    url
    (fromReadingOptions options)
    errPtr
    $ \newUrl ->
      runAccessorBody result $
        action =<< filePathFromNSURL newUrl
  when (errPtr /= nullPtr) $ peek errPtr >>= throwNonNullNSError
  accessorResult result

coordinateWriting' ::
  Ptr NSFileCoordinator ->
  Ptr (Ptr NSError) ->
  Ptr NSURL ->
  WritingOptions ->
  (FilePath -> IO a) ->
  IO a
coordinateWriting' fileCoordinator errPtr url options action = do
  result <- newAccessorResult
  m_NSFileCoordinator_coordinateWritingItem
    fileCoordinator
    url
    (fromWritingOptions options)
    errPtr
    $ \newUrl ->
      runAccessorBody result $
        action =<< filePathFromNSURL newUrl
  when (errPtr /= nullPtr) $ peek errPtr >>= throwNonNullNSError
  accessorResult result

coordinateReadingThenWriting' ::
  Ptr NSFileCoordinator ->
  Ptr (Ptr NSError) ->
  Ptr NSURL ->
  ReadingOptions ->
  Ptr NSURL ->
  WritingOptions ->
  (FilePath -> FilePath -> IO a) ->
  IO a
coordinateReadingThenWriting' fileCoordinator errPtr readingUrl readingOptions writingUrl writingOptions action = do
  result <- newAccessorResult
  m_NSFileCoordinator_coordinateReadingAndWritingItem
    fileCoordinator
    readingUrl
    (fromReadingOptions readingOptions)
    writingUrl
    (fromWritingOptions writingOptions)
    errPtr
    $ \newReadingUrl newWritingUrl -> runAccessorBody result do
      readingPath <- filePathFromNSURL newReadingUrl
      writingPath <- filePathFromNSURL newWritingUrl
      action readingPath writingPath
  when (errPtr /= nullPtr) $ peek errPtr >>= throwNonNullNSError
  accessorResult result

coordinateWritingAndWriting' ::
  Ptr NSFileCoordinator ->
  Ptr (Ptr NSError) ->
  Ptr NSURL ->
  WritingOptions ->
  Ptr NSURL ->
  WritingOptions ->
  (FilePath -> FilePath -> IO a) ->
  IO a
coordinateWritingAndWriting' fileCoordinator errPtr writingUrl1 writingOptions1 writingUrl2 writingOptions2 action = do
  result <- newAccessorResult
  m_NSFileCoordinator_coordinateWritingItem
    fileCoordinator
    writingUrl1
    (fromWritingOptions writingOptions1)
    errPtr
    $ \newWritingUrl1 -> runAccessorBody result do
      writingPath1 <- filePathFromNSURL newWritingUrl1
      coordinateWriting' fileCoordinator errPtr writingUrl2 writingOptions2 $
        action writingPath1
  when (errPtr /= nullPtr) $ peek errPtr >>= throwNonNullNSError
  accessorResult result

#endif

-- * Implementations (no-op on non-Darwin)

coordinateReading :: FilePath -> Bool -> ReadingOptions -> (FilePath -> IO a) -> IO a
#ifdef darwin_HOST_OS
coordinateReading path isDirectory options action = runResourceT do
  (_, fileCoordinator) <- allocate nsFileCoordinator_init m_release
  (_, errPtr) <- allocate calloc free
  (_, url) <- allocateNSURL path isDirectory
  liftIO $ coordinateReading' fileCoordinator errPtr url options action
#else
coordinateReading path _ _ action = action path
#endif

coordinateWriting :: FilePath -> Bool -> WritingOptions -> (FilePath -> IO a) -> IO a
#ifdef darwin_HOST_OS
coordinateWriting path isDirectory options action = runResourceT do
  (_, fileCoordinator) <- allocate nsFileCoordinator_init m_release
  (_, errPtr) <- allocate calloc free
  (_, url) <- allocateNSURL path isDirectory
  liftIO $ coordinateWriting' fileCoordinator errPtr url options action
#else
coordinateWriting path _ _ action = action path
#endif

coordinateReadingThenWriting :: FilePath -> Bool -> ReadingOptions -> FilePath -> Bool -> WritingOptions -> (FilePath -> FilePath -> IO a) -> IO a
#ifdef darwin_HOST_OS
coordinateReadingThenWriting readingPath readingIsDirectory readingOptions writingPath writingIsDirectory writingOptions action = runResourceT do
  (_, fileCoordinator) <- allocate nsFileCoordinator_init m_release
  (_, errPtr) <- allocate calloc free
  (_, readingUrl) <- allocateNSURL readingPath readingIsDirectory
  (_, writingUrl) <- allocateNSURL writingPath writingIsDirectory
  liftIO $ coordinateReadingThenWriting' fileCoordinator errPtr readingUrl readingOptions writingUrl writingOptions action
#else
coordinateReadingThenWriting readingPath _ _ writingPath _ _ action = action readingPath writingPath
#endif

coordinateWritingAndWriting :: FilePath -> Bool -> WritingOptions -> FilePath -> Bool -> WritingOptions -> (FilePath -> FilePath -> IO a) -> IO a
#ifdef darwin_HOST_OS
coordinateWritingAndWriting writingPath1 writingIsDirectory1 writingOptions1 writingPath2 writingIsDirectory2 writingOptions2 action = runResourceT do
  (_, fileCoordinator) <- allocate nsFileCoordinator_init m_release
  (_, errPtr) <- allocate calloc free
  (_, writingUrl1) <- allocateNSURL writingPath1 writingIsDirectory1
  (_, writingUrl2) <- allocateNSURL writingPath2 writingIsDirectory2
  liftIO $ coordinateWritingAndWriting' fileCoordinator errPtr writingUrl1 writingOptions1 writingUrl2 writingOptions2 action
#else
coordinateWritingAndWriting writingPath1 _ _ writingPath2 _ _ action = action writingPath1 writingPath2
#endif

data FilesToCoordinate t = FilesToCoordinate
  { readingPaths :: t (FilePath, Bool),
    blanketReadingOptions :: ReadingOptions,
    writingPaths :: t (FilePath, Bool),
    blanketWritingOptions :: WritingOptions
  }

newtype WrappedReader m = WrappedReader {unwrapped :: forall a. ReadingOptions -> (FilePath -> m a) -> m a}

unwrappingReader :: WrappedReader m -> ReadingOptions -> (FilePath -> m a) -> m a
unwrappingReader (WrappedReader unwrapped) = unwrapped

newtype WrappedWriter m = WrappedWriter {unwrapped :: forall a. WritingOptions -> (FilePath -> m a) -> m a}

unwrappingWriter :: WrappedWriter m -> WritingOptions -> (FilePath -> m a) -> m a
unwrappingWriter (WrappedWriter unwrapped) = unwrapped

-- | This only works up to ~448 files. When accessing more files, either for
-- reading or writing, the file coordinator has a tendency to segfault.
--
-- Just don't nest the wrappedreader/writer functions too deeply/call them too
-- concurrently and you should be fine.
coordinateAccessing ::
  (Traversable t) =>
  FilesToCoordinate t ->
  ( t (WrappedReader IO) ->
    t (WrappedWriter IO) ->
    IO a
  ) ->
  IO a
#ifdef darwin_HOST_OS
coordinateAccessing FilesToCoordinate{..} action = runResourceT do
    readingUrls <- for readingPaths $ \(path, isDirectory) -> do
      (_, url) <- allocateNSURL path isDirectory
      pure url
    writingUrls <- for writingPaths $ \(path, isDirectory) -> do
      (_, url) <- allocateNSURL path isDirectory
      pure url
    -- using ordNub allows Haskell caller's to not need to worry about
    -- specifying the same file multiple times
    (_, readingUrlsArray) <- allocateNSArray (ordNub $ readingUrls ^.. folded)
    (_, writingUrlsArray) <- allocateNSArray (ordNub $ writingUrls ^.. folded)
    (_, fileCoordinator) <- allocate nsFileCoordinator_init m_release
    (_, errPtr) <- allocate calloc free
    let readingAccessors =
          readingUrls <&> \url -> WrappedReader \readingOptions readingAction ->
            coordinateReading' fileCoordinator errPtr url readingOptions readingAction
    let writingAccessors =
          writingUrls <&> \url -> WrappedWriter \writingOptions writingAction ->
            coordinateWriting' fileCoordinator errPtr url writingOptions writingAction
    result <- liftIO newAccessorResult
    liftIO $
      m_NSFileCoordinator_prepareForReadingAndWritingItems
        fileCoordinator
        readingUrlsArray
        (fromReadingOptions blanketReadingOptions)
        writingUrlsArray
        (fromWritingOptions blanketWritingOptions)
        errPtr
        (runAccessorBody result $ action readingAccessors writingAccessors)
    liftIO $ accessorResult result
#else
coordinateAccessing FilesToCoordinate{..} action =
  let readingAccessors = readingPaths <&> \(path, _) -> WrappedReader \_ readingAction -> readingAction path
      writingAccessors = writingPaths <&> \(path, _) -> WrappedWriter \_ writingAction -> writingAction path
   in action readingAccessors writingAccessors
#endif

-- | An error from file coordination comes with what it means in words.
spec_peekNSError :: Spec
spec_peekNSError = do
#ifdef darwin_HOST_OS
  let peeked :: String -> Int -> IO (Maybe NSErrorException)
      peeked domain code = runResourceT do
        (_, nsError) <-
          allocate
            (withCString domain \d -> nsError_init d (fromIntegral code))
            m_release
        liftIO (peekNSError nsError)
      described domain code =
        fmap (\e -> (e.domain, e.code, e.description)) <$> peeked domain code
  it "describes a file error of Cocoa as the SDK documents it" do
    described "NSCocoaErrorDomain" 3072
      `shouldReturn` Just ("NSCocoaErrorDomain", 3072, "the operation was cancelled")
    described "NSCocoaErrorDomain" 257
      `shouldReturn` Just
        ( "NSCocoaErrorDomain",
          257,
          "the file could not be read because of a permission problem"
        )
  it "describes any other error as the error describes itself" do
    Just e <- peeked "NSPOSIXErrorDomain" 13
    (e.domain, e.code) `shouldBe` ("NSPOSIXErrorDomain", 13)
    e.description `shouldNotBe` ""
  it "gives texts that outlive the error" do
    -- the error and its strings are freed before the texts are looked at
    inOwnAutoreleasePool (described "an error domain that only this test has" 257)
      `shouldReturn` Just
        ( "an error domain that only this test has",
          257,
          "The operation couldn’t be completed. (an error domain that only this test has error 257.)"
        )
#else
  pure ()
#endif

-- | The path that a body is given is the body's to keep: it can be looked at
-- after the coordinated call has returned, and after the Objective-C string
-- that it was read from is freed.
spec_coordinatedPathKept :: Spec
spec_coordinatedPathKept = do
  let inTempFile :: (FilePath -> IO ()) -> IO ()
      inTempFile test = withSystemTempDirectory "coordination" \dir -> do
        writeFile (dir </> "file") "content"
        test (dir </> "file")
      -- the body returns its path without looking at it
      keeping :: FilePath -> IO FilePath
      keeping = pure
      keepingBoth :: FilePath -> FilePath -> IO (FilePath, FilePath)
      keepingBoth = curry pure
      -- what the call autoreleased is freed before its result is looked at
      afterwards :: (HasCallStack, Eq a, Show a) => IO a -> a -> IO ()
      afterwards call expected =
#ifdef darwin_HOST_OS
        inOwnAutoreleasePool call `shouldReturn` expected
#else
        call `shouldReturn` expected
#endif
  it "after reading" $ inTempFile \file ->
    coordinateReading file False defaultReadingOptions keeping
      `afterwards` file
  it "after writing" $ inTempFile \file ->
    coordinateWriting file False defaultWritingOptions keeping
      `afterwards` file
  it "after reading then writing" $ inTempFile \file ->
    coordinateReadingThenWriting
      file
      False
      defaultReadingOptions
      (file <> ".other")
      False
      defaultWritingOptions
      keepingBoth
      `afterwards` (file, file <> ".other")
  it "after writing two files" $ inTempFile \file ->
    coordinateWritingAndWriting
      file
      False
      defaultWritingOptions
      (file <> ".other")
      False
      defaultWritingOptions
      keepingBoth
      `afterwards` (file, file <> ".other")
  it "after accessing several files" $ inTempFile \file ->
    coordinateAccessing
      FilesToCoordinate
        { readingPaths = [(file, False)],
          blanketReadingOptions = defaultReadingOptions,
          writingPaths = [(file <> ".other", False)],
          blanketWritingOptions = defaultWritingOptions
        }
      ( \readers writers ->
          (,)
            <$> for readers (\r -> unwrappingReader r defaultReadingOptions keeping)
            <*> for writers (\w -> unwrappingWriter w defaultWritingOptions keeping)
      )
      `afterwards` ([file], [file <> ".other"])

-- | A body that throws must not end the process (on macOS the body runs in
-- a callback called from Objective-C): its caller gets the exception.
spec_coordinatedBodyThrows :: Spec
spec_coordinatedBodyThrows = do
  let throwing :: IO ()
      throwing = throwIO (userError "thrown by the body")
      isThrown :: IOError -> Bool
      isThrown = (== userError "thrown by the body")
      inTempFile :: (FilePath -> IO ()) -> IO ()
      inTempFile test = withSystemTempDirectory "coordination" \dir -> do
        writeFile (dir </> "file") "content"
        test (dir </> "file")
  it "passes on an exception thrown while reading" $ inTempFile \file ->
    coordinateReading file False defaultReadingOptions (const throwing)
      `shouldThrow` isThrown
  it "passes on an exception thrown while writing" $ inTempFile \file ->
    coordinateWriting file False defaultWritingOptions (const throwing)
      `shouldThrow` isThrown
  it "passes on an error call" $ inTempFile \file ->
    coordinateReading file False defaultReadingOptions (\_ -> error "boom" :: IO ())
      `shouldThrow` anyErrorCall
  it "passes on an exception thrown while reading then writing" $
    inTempFile \file ->
      coordinateReadingThenWriting
        file
        False
        defaultReadingOptions
        (file <> ".other")
        False
        defaultWritingOptions
        (\_ _ -> throwing)
        `shouldThrow` isThrown
  it "passes on an exception thrown while writing two files" $
    inTempFile \file ->
      coordinateWritingAndWriting
        file
        False
        defaultWritingOptions
        (file <> ".other")
        False
        defaultWritingOptions
        (\_ _ -> throwing)
        `shouldThrow` isThrown
  it "passes on an exception thrown while accessing several files" $
    inTempFile \file ->
      coordinateAccessing
        FilesToCoordinate
          { readingPaths = [(file, False)],
            blanketReadingOptions = defaultReadingOptions,
            writingPaths = [(file <> ".other", False)],
            blanketWritingOptions = defaultWritingOptions
          }
        (\_ _ -> throwing)
        `shouldThrow` isThrown
  it "passes on an exception thrown in a reader of several files" $
    inTempFile \file ->
      coordinateAccessing
        FilesToCoordinate
          { readingPaths = [(file, False)],
            blanketReadingOptions = defaultReadingOptions,
            writingPaths = [],
            blanketWritingOptions = defaultWritingOptions
          }
        ( \readers _ -> for_ readers \reader ->
            unwrappingReader reader defaultReadingOptions (const throwing)
        )
        `shouldThrow` isThrown
  it "still returns the result of a body that doesn't throw" $
    inTempFile \file ->
      coordinateReading file False defaultReadingOptions readFile
        `shouldReturn` "content"
