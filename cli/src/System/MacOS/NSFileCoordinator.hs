{-# LANGUAGE CPP #-}

-- | This module provides a high-level interface to the NSFileCoordinator API.
-- Though it is only available on macOS, this module provides a cross-platform
-- API by providing a no-op implementation on non-macOS platforms.
module System.MacOS.NSFileCoordinator
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
  )
where

import MyPrelude

#ifdef darwin_HOST_OS

import Control.Monad.Trans.Cont
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

data NSErrorException = NSErrorException
  { domain :: Text,
    code :: Int
  }
  deriving (Show, Exception)

#ifdef darwin_HOST_OS

-- * Utilities for Darwin

withNSURL :: FilePath -> Bool -> (Ptr NSURL -> IO a) -> IO a
withNSURL path isDirectory action = withCString path $ \pathPtr ->
  bracket (nsURL_initFileURL pathPtr (fromBool isDirectory)) m_release action

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

peekNSError :: Ptr NSError -> IO NSErrorException
peekNSError nsError = do
  domain <- p_NSError_domain nsError >>= packNSString
  code <- p_NSError_code nsError
  pure $ NSErrorException domain (fromIntegral code)

throwNSError :: Ptr NSError -> IO a
throwNSError nsError = peekNSError nsError >>= throwIO

withNSArray :: Objc a => [Ptr a] -> (Ptr NSArray -> IO b) -> IO b
withNSArray objects action = withArrayLen objects $ \len objectsPtr ->
  bracket
    (nsArray_initWithObjects (castPtr objectsPtr) (fromIntegral len))
    m_release
    action

-- * Relatively raw darwin wrappers

coordinateReading' ::
  Ptr NSFileCoordinator ->
  Ptr (Ptr NSError) ->
  Ptr NSURL ->
  ReadingOptions ->
  (FilePath -> IO a) ->
  IO a
coordinateReading' fileCoordinator errPtr url options action = do
  resultRef <- newIORef Nothing
  m_NSFileCoordinator_coordinateReadingItem
    fileCoordinator
    url
    (fromReadingOptions options)
    errPtr
    $ \newUrl -> do
      path <- filePathFromNSURL newUrl
      result <- action path
      writeIORef resultRef (Just result)
  when (errPtr /= nullPtr) $ peek errPtr >>= throwNSError
  readIORef resultRef >>= maybe (throwIO NullResultException) pure

coordinateWriting' ::
  Ptr NSFileCoordinator ->
  Ptr (Ptr NSError) ->
  Ptr NSURL ->
  WritingOptions ->
  (FilePath -> IO a) ->
  IO a
coordinateWriting' fileCoordinator errPtr url options action = do
  resultRef <- newIORef Nothing
  m_NSFileCoordinator_coordinateWritingItem
    fileCoordinator
    url
    (fromWritingOptions options)
    errPtr
    $ \newUrl -> do
      path <- filePathFromNSURL newUrl
      result <- action path
      writeIORef resultRef (Just result)
  when (errPtr /= nullPtr) $ peek errPtr >>= throwNSError
  readIORef resultRef >>= maybe (throwIO NullResultException) pure

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
  resultRef <- newIORef Nothing
  m_NSFileCoordinator_coordinateReadingAndWritingItem
    fileCoordinator
    readingUrl
    (fromReadingOptions readingOptions)
    writingUrl
    (fromWritingOptions writingOptions)
    errPtr
    $ \newReadingUrl newWritingUrl -> do
      readingPath <- filePathFromNSURL newReadingUrl
      writingPath <- filePathFromNSURL newWritingUrl
      result <- action readingPath writingPath
      writeIORef resultRef (Just result)
  when (errPtr /= nullPtr) $ peek errPtr >>= throwNSError
  readIORef resultRef >>= maybe (throwIO NullResultException) pure

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
  resultRef <- newIORef Nothing
  m_NSFileCoordinator_coordinateWritingItem
    fileCoordinator
    writingUrl1
    (fromWritingOptions writingOptions1)
    errPtr
    $ \newWritingUrl1 -> do
      writingPath1 <- filePathFromNSURL newWritingUrl1
      m_NSFileCoordinator_coordinateWritingItem
        fileCoordinator
        writingUrl2
        (fromWritingOptions writingOptions2)
        errPtr
        $ \newWritingUrl2 -> do
          writingPath2 <- filePathFromNSURL newWritingUrl2
          result <- action writingPath1 writingPath2
          writeIORef resultRef (Just result)
  when (errPtr /= nullPtr) $ peek errPtr >>= throwNSError
  readIORef resultRef >>= maybe (throwIO NullResultException) pure

#endif

-- * Implementations (no-op on non-Darwin)

coordinateReading :: FilePath -> Bool -> ReadingOptions -> (FilePath -> IO a) -> IO a
#ifdef darwin_HOST_OS
coordinateReading path isDirectory options action = evalContT do
  fileCoordinator <- ContT $ bracket nsFileCoordinator_init m_release
  errPtr <- ContT alloca
  url <- ContT $ withNSURL path isDirectory
  lift $ coordinateReading' fileCoordinator errPtr url options action
#else
coordinateReading path _ _ action = action path
#endif

coordinateWriting :: FilePath -> Bool -> WritingOptions -> (FilePath -> IO a) -> IO a
#ifdef darwin_HOST_OS
coordinateWriting path isDirectory options action = evalContT do
  fileCoordinator <- ContT $ bracket nsFileCoordinator_init m_release
  errPtr <- ContT alloca
  url <- ContT $ withNSURL path isDirectory
  lift $ coordinateWriting' fileCoordinator errPtr url options action
#else
coordinateWriting path _ _ action = action path
#endif

coordinateReadingThenWriting :: FilePath -> Bool -> ReadingOptions -> FilePath -> Bool -> WritingOptions -> (FilePath -> FilePath -> IO a) -> IO a
#ifdef darwin_HOST_OS
coordinateReadingThenWriting readingPath readingIsDirectory readingOptions writingPath writingIsDirectory writingOptions action = evalContT do
  fileCoordinator <- ContT $ bracket nsFileCoordinator_init m_release
  errPtr <- ContT alloca
  readingUrl <- ContT $ withNSURL readingPath readingIsDirectory
  writingUrl <- ContT $ withNSURL writingPath writingIsDirectory
  lift $ coordinateReadingThenWriting' fileCoordinator errPtr readingUrl readingOptions writingUrl writingOptions action
#else
coordinateReadingThenWriting readingPath _ _ writingPath _ _ action = action readingPath writingPath
#endif

coordinateWritingAndWriting :: FilePath -> Bool -> WritingOptions -> FilePath -> Bool -> WritingOptions -> (FilePath -> FilePath -> IO a) -> IO a
#ifdef darwin_HOST_OS
coordinateWritingAndWriting writingPath1 writingIsDirectory1 writingOptions1 writingPath2 writingIsDirectory2 writingOptions2 action = evalContT do
  fileCoordinator <- ContT $ bracket nsFileCoordinator_init m_release
  errPtr <- ContT alloca
  writingUrl1 <- ContT $ withNSURL writingPath1 writingIsDirectory1
  writingUrl2 <- ContT $ withNSURL writingPath2 writingIsDirectory2
  lift $ coordinateWritingAndWriting' fileCoordinator errPtr writingUrl1 writingOptions1 writingUrl2 writingOptions2 action
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

coordinateAccessing ::
  (Traversable t) =>
  FilesToCoordinate t ->
  ( t (WrappedReader IO) ->
    t (WrappedWriter IO) ->
    IO a
  ) ->
  IO a
#ifdef darwin_HOST_OS
coordinateAccessing FilesToCoordinate{..} action =
  evalContT do
    readingUrls <- for readingPaths $ \(path, isDirectory) ->
      ContT $ withNSURL path isDirectory
    writingUrls <- for writingPaths $ \(path, isDirectory) ->
      ContT $ withNSURL path isDirectory
    -- using ordNub allows Haskell caller's to not need to worry about
    -- specifying the same file multiple times
    readingUrlsArray <- ContT $ withNSArray (ordNub $ readingUrls ^.. folded)
    writingUrlsArray <- ContT $ withNSArray (ordNub $ writingUrls ^.. folded)
    fileCoordinator <- ContT $ bracket nsFileCoordinator_init m_release
    errPtr <- ContT alloca
    let readingAccessors =
          readingUrls <&> \url -> WrappedReader \readingOptions readingAction ->
            coordinateReading' fileCoordinator errPtr url readingOptions readingAction
    let writingAccessors =
          writingUrls <&> \url -> WrappedWriter \writingOptions writingAction ->
            coordinateWriting' fileCoordinator errPtr url writingOptions writingAction
    resultRef <- newIORef Nothing
    lift $
      m_NSFileCoordinator_prepareForReadingAndWritingItems
        fileCoordinator
        readingUrlsArray
        (fromReadingOptions blanketReadingOptions)
        writingUrlsArray
        (fromWritingOptions blanketWritingOptions)
        errPtr
        do
          result <- action readingAccessors writingAccessors
          writeIORef resultRef $ Just result
    readIORef resultRef >>= maybe (throwIO NullResultException) pure
#else
coordinateAccessing FilesToCoordinate{..} action =
  let readingAccessors = readingPaths <&> \(path, _) -> WrappedReader \_ readingAction -> readingAction path
      writingAccessors = writingPaths <&> \(path, _) -> WrappedWriter \_ writingAction -> writingAction path
   in action readingAccessors writingAccessors
#endif
