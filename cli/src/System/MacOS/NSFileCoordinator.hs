{-# LANGUAGE CPP #-}

module System.MacOS.NSFileCoordinator
  ( coordinateReading,
    coordinateWriting,
    coordinateReadingThenWriting,
    coordinateWritingAndWriting,
    coordinateAccessing,
    ReadingOptions (..),
    WritingOptions (..),
    NullResultException (..),
    NSErrorException (..),
  )
where

#ifdef darwin_HOST_OS

import Control.Lens
import Control.Monad.Trans.Cont
import Foreign
import Foreign.C.String
import MyPrelude
import System.MacOS.NSFileCoordinator.RawBindings

withNSURL :: FilePath -> Bool -> (Ptr NSURL -> IO a) -> IO a
withNSURL path isDirectory action = withCString path $ \pathPtr ->
  bracket (nsURL_initFileURL pathPtr (fromBool isDirectory)) m_release action

data WritingOptions = WritingOptions

fromWritingOptions :: WritingOptions -> NSFileCoordinatorWritingOptions
fromWritingOptions = 0

data ReadingOptions = ReadingOptions

fromReadingOptions :: ReadingOptions -> NSFileCoordinatorReadingOptions
fromReadingOptions = 0

filePathFromNSURL :: Ptr NSURL -> IO FilePath
filePathFromNSURL url = do
  path <- p_NSURL_path url
  unpack <$> packNSString path

data NSErrorException = NSErrorException
  { domain :: Text,
    code :: Int
  }
  deriving (Show, Exception)

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
    (nsArray_initWithObjects (castToId objectsPtr) (fromIntegral len))
    m_release
    action

data NullResultException = NullResultException
  deriving (Show, Exception)

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

coordinateReading :: FilePath -> Bool -> ReadingOptions -> (FilePath -> IO a) -> IO a
coordinateReading path isDirectory options action = evalContT do
  fileCoordinator <- ContT $ bracket nsFileCoordinator_init m_release
  errPtr <- ContT alloca
  url <- ContT $ withNSURL path isDirectory
  lift $ coordinateReading' fileCoordinator errPtr url options action

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

coordinateWriting :: FilePath -> Bool -> WritingOptions -> (FilePath -> IO a) -> IO a
coordinateWriting path isDirectory options action = evalContT do
  fileCoordinator <- ContT $ bracket nsFileCoordinator_init m_release
  errPtr <- ContT alloca
  url <- ContT $ withNSURL path isDirectory
  lift $ coordinateWriting' fileCoordinator errPtr url options action

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

coordinateReadingThenWriting :: FilePath -> Bool -> ReadingOptions -> FilePath -> Bool -> WritingOptions -> (FilePath -> FilePath -> IO a) -> IO a
coordinateReadingThenWriting readingPath readingIsDirectory readingOptions writingPath writingIsDirectory writingOptions action = evalContT do
  fileCoordinator <- ContT $ bracket nsFileCoordinator_init m_release
  errPtr <- ContT alloca
  readingUrl <- ContT $ withNSURL readingPath readingIsDirectory
  writingUrl <- ContT $ withNSURL writingPath writingIsDirectory
  lift $ coordinateReadingThenWriting' fileCoordinator errPtr readingUrl readingOptions writingUrl writingOptions action

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

coordinateWritingAndWriting :: FilePath -> Bool -> WritingOptions -> FilePath -> Bool -> WritingOptions -> (FilePath -> FilePath -> IO a) -> IO a
coordinateWritingAndWriting writingPath1 writingIsDirectory1 writingOptions1 writingPath2 writingIsDirectory2 writingOptions2 action = evalContT do
  fileCoordinator <- ContT $ bracket nsFileCoordinator_init m_release
  errPtr <- ContT alloca
  writingUrl1 <- ContT $ withNSURL writingPath1 writingIsDirectory1
  writingUrl2 <- ContT $ withNSURL writingPath2 writingIsDirectory2
  lift $ coordinateWritingAndWriting' fileCoordinator errPtr writingUrl1 writingOptions1 writingUrl2 writingOptions2 action

coordinateAccessing ::
  Traversable t =>
  t (FilePath, Bool) ->
  ReadingOptions ->
  t (FilePath, Bool) ->
  WritingOptions ->
  ( t (ReadingOptions -> (FilePath -> IO ()) -> IO ()) ->
    t (WritingOptions -> (FilePath -> IO ()) -> IO ()) ->
    IO ()
  ) ->
  IO ()
coordinateAccessing readingPaths blanketReadingOptions writingPaths blanketWritingOptions action =
  evalContT do
    readingUrls <- for readingPaths $ \(path, isDirectory) ->
      ContT $ withNSURL path isDirectory
    writingUrls <- for writingPaths $ \(path, isDirectory) ->
      ContT $ withNSURL path isDirectory
    readingUrlsArray <- ContT $ withNSArray (toList readingUrls)
    writingUrlsArray <- ContT $ withNSArray (toList readingUrls)
    fileCoordinator <- ContT $ bracket nsFileCoordinator_init m_release
    errPtr <- ContT alloca
    let readingAccessors =
          readingUrls <&> \url readingOptions readingAction ->
            coordinateReading' fileCoordinator errPtr url readingOptions readingAction
    let writingAccessors =
          writingUrls <&> \url writingOptions writingAction ->
            coordinateWriting' fileCoordinator errPtr url writingOptions writingAction
    lift $
      m_NSFileCoordinator_prepareForReadingAndWritingItems
        fileCoordinator
        readingUrlsArray
        (fromReadingOptions blanketReadingOptions)
        writingUrlsArray
        (fromWritingOptions blanketWritingOptions)
        errPtr
        $ action readingAccessors writingAccessors

#endif
