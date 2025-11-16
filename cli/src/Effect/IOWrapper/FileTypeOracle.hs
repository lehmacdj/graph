{-# LANGUAGE TemplateHaskell #-}

-- |
-- Effect for testing the filetype of a data file. Underlyingly uses file(1)
-- shell command.
module Effect.IOWrapper.FileTypeOracle
  ( runFileTypeOracle,
    getFileTypeInfo,
    FileTypeOracle (..),
  )
where

import Control.Lens.Regex.ByteString as ByteString
import Control.Lens.Regex.Text as Text
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Models.FileTypeInfo
import Models.MimeType
import MyPrelude
import System.Process.Typed

data FileTypeOracle :: Effect where
  GetFileTypeInfo ::
    Either ByteString FilePath -> FileTypeOracle m (Maybe FileTypeInfo)

makeEffect ''FileTypeOracle

-- | Extension lookup table for mimetypes where file(1) can't determine
-- extension
extensionLookupTable :: Map MimeType Text
extensionLookupTable =
  mapFromList
    [ (MimeType "video" "mp4", "mp4")
    ]

-- | Handle files where the file(1) command cannot identify the type
getHeuristicFileTypeInfo :: Either ByteString FilePath -> IO (Maybe FileTypeInfo)
getHeuristicFileTypeInfo source = withEarlyReturnIO do
  -- Read only first 20 bytes to check for HTML
  contents <- case source of
    Left bs -> pure bs
    Right fp -> readFile fp
  text <-
    eitherDecodeUtf8 contents
      `onLeft_` (returnEarly . Just)
        (FileTypeInfo (MimeType "application" "octet-stream") "")
  _ <-
    eitherDecodeStrict @AesonValue contents
      `onRight_` (returnEarly . Just)
        (FileTypeInfo (MimeType "application" "json") "json")
  -- must check before XML as HTML can match XML regex
  when (has [ByteString.regex|^.{0,20}<!DOCTYPE html|] contents) $
    returnEarly (Just (FileTypeInfo (MimeType "text" "html") "html"))
  when (has [Text.regex|^\s{0,20}<\?xml|] (T.stripStart text)) $
    returnEarly (Just (FileTypeInfo (MimeType "application" "xml") "xml"))
  when (has [Text.regex|^[a-z]{1,10}://|] (T.stripStart text)) $
    returnEarly (Just (FileTypeInfo (MimeType "text" "uri-list") "uri"))
  pure . Just $ FileTypeInfo (MimeType "text" "plain") "txt"

fileCommand ::
  [String] -> Either ByteString FilePath -> ProcessConfig () () ()
fileCommand extraArgs source =
  let fp = case source of
        Left _ -> "-"
        Right path -> path
      args = fp : "--brief" : extraArgs
      stdinSpec = case source of
        Left bs -> byteStringInput (fromStrict bs)
        Right _ -> nullStream
   in proc "file" args & setStdin stdinSpec

-- | Get mimetype using file(1) command
getMimetypeIO :: (MonadIO m) => Either ByteString FilePath -> m (Maybe MimeType)
getMimetypeIO source = liftIO . withEarlyReturnIO $ do
  let process = fileCommand ["--mime-type"] source
  output <-
    liftIO (try @_ @SomeException (readProcessStdout_ process))
      `onLeftM_` returnEarly Nothing
  pure $ parseMimeType (T.strip (decodeUtf8 (toStrict output)))

-- Returns the first extension if multiple are available
getFileExtensionIO :: (MonadIO m) => Either ByteString FilePath -> m (Maybe Text)
getFileExtensionIO source = liftIO . withEarlyReturnIO $ do
  let process = fileCommand ["--extension"] source
  output <-
    liftIO (try @_ @SomeException (readProcessStdout_ process))
      `onLeftM_` returnEarly Nothing
  -- file can return more than one extension (e.g., "jpeg/jpg/jpe/jfif")
  case T.split (== '/') (decodeUtf8 (toStrict output)) of
    (ext : _) -> pure $ Just ext
    [] -> returnEarly Nothing

-- | Get both mimetype and extension with fallback logic
-- Ported from Python's get_mimetype_and_extension function
-- Returns Nothing if the file doesn't exist or we fail to determine the type
getFileTypeInfoIO :: Either ByteString FilePath -> IO (Maybe FileTypeInfo)
getFileTypeInfoIO source = liftIO . withEarlyReturnIO $ do
  m_mimeType <- getMimetypeIO source
  m_extension <- getFileExtensionIO source
  case (m_mimeType, m_extension) of
    (Just mimeType, Just extension) ->
      returnEarly . Just $ FileTypeInfo {..}
    (Just mimeType, Nothing)
      | Just extension <- extensionLookupTable ^. at mimeType ->
          returnEarly . Just $ FileTypeInfo {..}
      | otherwise -> do
          liftIO (getHeuristicFileTypeInfo source) <&> \case
            Just heuristicInfo
              | heuristicInfo.mimeType == mimeType -> Just heuristicInfo
            _ -> Nothing
    (Nothing, Just extension) -> do
      sayErr "warning: identified extension but no mimetype"
      let mimeType = MimeType "application" "octet-stream"
      returnEarly . Just $ FileTypeInfo {..}
    (Nothing, Nothing) ->
      liftIO $ getHeuristicFileTypeInfo source

-- | Run FileTypeOracle effect
runFileTypeOracle ::
  (IOE :> es) =>
  Eff (FileTypeOracle : es) a ->
  Eff es a
runFileTypeOracle = interpret $ \_ -> \case
  GetFileTypeInfo source -> liftIO $ getFileTypeInfoIO source
