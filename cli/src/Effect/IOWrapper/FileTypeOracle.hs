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
import Models.FileTypeInfo
import Models.MimeType
import MyPrelude
import System.Process.Typed

data FileTypeOracle m r where
  GetFileTypeInfo :: FilePath -> FileTypeOracle m (Maybe FileTypeInfo)

makeSem ''FileTypeOracle

-- | Extension lookup table for mimetypes where file(1) can't determine
-- extension
extensionLookupTable :: Map MimeType Text
extensionLookupTable =
  mapFromList
    [ (MimeType "video" "mp4", "mp4")
    ]

-- | Handle files where the file(1) command cannot identify the type
getHeuristicFileTypeInfo :: FilePath -> IO (Maybe FileTypeInfo)
getHeuristicFileTypeInfo fp = withEarlyReturnIO do
  -- Read only first 20 bytes to check for HTML
  contents <- readFile fp
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

-- | Get mimetype using file(1) command
getMimetypeIO :: (MonadIO m) => FilePath -> m (Maybe MimeType)
getMimetypeIO fp = liftIO . withEarlyReturnIO $ do
  output <-
    ( liftIO . try @_ @SomeException . readProcessStdout_ $
        proc "file" ["--brief", "--mime-type", fp]
      )
      `onLeftM_` returnEarly Nothing
  pure $ parseMimeType (T.strip (decodeUtf8 (toStrict output)))

-- | Get file extension using file(1) command
-- Returns the first extension if multiple are available
getFileExtensionIO :: (MonadIO m) => FilePath -> m (Maybe Text)
getFileExtensionIO fp = liftIO . withEarlyReturnIO $ do
  output <-
    ( liftIO . try @_ @SomeException . readProcessStdout_ $
        proc "file" ["--brief", "--extension", fp]
      )
      `onLeftM_` returnEarly Nothing
  -- file can return more than one extension (e.g., "jpeg/jpg/jpe/jfif")
  case T.split (== '/') (decodeUtf8 (toStrict output)) of
    (ext : _) -> pure $ Just ext
    [] -> returnEarly Nothing

-- | Get both mimetype and extension with fallback logic
-- Ported from Python's get_mimetype_and_extension function
-- Returns Nothing if the file doesn't exist or we fail to determine the type
getFileTypeInfoIO :: FilePath -> IO (Maybe FileTypeInfo)
getFileTypeInfoIO fp = liftIO . withEarlyReturnIO $ do
  m_mimeType <- getMimetypeIO fp
  m_extension <- getFileExtensionIO fp
  case (m_mimeType, m_extension) of
    (Just mimeType, Just extension) ->
      returnEarly . Just $ FileTypeInfo {..}
    (Just mimeType, Nothing)
      | Just extension <- extensionLookupTable ^. at mimeType ->
          returnEarly . Just $ FileTypeInfo {..}
      | otherwise -> do
          liftIO (getHeuristicFileTypeInfo fp) <&> \case
            Just heuristicInfo
              | heuristicInfo.mimeType == mimeType -> Just heuristicInfo
            _ -> Nothing
    (Nothing, Just extension) -> do
      sayErr $
        ("[WARNING] " <> T.pack fp <> ": ")
          <> ("identified extension " <> extension <> " but no mimetype")
      let mimeType = MimeType "application" "octet-stream"
      returnEarly . Just $ FileTypeInfo {..}
    (Nothing, Nothing) ->
      liftIO $ getHeuristicFileTypeInfo fp

-- | Run FileTypeOracle effect
runFileTypeOracle ::
  (Member (Embed IO) r) =>
  Sem (FileTypeOracle : r) a ->
  Sem r a
runFileTypeOracle = interpret \case
  GetFileTypeInfo fp -> embed $ getFileTypeInfoIO fp
