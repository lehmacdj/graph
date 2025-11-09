

module MyPrelude.OsPath
  ( module X,
    module MyPrelude.OsPath,
  )
where

import ClassyPrelude hiding (readFile, readFileUtf8, writeFile, writeFileUtf8, (<.>), (</>))
import Control.Monad.Catch (MonadThrow)
import GHC.Stack (HasCallStack)
import System.File.OsPath qualified
import System.OsPath hiding (unpack)
import System.OsPath as X (OsChar, OsPath, OsString, (<.>), (</>))

encodeFilePath :: (MonadThrow m) => FilePath -> m OsPath
encodeFilePath = encodeUtf

decodeFilePath :: (MonadThrow m) => OsPath -> m FilePath
decodeFilePath = decodeUtf

textToOsString :: (HasCallStack) => Text -> OsString
textToOsString = error "unimplementable, missing: OsString.fromByteString"

readFile :: (MonadIO m) => OsPath -> m ByteString
readFile = liftIO . System.File.OsPath.readFile'

writeFile :: (MonadIO m) => OsPath -> ByteString -> m ()
writeFile path = liftIO . System.File.OsPath.writeFile' path

readFileUtf8 :: (MonadIO m) => OsPath -> m Text
readFileUtf8 = fmap decodeUtf8 . readFile

writeFileUtf8 :: (MonadIO m) => OsPath -> Text -> m ()
writeFileUtf8 path = writeFile path . encodeUtf8
