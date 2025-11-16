{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.FileSystem where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.TH
import Error.UserError
import MyPrelude
import System.Directory qualified as Directory
import System.Directory.Tree

data FileSystem :: Effect where
  ReadDirectory :: FilePath -> FileSystem m (DirTree ByteString)
  WriteDirectory :: FilePath -> DirTree ByteString -> FileSystem m (DirTree ())
  CanonicalizePath ::
    -- | filepath to convert
    FilePath ->
    FileSystem m (Either IOError FilePath)

makeEffect ''FileSystem

runFileSystemIO ::
  (IOE :> es, Error UserError :> es) =>
  Eff (FileSystem : es) a ->
  Eff es a
runFileSystemIO = interpret $ \_ -> \case
  ReadDirectory fp ->
    liftIO $ dirTree <$> readDirectoryWithL readFile fp
  WriteDirectory fp dt ->
    liftIO $ dirTree <$> writeDirectoryWith writeFile (fp :/ dt)
  CanonicalizePath fp ->
    liftIO . try . Directory.canonicalizePath $ fp
