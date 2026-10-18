{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.FileSystem where

import Error.UserError
import MyPrelude
import System.Directory qualified as Directory
import System.Directory.Tree hiding (readDirectory, writeDirectory)
import Utils.Testing

data FileSystem :: Effect where
  ReadDirectory :: FilePath -> FileSystem m (DirTree ByteString)
  WriteDirectory :: FilePath -> DirTree ByteString -> FileSystem m (DirTree ())
  CanonicalizePath ::
    -- | filepath to convert
    FilePath ->
    FileSystem m (Either IOError FilePath)
  -- | The user's home directory, for a path that the user writes with a
  -- @~@ ('expandHome').
  HomeDirectory :: FileSystem m (Either IOError FilePath)

makeEffect ''FileSystem

runFileSystemIO ::
  (IOE :> effs, Error UserError :> effs) =>
  Eff (FileSystem : effs) ~> Eff effs
runFileSystemIO = interpret \_ -> \case
  ReadDirectory fp ->
    liftIO $ dirTree <$> readDirectoryWithL readFile fp
  WriteDirectory fp dt ->
    liftIO $ dirTree <$> writeDirectoryWith writeFile (fp :/ dt)
  CanonicalizePath fp ->
    liftIO . try . Directory.canonicalizePath $ fp
  HomeDirectory -> liftIO (try Directory.getHomeDirectory)

-- | What follows the home directory in a path that starts there: one that
-- starts with a @~@ that is all of it or that a @/@ follows. A @~@ with a
-- user's name after it is not one.
homeRelative :: FilePath -> Maybe FilePath
homeRelative = \case
  "~" -> Just ""
  '~' : '/' : rest -> Just rest
  _ -> Nothing

-- | The path, with a @~@ at its start ('homeRelative') as the home
-- directory that is given. The home directory is only asked for then.
expandHome :: (Applicative m) => m FilePath -> FilePath -> m FilePath
expandHome home path =
  maybe (pure path) (\rest -> (</> rest) <$> home) (homeRelative path)

spec_expandHome :: Spec
spec_expandHome = it "reads a ~ at the start as the home directory" do
  let expanded = expandHome (Just "/home/u")
  expanded "~" `shouldBe` Just "/home/u"
  expanded "~/a b" `shouldBe` Just "/home/u/a b"
  -- and asks for no home directory where there is no ~
  for_ ["~user/a", "a/~/b", "./~", "~a", "a"] \path ->
    expandHome Nothing path `shouldBe` Just path
