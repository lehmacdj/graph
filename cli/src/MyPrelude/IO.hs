module MyPrelude.IO where

import ClassyPrelude
import System.IO

eputStr :: String -> IO ()
eputStr = hPutStrLn stderr

eprint :: Show a => a -> IO ()
eprint = eputStr . show

ioErrorToMaybe :: IO a -> IO (Maybe a)
ioErrorToMaybe = (`ClassyPrelude.catch` ioHandler) . (Just <$>)
  where
    ioHandler :: IOError -> IO (Maybe a)
    ioHandler = pure . const Nothing

ignoreIOError :: IO a -> IO ()
ignoreIOError a = ioErrorToMaybe a $> ()
