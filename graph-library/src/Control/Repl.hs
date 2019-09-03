{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Repl
    ( makeRepl
    , runRepl
    , doRepl
    , doRepl'
    , runRepl'
    , liftIO
    , Repl(..)
    , ReplBase(..)
    , withDefaultQuitParser
    , C(..)
    , Settings
    , setComplete
    , defaultSettings
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Reader.Class
import Data.Functor

import System.Console.Haskeline (InputT, defaultSettings, runInputT
    , getInputLine, Settings, MonadException(..), RunIO(..), setComplete
    , mapInputT)

newtype ReplBase r a = ReplBase { unReplBase :: ReaderT r IO a }
  deriving (Functor, Monad, MonadReader r, MonadIO, Applicative, MonadException)

newtype Repl r a = Repl { unRepl :: InputT (ReplBase r) a }
  deriving (Functor, Monad, MonadIO, Applicative)

instance MonadReader r (Repl r) where
  ask = Repl . lift $ ask
  local f m = Repl $ mapInputT (local f) (unRepl m)

data C c
  = Quit
  | C c
  deriving (Show, Eq, Ord)

-- | takes as parameters:
-- * a prompt
-- * a parser for commands or quit
-- * an executor for commands
makeRepl :: String
         -> (String -> Either String (C c))
         -> (c -> Repl r ())
         -> Repl r ()
makeRepl title parser execute = repl where
    repl = do
        command <- Repl $ getInputLine (title ++ "> ")
        case parser <$> command of
            Nothing -> liftIO (putStrLn "Goodbye!") $> ()
            Just (Right Quit) -> liftIO (putStrLn "Goodbye!") $> ()
            Just (Right (C command')) -> execute command' >> repl
            Just (Left err) -> liftIO (putStrLn err) >> repl

runRepl' :: Settings (ReplBase r) -> Repl r a -> r -> IO a
runRepl' settings repl r =
  (`runReaderT` r)
  . unReplBase
  . runInputT settings
  . unRepl
  $ repl

runRepl :: Repl r a -> r -> IO a
runRepl = runRepl' defaultSettings

-- | Takes :q and :quit as quit and otherwise defers to the main parser
withDefaultQuitParser
    :: (String -> Either String c) -> String -> Either String (C c)
withDefaultQuitParser p s
  | s == ":q" || s == ":quit" = Right Quit
  | otherwise = C <$> p s

doRepl :: String
       -> (String -> Either String (C c))
       -> (c -> Repl r ())
       -> r
       -> IO ()
doRepl = doRepl' defaultSettings

doRepl' :: Settings (ReplBase r)
       -> String
       -> (String -> Either String (C c))
       -> (c -> Repl r ())
       -> r
       -> IO ()
doRepl' settings = ((runRepl' settings.).). makeRepl
