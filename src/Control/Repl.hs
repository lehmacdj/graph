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
import Control.Monad.Trans.State.Strict (StateT(..), runStateT)
import Control.Monad.State.Class
import Data.Functor

import System.Console.Haskeline (InputT, defaultSettings, runInputT
    , getInputLine, Settings, MonadException(..), RunIO(..), setComplete)

newtype ReplBase s a = ReplBase { unReplBase :: StateT s IO a }
  deriving (Functor, Monad, MonadState s, MonadIO, Applicative, MonadException)

newtype Repl s a = Repl { unRepl :: InputT (ReplBase s) a }
  deriving (Functor, Monad, MonadIO, Applicative)

instance MonadState s (Repl s) where
  state = Repl . lift . state

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
         -> (c -> Repl s ())
         -> Repl s ()
makeRepl title reader execute = repl where
    repl = do
        command <- Repl $ getInputLine (title ++ "> ")
        case reader <$> command of
            Nothing -> liftIO (putStrLn "Goodbye!") $> ()
            Just (Right Quit) -> liftIO (putStrLn "Goodbye!") $> ()
            Just (Right (C command')) -> execute command' >> repl
            Just (Left err) -> liftIO (putStrLn err) >> repl

runRepl' :: Settings (ReplBase s) -> Repl s a -> s -> IO a
runRepl' settings repl s =
  fmap fst
  . (`runStateT` s)
  . unReplBase
  . runInputT settings
  . unRepl
  $ repl

runRepl :: Repl s a -> s -> IO a
runRepl = runRepl' defaultSettings

-- | Takes :q and :quit as quit and otherwise defers to the main parser
withDefaultQuitParser
    :: (String -> Either String c) -> String -> Either String (C c)
withDefaultQuitParser p s
  | s == ":q" || s == ":quit" = Right Quit
  | otherwise = C <$> p s

doRepl :: String
       -> (String -> Either String (C c))
       -> (c -> Repl s ())
       -> s
       -> IO ()
doRepl = doRepl' defaultSettings

doRepl' :: Settings (ReplBase s)
       -> String
       -> (String -> Either String (C c))
       -> (c -> Repl s ())
       -> s
       -> IO ()
doRepl' settings = ((runRepl' settings.).). makeRepl
