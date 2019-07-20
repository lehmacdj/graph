module Control.Repl
    ( makeRepl
    , runRepl
    , doRepl
    , liftIO
    , Repl
    , withDefaultQuitParser
    , C(..)
    ) where

import Control.Monad.State (liftIO, StateT, runStateT, lift)
import Data.Functor

import System.Console.Haskeline (InputT, defaultSettings, runInputT, getInputLine)

type Repl s = StateT s (InputT IO)

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
        command <- lift $ (reader <$>) <$> getInputLine (title ++ "> ")
        case command of
            Nothing -> liftIO (putStrLn "Goodbye!") $> ()
            Just (Right Quit) -> liftIO (putStrLn "Goodbye!") $> ()
            Just (Right (C command')) -> execute command' >> repl
            Just (Left err) -> liftIO (putStrLn err) >> repl

runRepl :: Repl s a -> s -> IO a
runRepl repl s = runInputT defaultSettings $ fst <$> runStateT repl s

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
doRepl = ((runRepl.).). makeRepl
