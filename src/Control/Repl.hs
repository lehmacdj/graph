module Control.Repl
    ( makeRepl
    , runRepl
    , doRepl
    , liftIO
    , Repl
    ) where

import Control.Monad.State (liftIO, StateT, runStateT, lift)
import Data.Functor

import System.Console.Haskeline (InputT, defaultSettings, runInputT, getInputLine)

type Repl s = StateT s (InputT IO)

-- the parameter repl is the continuation that should be executed to continue,
-- ordinary execution of the repl
makeRepl :: String
         -> (String -> Either String c)
         -> (c -> Repl s () -> Repl s ())
         -> Repl s ()
makeRepl title reader execute = repl where
    repl = do
        command <- lift $ (reader <$>) <$> getInputLine (title ++ "> ")
        case command of
            Nothing -> liftIO (putStrLn "Goodbye!") $> ()
            Just (Right command') -> execute command' repl
            Just (Left err) -> liftIO (putStrLn err) >> repl

runRepl :: Repl s a -> s -> IO a
runRepl repl s = runInputT defaultSettings $ fst <$> runStateT repl s

doRepl :: String
       -> (String -> Either String c)
       -> (c -> Repl s () -> Repl s ())
       -> s
       -> IO ()
doRepl = ((runRepl.).). makeRepl
