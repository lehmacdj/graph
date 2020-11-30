{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Repl
  ( makeRepl,
    liftIO,
    withDefaultQuitParser,
    C (..),
    Settings,
    setComplete,
    defaultSettings,
  )
where

import Control.Monad.IO.Class
import Data.Functor
import MyPrelude
import Polysemy.Readline
import System.Console.Haskeline
  ( Settings,
    defaultSettings,
    setComplete,
  )

data C c
  = Quit
  | C c
  deriving (Show, Eq, Ord)

-- | takes as parameters:
-- * a prompt
-- * a parser for commands or quit
-- * an executor for commands
makeRepl ::
  Member Readline effs =>
  String ->
  (String -> Either String (C c)) ->
  (c -> Sem effs ()) ->
  Sem effs ()
makeRepl title parser execute = repl
  where
    repl = do
      command <- getInputLine (title ++ "> ")
      case parser <$> command of
        Nothing -> outputStrLn "Goodbye!"
        Just (Right Quit) -> outputStrLn "Goodbye!"
        Just (Right (C command')) -> execute command' >> repl
        Just (Left err) -> outputStrLn err >> repl

-- | Takes :q and :quit as quit and otherwise defers to the main parser
withDefaultQuitParser ::
  (String -> Either String c) -> String -> Either String (C c)
withDefaultQuitParser p s
  | s == ":q" || s == ":quit" = Right Quit
  | otherwise = C <$> p s
