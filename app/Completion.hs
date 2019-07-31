{-# LANGUAGE ViewPatterns #-}

-- | Completion stuff for the interface
module Completion where

import Control.Monad.State
import Data.List

import System.Console.Haskeline

import State

type Base a = StateT S IO a

commands :: [String]
commands = []

-- | takes a list of strings and a reversed input string, and produces
-- words from the list that complete the previous thing
mkCompleter :: [String] -> String -> [Completion]
mkCompleter xs (reverse -> x) = map simpleCompletion $ filter (x `isPrefixOf`) xs

getCommandCompletions :: String -> String -> Base [Completion]
getCommandCompletions x y
  -- unless the previous text is "" then we want to fail to provide any
  -- completions so that we fall back to another completion provider
  | y == "" = pure . mkCompleter commands $ x
  | otherwise = pure []

completeCommand :: (String, String) -> Base (String, [Completion])
completeCommand = completeWordWithPrev Nothing " " getCommandCompletions

completePath :: (String, String) -> Base (String, [Completion])
completePath = undefined

completionFunction :: (String, String) -> Base (String, [Completion])
completionFunction =
  completeCommand
  `fallbackCompletion` completePath
