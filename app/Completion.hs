{-# LANGUAGE ViewPatterns #-}

-- | Completion stuff for the interface
module Completion where

import Control.Lens
import Data.List
import Data.Foldable
import Data.Maybe
import Control.Repl (ReplBase)
import Control.Monad.IO.Class

import System.Console.Haskeline

import State

import Lang.Path
import Lang.APath

import Lang.Path.Partial
import Graph

import Debug.Trace

type Base a = ReplBase S a

commands :: [String]
commands = []

-- | takes a list of strings and a reversed input string, and produces
-- words from the list that complete the previous thing
mkCompleter :: [String] -> String -> [Completion]
mkCompleter xs x = map simpleCompletion $ filter (x `isPrefixOf`) xs

getCommandCompletions :: String -> String -> Base [Completion]
getCommandCompletions x y
  -- unless the previous text is "" then we want to fail to provide any
  -- completions so that we fall back to another completion provider
  | y == "" = pure . mkCompleter commands . reverse $ x
  | otherwise = pure []

completeCommand :: (String, String) -> Base (String, [Completion])
completeCommand = completeWordWithPrev Nothing " " getCommandCompletions

-- this tries to parse as much of a path as possible, and then deduce what
-- the current word being completed is, it then tries to complete a
-- transition currently on as much as possible, then tries to complete from
-- that location
completePath :: (String, String) -> Base (String, [Completion])
completePath (i, e) = case getPartialPath (takeRelevantFromEnd i) of
  Nothing -> pure (i, [])
  Just (MissingSlash _ _) -> pure (i, [simpleCompletion "/"])
  Just (ppp@(PartialPath nid pp end)) -> do
    let p' = foldr (:/) One pp
    withAPath' (pure (i, [])) (mkAPath nid p') $ \n p -> do
      g <- use graph
      let
        octs = do
          ntid <- resolveSuccesses p n g
          nt <- maybeToList $ maybeLookupNode g ntid
          oc <- toList $ outgoingConnectsOf nt
          pure $ view connectTransition oc
      pure (fromMaybe i (stripPrefix (reverse end) i), mkCompleter octs end)

completionFunction :: (String, String) -> Base (String, [Completion])
completionFunction =
  completeCommand
  `fallbackCompletion` completePath
