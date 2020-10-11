{-# LANGUAGE NoImplicitPrelude #-}

-- | Completion stuff for the interface
module Completion where

import MyPrelude

import Control.Lens
import Control.Arrow

import System.Console.Haskeline

import Env
import App

import Lang.Path

import Lang.Path.Partial
import Lang.Parsing
import Graph

import Effect.Graph
import Effect.Graph.Advanced
import Effect.NodeLocated
import Effect.Throw

type Base a = AppBase a

commands :: [String]
commands = []

-- | takes a list of strings and a reversed input string, and produces
-- words from the list that complete the previous thing
mkCompleter :: [String] -> String -> [Completion]
mkCompleter xs x = map simpleCompletion' $ filter (x `isPrefixOf`) xs

getCommandCompletions :: String -> String -> Base [Completion]
getCommandCompletions x y
  -- unless the previous text is "" then we want to fail to provide any
  -- completions so that we fall back to another completion provider
  | y == "" = pure . mkCompleter commands . reverse $ x
  | otherwise = pure []

completeCommand :: (String, String) -> Base (String, [Completion])
completeCommand = completeWordWithPrev Nothing " " getCommandCompletions

quoteUnusualTransition :: String -> String
quoteUnusualTransition x
  | isUnusualTransition x = show x
  | otherwise = x

isUnusualTransition :: String -> Bool
isUnusualTransition = any (not . isIdentChar)

-- | simpleCompletion except the completion is not treated as finished,
-- thus no space is added after
simpleCompletion' :: String -> Completion
simpleCompletion' x = Completion x x False

mkTransitionCompleter :: [String] -> String -> [Completion]
mkTransitionCompleter xs x =
  quotify $ filter check xs where
    quotify z
      | any isUnusualTransition z = map (simpleCompletion' . show) z
      | otherwise = map simpleCompletion' z
    check =
      quoteUnusualTransition &&& id
      >>> over both (x `isPrefixOf`)
      >>> uncurry (||)

-- this tries to parse as much of a path as possible, and then deduce what
-- the current word being completed is, it then tries to complete a
-- transition currently on as much as possible, then tries to complete from
-- that location
completePath :: (String, String) -> Base (String, [Completion])
completePath (i, _) = case getPartialPath (takeRelevantFromEnd i) of
  Nothing -> pure (i, [])
  Just (MissingSlash _ _) -> pure (i, [simpleCompletion "/"])
  Just (PartialPath nid pp end) -> do
    fp' <- view filePath >>= readIORef
    case fp' of
      Nothing -> pure (i, [])
      Just fp -> do
        let
          handler =
            runReadGraphDualizeableIO @String fp
            >>> runDualizeableAppBase
            >>> runLocableAppBase
            >>> (\x -> handleError @Missing x (\_ -> pure (i, [])))
            >>> runM
        handler $ do
          let p = foldr (:/) One pp
          l <- currentLocation
          ntids <- toList <$> resolvePathSuccesses (fromMaybe l nid) p
          nts <- getNodes @String ntids
          let
            octs = -- outgoing connect transitions
              toListOf (folded . nodeOutgoing . folded . connectTransition) nts
          pure (fromMaybe i (stripPrefix (reverse end) i), mkCompleter octs end)

completionFunction :: (String, String) -> Base (String, [Completion])
completionFunction =
  completeCommand
  `fallbackCompletion` completePath
