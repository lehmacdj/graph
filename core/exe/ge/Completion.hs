{-# LANGUAGE NoImplicitPrelude #-}

-- | Completion stuff for the interface
module Completion where

import App
import Control.Arrow
import Control.Lens
import Effect.Graph.Advanced
import Effect.NodeLocated
import Effect.Throw
import Effect.Warn
import Graph
import Lang.Parsing
import Lang.Path
import Lang.Path.Partial
import MyPrelude
import System.Console.Haskeline
import UserError

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
isUnusualTransition = not . all isIdentChar

-- | simpleCompletion except the completion is not treated as finished,
-- thus no space is added after
simpleCompletion' :: String -> Completion
simpleCompletion' x = Completion x x False

mkTransitionCompleter :: [String] -> String -> [Completion]
mkTransitionCompleter xs x =
  quotify $ filter check xs
  where
    quotify z
      | any isUnusualTransition z = map (simpleCompletion' . show) z
      | otherwise = map simpleCompletion' z
    check =
      quoteUnusualTransition &&& id
        >>> over both (x `isPrefixOf`)
        >>> uncurry (||)

failCompletionWithOriginalInputOnErrorOrWarning ::
  Member (Embed IO) effs =>
  -- | The original input
  String ->
  Sem (Warn UserErrors : Error UserErrors : effs) (String, [Completion]) ->
  Sem effs (String, [Completion])
failCompletionWithOriginalInputOnErrorOrWarning i =
  printWarnings
    >>> (`handleError` const (pure (i, [])))

-- this tries to parse as much of a path as possible, and then deduce what
-- the current word being completed is, it then tries to complete a
-- transition currently on as much as possible, then tries to complete from
-- that location
completePath :: (String, String) -> Base (String, [Completion])
completePath (i, _) = case getPartialPath (takeRelevantFromEnd i) of
  Nothing -> pure (i, [])
  Just (MissingSlash _ _) -> pure (i, [simpleCompletion "/"])
  Just (PartialPath nid pp end) ->
    runMainEffects (failCompletionWithOriginalInputOnErrorOrWarning i) $ do
      let p = foldr (:/) One pp
      l <- currentLocation
      ntids <- toList <$> subsumeMissing (resolvePathSuccesses (fromMaybe l nid) p)
      nts <- getNodes @String ntids
      let octs =
            -- outgoing connect transitions
            toListOf (folded . nodeOutgoing . folded . connectTransition) nts
      pure (fromMaybe i (stripPrefix (reverse end) i), mkCompleter octs end)

completionFunction :: (String, String) -> Base (String, [Completion])
completionFunction =
  completeCommand
    `fallbackCompletion` completePath
