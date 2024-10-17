-- | Completion stuff for the interface
module Completion where

import Base62 (isBase62Char)
import Control.Arrow
import Control.Lens
import Effect.Graph.Advanced
import Effect.NodeLocated
import Effect.Warn
import DAL.Serialization (nodesWithPrefix)
import Graph.Types.NID (nidDigits)
import Interpreters
import Lang.Parsing
import Lang.Path
import Lang.Path.Partial
import MyPrelude
import System.Console.Haskeline
import UserError

commands :: [String]
commands = []

-- | takes a list of strings and a reversed input string, and produces
-- words from the list that complete the previous thing
mkCompleter :: [String] -> String -> [Completion]
mkCompleter xs x = map simpleCompletion' $ filter (x `isPrefixOf`) xs

getCommandCompletions :: String -> String -> IO [Completion]
getCommandCompletions x y
  -- unless the previous text is "" then we want to fail to provide any
  -- completions so that we fall back to another completion provider
  | y == "" = pure . mkCompleter commands . reverse $ x
  | otherwise = pure []

completeCommand :: (String, String) -> IO (String, [Completion])
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

-- TODO: fix this so that it is possible to complete transitions that use
-- special characters
-- currently, when completing something like @Hello/world/"!|@ (with the cursor
-- indicated by @|@, it isn't possible to complete a transition like @!!!@
-- because we don't appear to generate any options. Probably the partial
-- path parser needs to be improved to fix this.
mkTransitionCompleter :: [String] -> String -> [Completion]
mkTransitionCompleter xs x =
  mkCompleter (xs >>= validCompletions) x
  where
    validCompletions z
      | isUnusualTransition z = [show z]
      | x `isPrefixOf` z = [z]
      | otherwise = [show z, z]

failCompletionWithOriginalInputOnErrorOrWarning ::
  Member (Embed IO) effs =>
  -- | The original input
  String ->
  Sem (Warn UserError : Error UserError : effs) (String, [Completion]) ->
  Sem effs (String, [Completion])
failCompletionWithOriginalInputOnErrorOrWarning i =
  printWarnings
    >>> (`handleError` const (pure (i, [])))

-- this tries to parse as much of a path as possible, and then deduce what
-- the current word being completed is, it then tries to complete a
-- transition currently on as much as possible, then tries to complete from
-- that location
completePath :: Env -> (String, String) -> IO (String, [Completion])
completePath env (i, _) = case getPartialPath (takeRelevantFromEnd i) of
  Nothing -> pure (i, [])
  Just (MissingSlash _) -> pure (i, [simpleCompletion "/"])
  Just (PartialPath pp end) ->
    runMainEffectsIOWithErrorHandling
      (failCompletionWithOriginalInputOnErrorOrWarning i)
      env
      $ do
        let p = foldr (:/) One pp
        nid <- currentLocation
        ntids <- toList <$> subsumeUserError (resolvePathSuccesses nid p)
        nts <- getNodes @String ntids
        let octs =
              -- outgoing connect transitions
              toListOf (folded . #outgoing . folded . #transition) nts
        pure (fromMaybe i (stripPrefix (reverse end) i), mkCompleter octs end)

completeNID :: Env -> (String, String) -> IO (String, [Completion])
completeNID env (i, _) = do
  let (reversedNidPrefix, scraps) = span isBase62Char . take (nidDigits + 1) $ i
  case scraps of
    '@' : _ -> do
      base <- readIORef $ env ^. #_filePath
      nids <- nodesWithPrefix base (reverse reversedNidPrefix)
      pure (fromMaybe i (stripPrefix reversedNidPrefix i), map (simpleCompletion' . show) nids)
    _ -> pure (i, [])

completionFunction :: Env -> (String, String) -> IO (String, [Completion])
completionFunction env =
  completeCommand
    `fallbackCompletion` completeNID env
    `fallbackCompletion` completePath env
