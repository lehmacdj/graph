-- | This file is for migrations, when making changes that are backwards
-- incompatible to the graph structure, the commit containing that change
-- should also contain a migration for fixing up graphs. Hopefully longterm we
-- can have a more permanent solution, but that isn't necessary until I have a
-- version that I want to declare "stable"
module Executable.Migration (main) where

import Control.Lens hiding (index)
import qualified Data.Char as Char
import Data.Set.Lens
import Effect.FreshNID
import Effect.Graph
import Effect.Graph.Advanced
import Graph.Connect
import Graph.Node
import Graph.Types
import Interpreters (runReadWriteGraphIO)
import MyPrelude hiding (throwString)
import Text.Printf
import UserError

matchOneSem :: Member (Error UserError) r => String -> Node String -> Sem r NID
matchOneSem t n = case matchConnect t (outgoingConnectsOf n) of
  [nid] -> pure nid
  [] ->
    throwString $
      "connect wasn't matched; no transition "
        <> show t
        <> " from "
        <> show (nidOf n)
  nids ->
    throwString $
      "connect has too many matches; too many transitions "
        <> show nids
        <> " from "
        <> show (nidOf n)

validYear :: String -> Bool
validYear = (`elem` map (printf "%0.2d") [2019 .. 2021 :: Int])

validMonth :: String -> Bool
validMonth = (`elem` map (printf "%0.2d") [1 .. 12 :: Int])

validDay :: String -> Bool
validDay = (`elem` map (printf "%0.2d") [1 .. 31 :: Int])

validHour :: String -> Bool
validHour = (`elem` map (printf "%0.2d") [0 .. 23 :: Int])

-- | Valid minute or second
validMS :: String -> Bool
validMS = (`elem` map (printf "%0.2d") [0 .. 59 :: Int])

validFS :: String -> Bool
validFS = (&&) <$> (12 ==) . length <*> all Char.isDigit

getNodeSem' ::
  Members [Error UserError, ReadGraph String] r => NID -> Sem r (Node String)
getNodeSem' = subsumeUserError . getNodeSem

connectsMatchingPredicate :: Node String -> (String -> Bool) -> Set (Connect String)
connectsMatchingPredicate n predicate =
  flip setOf n $
    #outgoing
      . folded
      . filtered (predicate . view #transition)

leftStrength :: Functor m => (a, m b) -> m (a, b)
leftStrength (a, mb) = (a,) <$> mb

augmentWithNode ::
  Members [Error UserError, ReadGraph String] r =>
  [Connect String] ->
  Connect String ->
  Sem r ([Connect String], Node String)
augmentWithNode prevConnects =
  leftStrength . ((: prevConnects) &&& getNodeSem' . view #node)

migrate ::
  Members
    [ ReadGraph String,
      WriteGraph String,
      Error UserError,
      Embed IO,
      FreshNID
    ]
    effs =>
  Sem effs ()
migrate = do
  origin <- getNodeSem' nilNID
  importDatesNID <- matchOneSem "import-dates" origin
  importDates <- getNodeSem' importDatesNID
  years <-
    traverse (augmentWithNode []) . toList $
      importDates `connectsMatchingPredicate` validYear
  months <-
    fmap concat . sequence $
      years <&> \(prevConnects, year) ->
        traverse (augmentWithNode prevConnects) . toList $
          year `connectsMatchingPredicate` validMonth
  days <-
    fmap concat . sequence $
      months <&> \(prevConnects, month) ->
        traverse (augmentWithNode prevConnects) . toList $
          month `connectsMatchingPredicate` validDay
  hours <-
    fmap concat . sequence $
      days <&> \(prevConnects, day) ->
        traverse (augmentWithNode prevConnects) . toList $
          day `connectsMatchingPredicate` validHour
  minutes <-
    fmap concat . sequence $
      hours <&> \(prevConnects, hour) ->
        traverse (augmentWithNode prevConnects) . toList $
          hour `connectsMatchingPredicate` validMS
  seconds <-
    fmap concat . sequence $
      minutes <&> \(prevConnects, minute) ->
        traverse (augmentWithNode prevConnects) . toList $
          minute `connectsMatchingPredicate` validMS
  let targets =
        concat $
          seconds <&> \(prevConnects, s) ->
            map ((: prevConnects) &&& view #node) . toList $
              s `connectsMatchingPredicate` validFS
  nodesToDelete <-
    ordNub . concat <$> for targets \(prevConnects, nid) -> do
      subsumeUserError $
        transitionsViaManyTo
          nilNID
          ("new-import-dates" `ncons` newLabelsFromPrev prevConnects)
          nid
      pure (nidsToDeleteFromPrev prevConnects)
  for_ nodesToDelete deleteNode

newLabelsFromPrev :: HasCallStack => [Connect String] -> [String]
newLabelsFromPrev = \case
  (map (fst . pairOfConnect) -> [f, s, m, h, d, mo, y]) ->
    [y, mo, d, h <> ":" <> m <> ":" <> s <> "." <> f]
  connects -> error $ "bad set of connects: " <> show connects

nidsToDeleteFromPrev :: [Connect String] -> [NID]
nidsToDeleteFromPrev = drop 1 . map (snd . pairOfConnect)

main :: IO ()
main = do
  _ <- error "are you sure you want to use this, not the commit with change; migration should already be complete"
  args :: [String] <- map unpack <$> getArgs
  case index args 0 of
    Nothing -> putStrLn . pack $ "needs one command line argument with a graph"
    Just dir -> runReadWriteGraphIO dir migrate
