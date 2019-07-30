{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Repl
import Data.Foldable (toList)
import Data.Set (Set)
import Text.Printf
import Control.Lens
import Control.Monad.State
import Control.Exception (catch)
import Data.List (intercalate)
import System.IO.Term.Image
import Network.HTTP.Conduit
import qualified Data.Map as Map

import Graph
import Graph.Serialize2
import Graph.Import.Filesystem
import Graph.Import.ByteString
import Graph.Advanced

import Lang.Command2
import Lang.Command2.Parse
import Lang.APath

import Error

import State
-- import Completion

errorNoEdge :: String -> Repl S ()
errorNoEdge = liftIO . printf "edge missing '%s': failed to execute command\n"

printTransitions :: Set (Connect String) -> IO ()
printTransitions = putStrLn . unlines' . fmap dtransition . toList where
  unlines' = intercalate "\n"
  dtransition (Connect t nid) = show t ++ " at " ++ show nid

currentNode :: Repl S (Node String)
currentNode = lookupNode <$> use graph <*> use currentNID

currentNodeDataFile :: Repl S FilePath
currentNodeDataFile = do
  cnid <- use currentNID
  p <- use filePath
  case p of
    Just base -> pure (nodeDataFile base cnid)
    Nothing -> error "there is no current path"

describe :: String -> Maybe a -> Either String a
describe s Nothing = Left s
describe _ (Just x) = Right x

apathResolve :: APath String -> Repl S (Either String (Node String, Path String))
apathResolve (Absolute nid p) = do
  g <- use graph
  pure . describe ("invalid nid " ++ show nid) $ (\x -> (x, p)) <$> maybeLookupNode g nid
apathResolve (Relative p) = Right <$> do
  n <- currentNode
  pure (n, p)

withAPath
  :: APath String
  -> (Node String -> Path String -> Repl S ())
  -> Repl S ()
withAPath a f = do
  r <- apathResolve a
  case r of
    Left e -> liftIO $ putStrLn e
    Right (n, p) -> f n p

withTwoAPaths
  :: APath String
  -> APath String
  -> (Node String -> Path String -> Node String -> Path String -> Repl S ())
  -> Repl S ()
withTwoAPaths a b f = do
  r <- apathResolve a
  r' <- apathResolve b
  case sequenceOf both (r, r') of
    Left e -> liftIO $ putStrLn e
    Right ((n, p), (n', p')) -> f n p n' p'

-- | Style guide for commands for the future:
-- All commands and paths are interpreted relative to the current location
-- We can reintroduce the ability to execute commands relative to a different
-- location later via an `at` command that changes the location and then
-- changes it back.
-- This means that new nodes created and edges created etc start/end at the
-- current node
-- Commands that act on nodes should also act on at least deterministic
-- paths and if possible nondeterministic paths too
execCommand :: Command -> Repl S ()
execCommand c = case c of
  ChangeNode a -> do
    r <- apathResolve a
    g <- use graph
    case r of
      Left e -> liftIO $ putStrLn e
      Right (n, p) -> case resolveSingle p n g of
        Nothing -> errorNoEdge (show p)
        Just nid -> currentNID .= nid
  Dualize -> modifying graph dualizeGraph
  Make a -> withAPath a $ \n p -> do
    g <- use graph
    g' <- mkPath p n g
    graph .= g'
  Merge a -> withAPath a $ \n p -> do
    g <- use graph
    let g' = mgPath p n g
    graph .= g'
  Clone a t -> withAPath a $ \n p -> do
    g <- use graph
    case resolveSingle p n g of
      Nothing -> errorNoEdge (show p)
      Just nid -> do
        cnid <- use currentNID
        (n', g') <- cloneNode (lookupNode g nid) g
        let g'' = insertEdge (Edge cnid t (nidOf n')) g'
        graph .= g''
  Query a t -> withAPath a $ \n p -> do
    g <- use graph
    case resolveSuccesses' p n g of
      xs -> do
        xnid <- use currentNID
        (n', g') <- followMkEdgeFrom' t xnid g
        graph .= g'
        graph %= insertEdges (uncurry (Edge (nidOf n')) <$> over (mapped._1) projPath xs)
  Tag a b -> withTwoAPaths a b $ \n p n' p' -> do
    -- TODO: make the tag command guarnatee that the tag that is made is a new path
    -- to a new node, not related to previous paths
    -- i.e. tag a b; tag a b; should create two identical links a that point to b
    execCommand (Make (Absolute (nidOf n) p))
    g <- use graph
    case (resolveSuccesses p (refreshNode g n) g, resolveSingle p' (refreshNode g n') g) of
      (nids, Just nid) -> graph .= mergeNodeIds g (nid:nids)
      _ -> errorNoEdge (show p' ++ "\n" ++ show p)
  Remove a -> withAPath a $ \n p -> graph %= deletePath p n
  At a c' -> withAPath a $ \n p -> do
    g <- use graph
    let xs = resolveSuccesses p n g
    forM_ xs $ \nid -> do
      cnid <- use currentNID
      currentNID .= nid
      execCommand c'
      currentNID .= cnid
  Dedup s -> do
    n <- currentNode
    g <- use graph
    let xs :: [String -> Edge String]
        xs = [ \suffix -> Edge nid1 (t ++ suffix) nid2
             | ([FromVia nid1 t], nid2) <- resolveSuccesses' (Literal s) n g
             ]
    let noSuffix = repeat ""
        suffixes
          | length xs < 2 = noSuffix
          | otherwise = show <$> ([1..] :: [Int])
    graph %= delEdges (zipWith ($) xs noSuffix)
    graph %= insertEdges (zipWith ($) xs suffixes)
  ListOut -> do
    -- TODO: possibly make this also print node ids
    n <- currentNode
    liftIO $ printTransitions (outgoingConnectsOf n)
  NodeId -> do
    n <- use currentNID
    liftIO $ print n
  Dump fn -> do
    g <- use graph
    result <- liftIO $ serializeGraph g fn
    displayErr result
  Load fn -> do
    result <- liftIO (deserializeGraph fn)
    displayErrOrGet result $ \g -> do
      graph .= g
      let maxId = maximum (0 : (Map.keys . nodeMap $ g))
      nextId .= maxId + 1
      filePath .= Just fn
  Debug -> do
    s <- get
    liftIO $ print s
  ShowImage -> do
    n <- currentNode
    case view nodeData n of
      Just i -> liftIO . printImage $ i
      Nothing -> liftIO $ putStrLn "error: no image available for this node"
  Import fp -> do
    importer <- liftIO $ importDirectory fp
    nid <- use currentNID
    g <- use graph
    g' <- importer nid g
    graph .= g'
  ImportUrl uri -> do
    md <- liftIO $ (Just <$> simpleHttp uri) `catch` ioExceptionHandler
    case md of
      Nothing -> liftIO $ putStrLn "error: couldn't fetch uri"
      Just d -> do
        nid <- use currentNID
        g <- use graph
        (nid', g') <- importData nid d g
        graph .= g'
        currentNID .= nid'

ioExceptionHandler :: IOError -> IO (Maybe a)
ioExceptionHandler _ = pure Nothing

main :: IO ()
main = doRepl "g" (withDefaultQuitParser parseCommand) execCommand emptyS
