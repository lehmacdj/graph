{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import Control.Monad.Unique

data S = S
  { _filePath :: Maybe FilePath
  , _nextId :: Id -- ^ next id that is unique within the current graph
  , _currentNID :: Id
  , _graph :: Graph String
  }
  deriving (Show, Eq, Ord)
makeLenses ''S

instance MonadUnique Id (Repl S) where
  fresh = do
    nid <- use nextId
    nextId += 1
    pure nid

-- | Create a node with a unique id not yet in the graph
-- intended for use in larger monad states using zoom from Control.Lens.Zoom
-- Every node should be created using this method, this guarantees that every
-- new node has a unique id.
freshNode :: Repl S (Node t)
freshNode = do
  nid <- use nextId
  nextId += 1
  pure (emptyNode nid)

emptyS :: S
emptyS = S Nothing 1 0 (insertNode (emptyNode 0) emptyGraph)

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
        graph %= insertEdges (uncurry (Edge (nidOf n')) <$> xs)
  AddLinksFromTo a t -> withAPath a $ \n p -> do
    g <- use graph
    case resolveSuccesses' p n g of
      xs -> do
        cnid <- use currentNID
        (n', g') <- followMkEdgeFrom' t cnid g
        graph .= g'
        graph %= insertEdges (xs <&> \(name, nid) -> Edge nid name (nidOf n'))
  Remove a -> withAPath a $ \n p -> graph %= deletePath p n
  At a c' -> withAPath a $ \n p -> do
    g <- use graph
    case resolveSingle p n g of
      Nothing -> errorNoEdge (show p)
      Just nid -> do
        cnid <- use currentNID
        currentNID .= nid
        execCommand c'
        currentNID .= cnid
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
    case result of
      Nothing -> liftIO (putStrLn ("error: failed to decode " ++ fn))
      Just () -> pure ()
  Load fn -> do
    g <- liftIO (deserializeGraph fn)
    case g of
      Nothing -> liftIO (putStrLn ("error: failed to decode " ++ fn))
      Just g' -> do
        graph .= g'
        let maxId = maximum (0 : (Map.keys . nodeMap $ g'))
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
