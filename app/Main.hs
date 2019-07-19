{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Lib
import Control.Repl
import Data.Functor
import Data.Foldable (toList)
import Data.Set (Set)
import Text.Printf
import Control.Lens
import Control.Lens.Zoom
import Data.Set.Lens (setmapped)
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson as Aeson
import Control.Exception (catch)
import Data.List (intercalate)
import System.IO.Term.Image
import System.Directory
import qualified Data.Map as Map

import Graph
import Graph.Connect
import Graph.Serialize2
import Graph.Import.Filesystem

import Command
import Command.Parser

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

printTransitions :: Set String -> IO ()
printTransitions = putStrLn . unlines' . fmap show . toList where
  unlines' = intercalate "\n"

currentNode :: Repl S (Node String)
currentNode = lookupNode <$> use graph <*> use currentNID

currentNodeDataFile :: Repl S FilePath
currentNodeDataFile = do
  cnid <- use currentNID
  path <- use filePath
  case path of
    Just base -> pure (nodeDataFile base cnid)
    Nothing -> error "there is no current path"

currentBinaryData :: Repl S ByteString
currentBinaryData = do
  cnid <- use currentNID
  path <- use filePath
  case path of
    Just base -> liftIO $ getBinaryData base cnid
    Nothing -> pure ""

execCommand :: Command -> Repl S () -> Repl S ()
execCommand c continue = case c of
  Quit -> pure ()
  ChangeNode s -> do
    n <- currentNode
    case matchConnect s (outgoingConnectsOf n) of
      Nothing -> errorNoEdge s
      Just nid -> currentNID .= nid
    continue
  Dualize -> modifying graph dualizeGraph >> continue
  MakeNode s -> do
    nid <- use currentNID
    n' <- freshNode
    let nid' = nidOf n'
    let e = Edge nid s nid'
    graph %= insertNode n'
    graph %= insertEdge e
    continue
  NodeId -> do
    n <- use currentNID
    liftIO $ print n
    continue
  ListOut -> do
    n <- currentNode
    liftIO $ printTransitions (outgoingTransitionsOf n)
    continue
  ListIn -> do
    n <- currentNode
    liftIO $ printTransitions (incomingTransitionsOf n)
    continue
  AddEdgeTo nid s -> do
    cnid <- use currentNID
    graph %= insertEdge (Edge cnid s nid)
    continue
  AddEdgeFrom nid s -> do
    cnid <- use currentNID
    graph %= insertEdge (Edge nid s cnid)
    continue
  Goto nid -> do
    g <- use graph
    case maybeLookupNode g nid of
      Just n -> currentNID .= nidOf n
      Nothing -> liftIO (putStrLn "error: no node with such node id")
    continue
  Dump fn -> do
    g <- use graph
    result <- liftIO $ serializeGraph g fn
    case result of
      Nothing -> liftIO (putStrLn ("error: failed to decode " ++ fn))
      Just () -> pure ()
    continue
  Load fn -> do
    g <- liftIO (deserializeGraph fn)
    case g of
      Nothing -> liftIO (putStrLn ("error: failed to decode " ++ fn))
      Just g' -> do
        graph .= g'
        let maxId = maximum (0 : (Map.keys . nodeMap $ g'))
        nextId .= maxId + 1
        filePath .= Just fn
    continue
  Debug -> do
    s <- get
    liftIO $ print s
    continue
  RemoveEdgeOut s -> do
    n <- currentNode
    case matchConnect s (outgoingConnectsOf n) of
      Nothing -> pure ()
      Just nid -> graph %= delEdge (Edge (nidOf n) s nid)
    continue
  RemoveEdgeIn s -> do
    n <- currentNode
    case matchConnect s (incomingConnectsOf n) of
      Nothing -> pure ()
      Just nid -> graph %= delEdge (Edge (nidOf n) s nid)
    continue
  CloneNode nid -> do
    g <- use graph
    case maybeLookupNode g nid of
      Nothing -> pure ()
      Just n -> do
        n' <- freshNode
        let nid' = nidOf n'
            selfLoopify :: Set (Connect String) -> Set (Connect String)
            selfLoopify = (setmapped . connectNode . filtered (==nid)) .~ nid'
            setNI = nodeIncoming .~ selfLoopify (view nodeIncoming n)
            setNO = nodeOutgoing .~ selfLoopify (view nodeOutgoing n)
            n'' = (setNI . setNO) n'
        graph %= insertNode n''
    continue
  ShowImage -> do
    n <- currentNode
    case view nodeData n of
      Just i -> liftIO . printImage $ i
      Nothing -> liftIO $ putStrLn "error: no image available for this node"
    continue
  SetBinaryData fp -> do
    dfp <- currentNodeDataFile
    liftIO $ copyFile fp dfp
    continue
  Import fp -> do
    importer <- liftIO $ importDirectory fp
    nid <- use currentNID
    g <- use graph
    g' <- importer nid g
    graph .= g'
    continue

ioExceptionHandler :: IOError -> IO (Maybe a)
ioExceptionHandler _ = pure Nothing

main :: IO ()
main = doRepl "g" parseCommand execCommand emptyS
