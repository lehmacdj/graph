{-# LANGUAGE TemplateHaskell #-}

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
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson as Aeson
import Control.Exception (catch)
import Data.List (intercalate)

import Data.Graph
import Data.Graph.Connect

import Command
import Command.Parser

data S = S
  { _currentNID :: Id
  , _graph :: Graph String
  }
  deriving (Show, Eq, Ord)
makeLenses ''S

emptyS = uncurry S $ (`runState` emptyGraph) $ do
  f <- freshNode
  modify (insertNode f)
  pure (nidOf f)

errorNoEdge :: String -> Repl S ()
errorNoEdge = liftIO . printf "edge missing '%s': failed to execute command\n"

printTransitions :: Set String -> IO ()
printTransitions = putStrLn . unlines' . fmap show . toList where
  unlines' = intercalate "\n"

currentNode :: Repl S (Node String)
currentNode = lookupNode <$> use graph <*> use currentNID

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
    n' <- zoom graph freshNode
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
    result <- liftIO $ (Just <$> B.writeFile fn (Aeson.encode g)) `catch` ioExceptionHandler
    case result of
      Nothing -> liftIO (putStrLn ("error: failed to decode " ++ fn)) >> continue
      Just () -> continue
  Load fn -> do
    g <- liftIO $ (Aeson.decode <$> B.readFile fn) `catch` ioExceptionHandler
    case g of
      Nothing -> liftIO (putStrLn ("error: failed to decode " ++ fn))
      Just g' -> graph .= g'
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
        n' <- zoom graph freshNode
        let nid' = nidOf n'
            selfLoopify :: Set (Connect String) -> Set (Connect String)
            selfLoopify = (setmapped . connectNode . filtered (==nid)) .~ nid'
            setNI = nodeIncoming .~ selfLoopify (view nodeIncoming n)
            setNO = nodeOutgoing .~ selfLoopify (view nodeOutgoing n)
            n'' = (setNI . setNO) n'
        graph %= insertNode n''
    continue

ioExceptionHandler :: IOError -> IO (Maybe a)
ioExceptionHandler _ = pure Nothing

main :: IO ()
main = doRepl "g" parseCommand execCommand emptyS
