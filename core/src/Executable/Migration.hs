module Executable.Migration (main) where

import Effect.Graph
import Effect.Graph.Advanced
import Graph.Connect
import Graph.Node
import Graph.Types
import Interpreters (runReadWriteGraphIO)
import MyPrelude
import UserError

-- | This function is for determining what nodes to rewrite. Below is some of
-- the stuff I used one of the times I used this program
-- (filter outOfRange nids)
--  `zip` ([(maximumEx (filter (not . outOfRange) nids) + 1) ..])
-- outOfRange _ = False
nodeRewrites :: [NID] -> [(NID, NID)]
nodeRewrites _ = []

applyRewrite ::
  Members [ReadGraph String, WriteGraph String, Error UserError] effs =>
  (NID, NID) ->
  Sem effs ()
applyRewrite (nid, nid') = subsumeUserError $ do
  n <- getNodeSem nid
  let i = selfLoopify nid nid' $ incomingConnectsOf n
      o = selfLoopify nid nid' $ outgoingConnectsOf n
  insertNode @String (Node nid' i o (dataOf n))
  deleteNode @String nid

renumberNodes ::
  Members [ReadGraph String, WriteGraph String, Error UserError, Embed IO] effs =>
  Sem effs ()
renumberNodes = do
  nodes <- nodeManifest @String
  traverse_ applyRewrite $ nodeRewrites nodes

main :: IO ()
main = do
  args :: [String] <- map unpack <$> getArgs
  _ <- error "make sure to read source / change it before running this executable"
  case index args 0 of
    Nothing -> putStrLn . pack $ "needs one command line argument"
    Just dir -> runReadWriteGraphIO dir renumberNodes
