{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Arrow ((>>>))
import Effect.Graph
import Effect.Graph.Advanced
import Effect.Throw
import Effect.Warn
import Graph.Connect
import Graph.Node
import Graph.Types
import MyPrelude
import UserError

nodeRewrites :: [NID] -> [(NID, NID)]
nodeRewrites nids = []
  where
    -- (filter outOfRange nids)
    --  `zip` ([(maximumEx (filter (not . outOfRange) nids) + 1) ..])
    outOfRange _ = False

applyRewrite ::
  Members [ReadGraph String, WriteGraph String, ThrowUserError] effs =>
  (NID, NID) ->
  Eff effs ()
applyRewrite (nid, nid') = subsumeMissing $ do
  n <- getNode' nid
  let i = selfLoopify nid nid' $ incomingConnectsOf n
      o = selfLoopify nid nid' $ outgoingConnectsOf n
  insertNode @String (Node nid' i o (dataOf n))
  deleteNode @String nid

renumberNodes ::
  ( Members [ReadGraph String, WriteGraph String, ThrowUserError] effs,
    LastMember IO effs
  ) =>
  Eff effs ()
renumberNodes = do
  nodes <- nodeManifest @String
  traverse_ applyRewrite $ nodeRewrites nodes

runReadWriteGraphIO ::
  FilePath ->
  Eff
    [ WriteGraph String,
      ReadGraph String,
      Warn UserErrors,
      ThrowUserError,
      IO
    ]
    () ->
  IO ()
runReadWriteGraphIO dir =
  runWriteGraphIO dir
    >>> runReadGraphIO dir
    >>> printWarnings @UserErrors
    >>> printErrors
    >>> runM

main :: IO ()
main = do
  args :: [String] <- map unpack <$> getArgs
  error "make sure to read source / change it before running this executable"
  case index args 0 of
    Nothing -> putStrLn . pack $ "needs one command line argument"
    Just dir -> runReadWriteGraphIO dir $ renumberNodes
