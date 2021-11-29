-- | Module providing utilities relating to stack scripts that can be run from
-- the graph editor interface. This provides both the functions for those
-- scripts, and the functions for calling such scripts.
--
-- WARNING: the interface between 'runScript' and 'defaultMain' is unstable.
-- It is guaranteed that running a script defined with defaultMain will work,
-- but otherwise the interface is undefined.
-- TODO: tutorial on how to use this
module Extensibility where

import Effect.Graph
import Effect.NodeLocated
import Effect.Warn
import Interpreters (HasMainEffects, runLocatedReadWriteGraphIO)
import MyPrelude
import System.IO.Temp
import System.Process.Typed
import UserError

-- | Provide a convenient interface for writing stack scripts that interface
-- with the graph that can be run from the graph directory.
--
-- For now this just allows commands that have access to the current node,
-- access to the graph, and the ability to emit warnings/errors.
--
-- TODO: generalize for a variety of usecases:
-- 1. more effects (ideally the full set of effects available to commands right now)
-- 2. commands with arguments, or additional requirements
--
-- It should be relatively easy to extend this in those directions because we can
-- define the process call interface however we want. Most likely we can just
-- pass a serialized version of Env over via command line arguments and read
-- modified values back from stdout (or more likely a temporary pipe or file,
-- because we'll want the process to be able to emit to stdout/err itself too.
defaultMain ::
  ( forall r.
    Members [GetLocation, WriteGraph String, ReadGraph String, Warn UserError, Error UserError, Embed IO] r =>
    Sem r ()
  ) ->
  IO ()
defaultMain action = do
  args <- getArgs
  case args of
    [unpack -> base, readMay . unpack -> Just nid] ->
      runLocatedReadWriteGraphIO base nid action
    _ ->
      error $
        "didn't provide correct command line arguments; "
          ++ "was this called without using runScript?"

-- | Base resolver, this should always be equal to the resolver in stack.yaml
-- for maximum compatibility.
baseResolver :: Text
baseResolver = "lts-18.14"

-- | Commit to fetch from github when running executable
commit :: Text
commit = "74271502e6da21749993cac5f57724b37ddc86eb"

-- | Contents of the snapshot that is used when calling an executable script.
-- TODO: clean this up so that it is more testable that this is in good shape
-- and being updated as necessary.
snapshotContents :: Text
snapshotContents =
  "resolver: " ++ baseResolver ++ "\n"
    ++ "packages:\n"
    ++ "- github: lehmacdj/graph\n"
    ++ ("  commit: " ++ commit ++ "\n")
    ++ "  subdirs:\n"
    ++ "    - core/\n"
    ++ "- polysemy-plugin-0.3.0.0\n"
    ++ "- polysemy-1.5.0.0\n"
    ++ "- polysemy-readline-0.2.0.0@sha256:30633eaee828f967b9f653f5406e55671684ae1ec6c513e3f2de184a301dadd1,2106\n"
    ++ "- polysemy-zoo-0.7.0.2@sha256:64857606e456c4df033a79e41cef49059b23c98046b76aeadf990b08379dd6e9,3990\n"
    ++ "- witherable-class-0.0.1@sha256:6ec78f157f715098eecc08dff177064abfc783ee2151a95f5aecff5c4c7d2d95,847\n"
    ++ "- compact-0.2.0.0@sha256:75ef98cb51201b4a0d6de95cbbb62be6237c092a3d594737346c70c5d56c2380,2413\n"

-- | Dependencies used when running a script. These all need to be available in
-- the snapshot defined by 'snapshotContents'
dependencies :: [Text]
dependencies = ["graph", "polysemy", "lens", "polysemy-plugin"]

dependenciesArgs :: [Text]
dependenciesArgs =
  take (length dependencies * 2) $
    repeat "--package" `interleave` dependencies

-- | Run a script that uses 'defaultMain'.
-- TODO: weaken the constraint here, HasMainEffects probably gives this
-- more power than is really necessary
runScript :: HasMainEffects r => FilePath -> Sem r ()
runScript program = do
  tempDir <- embed do
    tmpParentDir <- getCanonicalTemporaryDirectory
    createTempDirectory tmpParentDir "ge-external-program-call"
  traceM $ "using tempDir: " ++ tempDir
  let tmpResolver = tempDir </> "snapshot.yaml"
  embed $ writeFile tmpResolver (encodeUtf8 snapshotContents)
  traceM "wrote resolver"
  nid <- currentLocation
  traceShowM nid
  base <- getGraphFilePath
  traceM base
  traceShowM dependencies
  traceShowM dependenciesArgs
  traceShowM tmpResolver
  traceShowM program
  traceShowM base
  traceShowM $ show nid
  let args =
        fmap unpack ["script", "--resolver", tmpResolver]
          ++ fmap unpack dependenciesArgs
          ++ [program, "--"]
          ++ [base, show nid]
  traceM $ "running stack with args:" ++ show args
  runProcess_ $ proc "stack" args
