-- | Module providing utilities relating to stack scripts that can be run from
-- the graph editor interface. This provides both the functions for those
-- scripts, and the functions for calling such scripts.
--
-- WARNING: the interface between 'runScript' and 'defaultMain' is unstable.
-- It is guaranteed that running a script defined with defaultMain will work,
-- but otherwise the interface is undefined.
-- TODO: tutorial on how to use this
module Utils.Extensibility where

import qualified Data.ByteString.Lazy as BL
import Effect.Interpreters (HasMainEffects, runLocatedReadWriteGraphIO)
import Effect.NodeLocated
import Effect.UserError
import Effect.Warn
import Graph.Effect
import MyPrelude
import System.IO.Temp
import System.Process.Typed

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
    Members [GetLocation, WriteGraph String, ReadGraph String (Maybe ByteString), Warn UserError, Error UserError, Embed IO] r =>
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
-- for for the commit below for maximum compatibility.
baseResolver :: Text
baseResolver = "lts-21.24"

-- | Commit to fetch from github when running executable, see this link for API
-- https://github.com/lehmacdj/graph/blob/db615d800a71c769528aaadd6feed5a9984d9535
commit :: Text
commit = "db615d800a71c769528aaadd6feed5a9984d9535"

-- | Contents of the snapshot that is used when calling an executable script.
-- TODO: clean this up so that it is more testable that this is in good shape
-- and being updated as necessary.
snapshotContents :: Text
snapshotContents =
  "resolver: " ++ baseResolver ++ "\n"
    ++ "packages:\n"
    ++ ("- url: https://github.com/lehmacdj/graph/releases/download/graph-cli-" ++ commit ++ "/graph-" ++ commit ++ ".tar.gz\n")
    ++ "- polysemy-readline-0.2.0.0@sha256:30633eaee828f967b9f653f5406e55671684ae1ec6c513e3f2de184a301dadd1,2106\n"
    ++ "- polysemy-zoo-0.8.2.0@sha256:bd8f802cb0402082053b3b0b97c8b551b071a14960e6b271a5b78be728b73a24,3952\n"
    ++ "- compact-0.2.0.0@sha256:75ef98cb51201b4a0d6de95cbbb62be6237c092a3d594737346c70c5d56c2380,2413\n"

-- | Dependencies used when running a script. These all need to be available in
-- the snapshot defined by 'snapshotContents'
dependencies :: [Text]
dependencies = ["graph", "polysemy", "lens", "polysemy-plugin", "bytestring"]

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
  let tmpResolver = tempDir </> "snapshot.yaml"
  embed $ writeFile tmpResolver (encodeUtf8 snapshotContents)
  nid <- currentLocation
  base <- getGraphFilePath
  let args =
        ["script", "--silent", "--no-terminal"]
          ++ fmap unpack ["--resolver", tmpResolver]
          ++ fmap unpack dependenciesArgs
          ++ [program, "--"]
          ++ [base, show nid]
  -- TODO: switch to using 'runProcess_' which uses inherit once we no-longer
  -- need two different versions of terminfo. I was getting:
  -- > ge: Received ExitFailure (-11) when running
  -- on the call to 'runProcess_' before. This is a segfault, and based on a
  -- little bit of research I suspect that it is a consequence of the two
  -- versions of terminfo being linked.
  -- Using 'readProcessInterleaved_' seems to avoid it, but is unideal, because
  -- then we aren't able to interleave reading and writing output if we want
  -- to have an executable thing.
  output <- readProcessInterleaved_ $ proc "stack" args
  embed $ BL.putStr output
