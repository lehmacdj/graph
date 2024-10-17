module Effect.Graph.Export.Filesystem
  ( exportToDirectory,
  )
where

import Data.Tree (Tree)
import qualified Data.Tree as T
import Effect.FileTypeOracle
import Effect.Graph
import Effect.Warn
import Models.Graph
import qualified DAL.Serialization as S2
import MyPrelude
import Polysemy.Error
import System.Directory (doesFileExist)
import System.Directory.Clone (cloneFile)
import qualified System.Directory.Tree as DT
import System.FilePath (makeValid, splitFileName)
import UserError

-- | Like a DFS tree of the graph, but include duplciates unless they would form
-- a cycle. Term Heterarchy comes from neuron zettelkasten
graphHeterarchy ::
  forall r.
  Members [Embed IO, RawGraph, Error UserError, FileTypeOracle] r =>
  (String, NID) ->
  Sem r (Tree (Maybe (FilePath, String), String))
graphHeterarchy (startName, start) = do
  fp <- getGraphFilePath
  let dataFileAndExtensionIfExists :: NID -> Sem r (Maybe (FilePath, String))
      dataFileAndExtensionIfExists nid = do
        let ndf = S2.nodeDataFile fp nid
        fileExists <- embed $ doesFileExist ndf
        if fileExists
          then Just <$> ((ndf,) . unpack <$> getExtension ndf)
          else pure Nothing
      go :: Set NID -> NID -> Sem r [Tree (Maybe (FilePath, String), String)]
      go visited nid
        | nid `member` visited = pure []
        | otherwise = do
          n :: Node String <- S2.deserializeNodeWithoutDataF fp nid
          let visited' = singleton nid <> visited
              toTree (Connect t nid') = do
                dfAndE <- dataFileAndExtensionIfExists nid'
                T.Node (dfAndE, t) <$> go visited' nid'
          traverse toTree (toList (outgoingConnectsOf n))
  dfAndE <- dataFileAndExtensionIfExists start
  T.Node (dfAndE, startName) <$> go mempty start

heterarchyToDirTree ::
  Tree (Maybe (FilePath, String), String) ->
  DT.DirTree FilePath
heterarchyToDirTree = DT.Dir "." . T.foldTree toNodes
  where
    toNodes ::
      (Maybe (FilePath, String), String) ->
      -- | A inner list consisting of a max of 2 elements consisting of:
      -- * a file if the data for that node is non null
      -- * a directory if that node has children
      [[DT.DirTree FilePath]] ->
      [DT.DirTree FilePath]
    toNodes (dfAndE, n) children =
      let fn = makeValid n
          file = case dfAndE of
            Just (df, ext) -> [DT.File (fn <.> ext) df]
            Nothing -> []
          dir = case concat children of
            [] -> []
            children' -> [DT.Dir fn children']
       in dir <> file

-- | Writes a directory structure resembling the decendents of a given node.
-- The output path is either taken to be a name for the origin node, or a place
-- to put the graph depending on its structure, in a way that is fairly
-- consistent with the behavior of unix @mv@:
-- * @some/path@ will put the data for the origin node as a file @path.ext@ and
-- children in the directory @path/@
-- * @some/path/@ will put the the data for the origin node as file @origin.ext@
-- and children in the directory @origin/@
--
-- This behavior is formally defined by the splitting rules of 'splitFileName'.
-- If the second split name is @""@ then origin is used for the name of the
-- origin node.
--
-- TODO: It would be nice to implement more of this in terms of ReadGraph, or
-- some more principled effect than Embed IO. To be able to do that we need a
-- few facilities:
-- 1. we need to be able to fetch nodes without their data. Loading the file
-- for every node every time is too expensive in general
-- 2. we need to be able to access filepath of nodes or stuff. This second
-- thing is arguably not a feature we want, so maybe it is better to keep this
-- as a hacky thing, but anyways we'll cross that bridge when we get there.
exportToDirectory ::
  forall r.
  Members [RawGraph, Embed IO, Error UserError, FileTypeOracle, Warn UserError] r =>
  NID ->
  FilePath ->
  Sem r ()
exportToDirectory nid outputFp = do
  let (dir, splitName) = splitFileName outputFp
      originName
        | splitName == "" = "origin"
        | otherwise = splitName
  heterarchy <- graphHeterarchy (originName, nid)
  let dt = heterarchyToDirTree heterarchy
  _ DT.:/ results <- embed $ DT.writeDirectoryWith (flip cloneFile) (dir DT.:/ dt)
  when (DT.anyFailed results) do
    let overallDescription =
          OtherError "some error occurred while exporting a directory tree:"
        failureToUserError = \case
          DT.Failed name e ->
            OtherError ("faile while writing file: " <> name) <> IOFail e
          _ -> error "invariant of failures guarantes only failures are left"
    warn . Multiple . toNonEmpty . (overallDescription `ncons`) $
      failureToUserError <$> DT.failures results
