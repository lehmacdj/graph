{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.MetadataWriteDiff where

import Control.Lens (ifor, ifor_)
import Control.Monad.Trans.Cont
import DAL.DirectoryFormat
import DAL.FileSystemOperations.Metadata
import DAL.Interpreters
import DAL.RawGraph
import Error.Missing
import Error.UserError
import Models.Edge
import Models.Graph
import Models.NID
import Models.Node
import MyPrelude
import System.MacOS.NSFileCoordinator

data GraphMetadataFilesystemOperationsWriteDiff m a where
  WriteGraphDiff ::
    -- | Nodes read from the source of truth (e.g. the filesystem)
    Map NID (Maybe (Node Text ())) ->
    -- | Graph of changes, including all additions, and accurately reflecting
    -- deletions (though deletions may not be reflected if they were never
    -- loaded)
    Graph Text () ->
    -- | Edges that were deleted
    Set (Edge Text) ->
    GraphMetadataFilesystemOperationsWriteDiff m ()

makeSem ''GraphMetadataFilesystemOperationsWriteDiff

writeGraphDiff_ ::
  (Members [RawGraph, Embed IO, Error UserError] effs) =>
  Map NID (Maybe (Node Text ())) ->
  Graph Text () ->
  Set (Edge Text) ->
  Sem effs ()
writeGraphDiff_ loaded changes deletedEdges = do
  let finalNodes :: Set (Node Text ())
      finalNodes = setFromList $ nodesOf changes
      loadedNodes :: Set (Node Text ())
      loadedNodes = setFromList $ catMaybes $ toList loaded
      -- nodes that we may need to write to
      writingNids =
        mapFromSet
          $
          -- if a node is still the same as when it was loaded, it wasn't changed
          mapSet (.nid) (finalNodes \\ loadedNodes)
          -- any node which had an edge deleted may have changed
          <> toSetOf (folded . #source <> folded . #sink) deletedEdges
      -- we need to read every node we write to, plus any node that was loaded
      -- to check that nothing changed since we started the transaction
      readingNids = writingNids <> (loaded $> ())
  readingPaths <- ifor readingNids \nid _ -> getMetadataFile nid <&> (,False)
  writingPaths <- ifor writingNids \nid _ -> getMetadataFile nid <&> (,False)
  let blanketReadingOptions = defaultReadingOptions
      blanketWritingOptions = defaultWritingOptions
  withUnliftIORawGraphUserError \(UnliftRawGraphUserErrorIO unliftIO) ->
    coordinateAccessing FilesToCoordinate {..} \wrappedReaders wrappedWriters -> evalContT do
      let beginReading ::
            forall a. (HasCallStack) => NID -> ReadingOptions -> ContT a IO FilePath
          beginReading nid options =
            ContT
              $ unwrappingReader (wrappedReaders ^. at nid . to (unwrapEx "missing reader")) options
      let beginWriting ::
            forall a. (HasCallStack) => NID -> WritingOptions -> ContT a IO FilePath
          beginWriting nid options =
            ContT
              $ unwrappingWriter (wrappedWriters ^. at nid . to (unwrapEx "missing writer")) options
      upToDateNodes' <- ifor readingPaths \nid _ -> do
        path <- beginReading nid defaultReadingOptions
        upToDateNode <- lift . unliftIO $ readNodeMetadata_ path
        -- if the node was loaded, we require that it is the same now as it was
        -- when loading, if it wasn't the node is up to date inherently
        pure
          if maybe True (== upToDateNode) $ loaded ^. at nid
            then Just upToDateNode
            else Nothing
      let notUpToDateNids = keysSet $ filterMap (== Nothing) upToDateNodes'
      unless (null notUpToDateNids)
        $ lift
        . unliftIO
        . throwText
        $ "The following nodes were changed before trying to write the diff: "
        ++ tshow notUpToDateNids
      let upToDateNodes :: Map NID (Maybe (Node Text ()))
          upToDateNodes = map (unwrapEx "node exists due to above check") upToDateNodes'
      ifor_ writingPaths \nid _ -> do
        let upToDateNode =
              unwrapEx "upToDateNode exists for nodes we are writing"
                $ upToDateNodes
                ^. at nid
        let changedNode = changes ^. at nid
        -- only hold the write lock while we are actually writing
        evalContT . lift $ case (upToDateNode, changedNode) of
          (Just _, Nothing) -> do
            path <- beginWriting nid defaultWritingOptions {forDeleting = True}
            lift . unliftIO $ deleteNodeMetadata_ path
          (Nothing, Just node) -> do
            path <- beginWriting nid defaultWritingOptions
            lift . unliftIO $ writeNodeMetadata_ path (node & withoutEdges deletedEdges)
          (Just upToDateNode', Just changedNode')
            | upToDateNode' /= changedNode' -> do
                path <- beginWriting nid defaultWritingOptions
                lift
                  . unliftIO
                  $ writeNodeMetadata_ path
                  $ mergeNodesEx upToDateNode' changedNode'
                  & withoutEdges deletedEdges
            | otherwise ->
                -- node didn't change
                pure ()
          (Nothing, Nothing) ->
            -- node never existed and still doesn't exist
            pure ()

runGraphMetadataFilesystemOperationsWriteDiffIO ::
  (Members [RawGraph, Embed IO, Error UserError] effs) =>
  Sem (GraphMetadataFilesystemOperationsWriteDiff ': effs) a ->
  Sem effs a
runGraphMetadataFilesystemOperationsWriteDiffIO = interpret \case
  WriteGraphDiff loaded changes deletedEdges ->
    writeGraphDiff_ loaded changes deletedEdges
