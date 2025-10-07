{-# LANGUAGE TemplateHaskell #-}

module DAL.FileSystemOperations.MetadataWriteDiff where

import Control.Lens (ifor)
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
  (NID -> FilePath -> Sem '[RawGraph, Error UserError, Embed IO, Final IO] (Maybe (Node Text ()))) ->
  (FilePath -> Node Text () -> Sem '[RawGraph, Error UserError, Embed IO, Final IO] ()) ->
  (FilePath -> Sem '[RawGraph, Error UserError, Embed IO, Final IO] ()) ->
  Map NID (Maybe (Node Text ())) ->
  Graph Text () ->
  Set (Edge Text) ->
  Sem effs ()
writeGraphDiff_
  readNodeMetadata'
  writeNodeMetadata'
  deleteNodeMetadata'
  (force -> loaded)
  (force -> changes)
  (force -> deletedEdges) = do
    let finalNodes :: Set (Node Text ())
        finalNodes = setFromList $ nodesOf changes
        loadedNodes :: Set (Node Text ())
        loadedNodes = setFromList $ catMaybes $ toList loaded
        -- nodes that we may need to write to
        writingNids =
          mapFromSet $
            -- if a node is still the same as when it was loaded, it wasn't changed
            mapSet (.nid) (finalNodes \\ loadedNodes)
              -- any node which had an edge deleted may have changed
              <> toSetOf (folded . #source <> folded . #sink) deletedEdges
        -- we need to read every node we write to, plus any node that was loaded
        -- to check that nothing changed since we started the transaction
        readingNids = writingNids <> (loaded $> ())
    readingPaths <- ifor readingNids \nid () -> getMetadataFile nid <&> (,False)
    writingPaths <- ifor writingNids \nid () -> getMetadataFile nid <&> (,False)
    let blanketReadingOptions = defaultReadingOptions
        blanketWritingOptions = defaultWritingOptions
    withUnliftIORawGraphUserError \(UnliftRawGraphUserErrorIO unliftIO) ->
      coordinateAccessing FilesToCoordinate {..} \wrappedReaders wrappedWriters -> do
        let beginReading ::
              forall a. (HasCallStack) => NID -> ReadingOptions -> ContT a IO FilePath
            beginReading nid options =
              ContT $
                unwrappingReader (wrappedReaders ^. at nid . to (unwrapEx "missing reader")) options
        let beginWriting ::
              forall a. (HasCallStack) => NID -> WritingOptions -> ContT a IO FilePath
            beginWriting nid options =
              ContT $
                unwrappingWriter (wrappedWriters ^. at nid . to (unwrapEx "missing writer")) options
        upToDateNodes' <- ipooledForConcurrentlyN 300 readingPaths \nid _ ->
          evaluateDeep =<< evalContT do
            path <- beginReading nid defaultReadingOptions
            upToDateNode <- lift . unliftIO $ readNodeMetadata' nid path
            _ <- evaluateDeep upToDateNode
            -- if the node was loaded, we require that it is the same now as it was
            -- when loading, if it wasn't the node is up to date inherently
            pure
              if maybe True (== upToDateNode) $ loaded ^. at nid
                then Just upToDateNode
                else Nothing
        let notUpToDateNids = keysSet $ filterMap (== Nothing) upToDateNodes'
        unless (null notUpToDateNids)
          $ unliftIO
            . throwText
          $ "The following nodes were changed before trying to write the diff: "
            ++ tshow notUpToDateNids
        let upToDateNodes = map (unwrapEx "node exists due to above check") upToDateNodes'
        ipooledForConcurrentlyN_ 300 writingPaths \nid _ -> do
          let upToDateNode =
                unwrapEx "upToDateNode exists for nodes we are writing" $
                  upToDateNodes ^. at nid
          let changedNode =
                (changes ^. at nid)
                  <&> withoutEdges deletedEdges . maybe id mergeNodesEx upToDateNode
          evalContT case (upToDateNode, changedNode) of
            (Just _, Nothing) -> do
              path <- beginWriting nid defaultWritingOptions {forDeleting = True}
              lift . unliftIO $ deleteNodeMetadata' path
            (Nothing, Just node) -> do
              path <- beginWriting nid defaultWritingOptions
              lift . unliftIO $ writeNodeMetadata' path (node & withoutEdges deletedEdges)
            (Just upToDateNode', Just changedNode')
              | upToDateNode' /= changedNode' -> do
                  path <- beginWriting nid defaultWritingOptions
                  lift . unliftIO $ writeNodeMetadata' path changedNode'
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
    writeGraphDiff_
      (readNodeMetadata_ False)
      (writeNodeMetadata_ False)
      (deleteNodeMetadata_ False)
      loaded
      changes
      deletedEdges

runGraphMetadataFilesystemOperationsWriteDiffDryRun ::
  (Members [RawGraph, Embed IO, Error UserError] effs) =>
  Sem (GraphMetadataFilesystemOperationsWriteDiff ': effs) a ->
  Sem effs a
runGraphMetadataFilesystemOperationsWriteDiffDryRun = interpret \case
  WriteGraphDiff loaded changes deletedEdges -> do
    -- say "loaded:"
    -- ifor_ loaded \nid node ->
    --   say $ "  " <> tshow nid <> " -> " <> tshow node
    -- if null (tshow changes)
    --   then say "changes: none"
    --   else do
    --     say "changes:"
    --     say $ tshow changes
    -- say $ "deletedEdges: " <> tshow (toList deletedEdges)
    writeGraphDiff_
      (readNodeMetadata_ False)
      (\path node -> say $ "write: " <> tshow node <> " -> " <> tshow path)
      (\path -> say $ "delete at path: " <> tshow path)
      loaded
      changes
      deletedEdges
