module Models.Augmentation.FileType where

import Error.UserError
import Graph.GraphMetadataEditing
import Graph.MaterializePath
import Graph.Paths
import Graph.SystemNodes
import Models.Common
import Models.Connect
import Models.Edge
import Models.FileTypeInfo
import Models.MaterializedPath
import Models.MimeType
import Models.NID
import Models.Path.Simple
import Models.Path.TH
import MyPrelude

newtype FileExtension = FileExtension
  { extension :: Maybe Text
  }
  deriving (Eq, Show, Ord, Generic)

newtype FileType = FileType
  { fileType :: Maybe FileTypeInfo
  }
  deriving (Eq, Show, Ord, Generic)

instance ShowableAugmentation FileExtension where
  augmentationProperties (FileExtension Nothing) = []
  augmentationProperties (FileExtension (Just ext)) =
    [(Just "extension", ext)]

instance ShowableAugmentation FileType where
  augmentationProperties (FileType Nothing) = []
  augmentationProperties (FileType (Just FileTypeInfo {..})) =
    [ (Just "extension", extension),
      (Just "mime-type", tshow mimeType)
    ]

-- | maybe this should fall back to using `file` if we don't have metadata
-- stored in the graph
fetchFileExtension ::
  (Members '[GraphMetadataReading, Error UserError] r) =>
  NID ->
  Eff es FileExtension
fetchFileExtension nid = do
  mp <-
    materializePath
      nid
      [path|
        ~*/%{excludedLargeSystemNodes}
        /~*/%{Absolute fileExtensionsNID}
      |]
  case toList (finalNonLoopEdges mp.path) of
    [] -> pure $ FileExtension Nothing
    [extension] -> pure . FileExtension . Just $ (head extension).transition
    xs ->
      throwText $
        "found multiple file extensions: "
          <> tshow (map (map (.transition) . toList) xs)

-- | maybe this should fall back to using `file` if we don't have metadata
-- stored in the graph
fetchFileType ::
  (Members '[GraphMetadataReading, Error UserError] r) =>
  NID ->
  Eff es FileType
fetchFileType nid = do
  mp <-
    materializePath
      nid
      [path|
        ~*/%{excludedLargeSystemNodes}
        /~*/%{excludedLargeSystemNodes}
        /~*/%{Absolute mimeTypesNID}
      |]
  case toList (leftmostConnects mp.path) of
    [] -> pure (FileType Nothing)
    [(_, [Connect subtype _, Connect type' _])] -> do
      extension <-
        ((.extension) <$> fetchFileExtension nid)
          `onNothingM` throwText "file type found but no file extension"
      let mimeType = MimeType {..}
      pure $ FileType (Just FileTypeInfo {..})
    xs -> throwText $ "found multiple file types: " <> tshow (map snd xs)

writeTags ::
  (Members '[GraphMetadataEditing] r) =>
  NID ->
  FileType ->
  Eff es ()
writeTags _ _ = error "unimplemented"
