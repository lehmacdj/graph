module Models.Augmentation.FileType where

import Error.UserError
import Graph.GraphMetadataEditing
import Graph.PathPattern.Resolve
import Graph.Paths
import Graph.SystemNodes
import Models.Common
import Models.Edge
import Models.FileTypeInfo
import Models.MimeType
import Models.NID
import Models.Path.Simple
import Models.Path.TH
import Models.PathPattern.Match (ConcreteHop (..), ConcreteRoute (..), routeLastEdges, selectedRoutes)
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
  Eff r FileExtension
fetchFileExtension nid = do
  mp <-
    resolvePath
      nid
      [path|
        ~*/%{excludedLargeSystemNodes}
        /~*/%{Absolute fileExtensionsNID}
      |]
  let extensions =
        ordNub
          [ e.transition
          | r <- mp.resolutions,
            es <- toList (routeLastEdges r),
            e <- toList es
          ]
  case extensions of
    [] -> pure $ FileExtension Nothing
    [extension] -> pure . FileExtension . Just $ extension
    xs -> throwText $ "found multiple file extensions: " <> tshow xs

-- | maybe this should fall back to using `file` if we don't have metadata
-- stored in the graph
fetchFileType ::
  (Members '[GraphMetadataReading, Error UserError] r) =>
  NID ->
  Eff r FileType
fetchFileType nid = do
  mp <-
    resolvePath
      nid
      [path|
        ~*/%{excludedLargeSystemNodes}
        /~*/%{excludedLargeSystemNodes}
        /~*/%{Absolute mimeTypesNID}
      |]
  -- labels from the file node towards the mime type system node
  case map (map (.label) . (.hops)) (concatMap selectedRoutes mp.resolutions) of
    [] -> pure (FileType Nothing)
    [[_, subtype, type']] -> do
      extension <-
        ((.extension) <$> fetchFileExtension nid)
          `onNothingM` throwText "file type found but no file extension"
      let mimeType = MimeType {..}
      pure $ FileType (Just FileTypeInfo {..})
    xs -> throwText $ "found multiple file types: " <> tshow xs

writeTags ::
  (Members '[GraphMetadataEditing] r) =>
  NID ->
  FileType ->
  Eff r ()
writeTags _ _ = error "unimplemented"
