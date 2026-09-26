-- | The augmentations that read the system nodes through paths.
module Models.Augmentation.FetchSpec where

import Effectful.State.Dynamic
import Error.UserError
import Graph.GraphMetadataEditing (GraphMetadataEditing', runInMemoryGraphMetadataEditing)
import Graph.SystemNodes
import Models.Augmentation.FileType
import Models.Augmentation.Tags
import Models.Edge
import Models.FileTypeInfo
import Models.Graph (Graph)
import Models.MimeType
import Models.NID
import MyPrelude
import Utils.Testing

file :: NID
file = smallNID 1

-- | A file tagged foo and bar, with extension png and mime type image/png.
fixture :: Graph Text ()
fixture =
  graphWithEdges
    [ Edge tagsNID "foo" (smallNID 2),
      Edge (smallNID 2) "" file,
      Edge tagsNID "bar" (smallNID 3),
      Edge (smallNID 3) "" file,
      Edge fileExtensionsNID "png" (smallNID 4),
      Edge (smallNID 4) "" file,
      Edge mimeTypesNID "image" (smallNID 5),
      Edge (smallNID 5) "png" (smallNID 6),
      Edge (smallNID 6) "" file
    ]

runFetch ::
  Graph Text () ->
  Eff '[GraphMetadataEditing' Text, Error UserError, State (Graph Text ())] a ->
  Either String a
runFetch g action =
  first show $
    action
      & runInMemoryGraphMetadataEditing
      & runErrorNoCallStack
      & evalStateLocal g
      & runPureEff

spec_fetchAugmentations :: Spec
spec_fetchAugmentations = describe "fetching augmentations" do
  it "reads every tag of a node" $
    fmap (.tags) (runFetch fixture (fetchTags file))
      `shouldBe` Right (setFromList ["foo", "bar"])
  it "reads no tags of an untagged node" $
    fmap (.tags) (runFetch fixture (fetchTags (smallNID 2)))
      `shouldBe` Right mempty
  it "reads the file extension" $
    fmap (.extension) (runFetch fixture (fetchFileExtension file))
      `shouldBe` Right (Just "png")
  it "reads the file type" $
    fmap (.fileType) (runFetch fixture (fetchFileType file))
      `shouldBe` Right
        ( Just
            FileTypeInfo
              { mimeType = MimeType {type' = "image", subtype = "png"},
                extension = "png"
              }
        )
  it "has no file type without metadata" $
    fmap (.fileType) (runFetch fixture (fetchFileType (smallNID 2)))
      `shouldBe` Right Nothing
