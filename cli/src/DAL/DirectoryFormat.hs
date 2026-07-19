module DAL.DirectoryFormat where

import DAL.RawGraph
import Models.NID
import MyPrelude

metadataFile :: FilePath -> NID -> FilePath
metadataFile base nid = base </> (unpack (nidRepresentation nid) ++ ".json")

legacyNodeDataFile :: FilePath -> NID -> FilePath
legacyNodeDataFile base nid = nodeDataFile base nid ".data"

nodeDataFile :: FilePath -> NID -> String -> FilePath
nodeDataFile base nid extension = base </> (unpack (nidRepresentation nid) ++ extension)

getMetadataFile :: (RawGraph :> effs) => NID -> Eff effs FilePath
getMetadataFile nid = metadataFile <$> getGraphFilePath <*> pure nid

getNodeDataFile :: (RawGraph :> effs) => NID -> String -> Eff effs FilePath
getNodeDataFile nid extension =
  nodeDataFile <$> getGraphFilePath <*> pure nid <*> pure extension
