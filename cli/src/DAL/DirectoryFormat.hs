module DAL.DirectoryFormat where

import MyPrelude
import Models.NID
import Effect.RawGraph

metadataFile :: FilePath -> NID -> FilePath
metadataFile base nid = base </> (show nid ++ ".json")

legacyNodeDataFile :: FilePath -> NID -> FilePath
legacyNodeDataFile base nid = nodeDataFile base nid ".data"

nodeDataFile :: FilePath -> NID -> String -> FilePath
nodeDataFile base nid extension = base </> (show nid ++ extension)

getMetadataFile :: Member RawGraph effs => NID -> Sem effs FilePath
getMetadataFile nid = metadataFile <$> getGraphFilePath <*> pure nid

getNodeDataFile :: Member RawGraph effs => NID -> String -> Sem effs FilePath
getNodeDataFile nid extension =
  nodeDataFile <$> getGraphFilePath <*> pure nid <*> pure extension
