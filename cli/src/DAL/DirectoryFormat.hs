module DAL.DirectoryFormat where

import MyPrelude
import Models.NID
import Effect.RawGraph

metadataFile :: FilePath -> NID -> FilePath
metadataFile base nid = base </> (show nid ++ ".json")

nodeDataFile :: FilePath -> NID -> FilePath
nodeDataFile base nid = base </> (show nid ++ ".data")

getMetadataFile :: Member RawGraph effs => NID -> Sem effs FilePath
getMetadataFile nid = metadataFile <$> getGraphFilePath <*> pure nid

getNodeDataFile :: Member RawGraph effs => NID -> Sem effs FilePath
getNodeDataFile nid = nodeDataFile <$> getGraphFilePath <*> pure nid
