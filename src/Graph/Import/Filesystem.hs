{-# LANGUAGE FlexibleContexts #-}
{-|
   Import directories from filesystems as nodes.
   Data is automatically deduplicated.
   Each file is hashed and stored in a node: by-hash/{hash}.
   Other instances of files with that data are stored as a link to that node
   from their place in the directory tree.
 -}
module Graph.Import.Filesystem where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.SHA

import Control.Arrow ((&&&))

import System.Directory.Tree

import Graph

import Control.Monad.Unique

import Graph.Advanced

adjoinSHA :: ByteString -> (String, ByteString)
adjoinSHA = showDigest . sha512 &&& id

importDirectory
   :: MonadUnique Id m
   => FilePath -> IO (Id -> Graph String -> m (Graph String))
importDirectory base = do
   _ :/ fileTree <- readDirectoryWithL B.readFile base
   if anyFailed fileTree
      then putStrLn $ "error: search failed at least partially, "
                   ++ "missed directories will be ignored"
      else pure ()
   pure . addDirectories $ adjoinSHA <$> fileTree

addDirectories
   :: MonadUnique Id m
   => DirTree (String, ByteString) -> Id -> Graph String -> m (Graph String)
addDirectories dt' root gi = do
   (fileHashes, gi') <- followMkEdgeFrom' "file-hashes" root gi
   let fhid = nidOf fileHashes
       go dt nid g = case dt of
          File fn (sha, contents) -> do
             (n', g') <- followMkEdgeFrom' sha fhid g
             -- TODO: possibly add handling of filename extensions, to categorize
             pure $ insertEdge (Edge nid fn (nidOf n')) g'
          Dir fn [] -> do
             (_, g') <- followMkEdgeFrom' fn nid g
             pure g'
          Dir fn (x:xs) -> do
             (n', g') <- followMkEdgeFrom' fn nid g
             -- dfs down x
             g'' <- go x (nidOf n') g'
             -- then continue evaluating at this point
             go (Dir fn xs) nid g''
          Failed _ _ -> pure g
   go dt' root gi'
