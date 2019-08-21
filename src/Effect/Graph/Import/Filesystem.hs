{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-|
   Import directories from filesystems as nodes.
   Data is automatically deduplicated.
   Each file is hashed and stored in a node: by-hash/{hash}.
   Other instances of files with that data are stored as a link to that node
   from their place in the directory tree.
 -}
module Effect.Graph.Import.Filesystem where

import ClassyPrelude

import Data.Digest.Pure.SHA

import System.Directory.Tree hiding (readDirectory)

import Graph hiding (insertEdge)

import Control.Monad.Freer
import Control.Monad.Freer.Fresh
import Effect.Graph
import Effect.Graph.Advanced
import Effect.Filesystem
import Effect.Console
import Effect.Throw

import Effect.Graph.Import.ByteString

computeSHA :: LByteString -> String
computeSHA = showDigest . sha512

importDirectory
   :: ( Members [Fresh, FileSystemTree, Console, ThrowMissing] effs
      , HasGraph String effs
      )
   => FilePath -> Id -> Eff effs ()
importDirectory base nid = do
   fileTree <- readDirectory base
   if anyFailed fileTree
      then do
         echo $ "error: search failed at least partially, "
                   ++ "missed directories will be ignored"
         echo "the failed files are:"
         echo . show $ failures fileTree
      else pure ()
   addDirectories fileTree nid

addDirectories
   :: (Members [Fresh, ThrowMissing] effs, HasGraph String effs)
   => DirTree LByteString -> Id -> Eff effs ()
addDirectories dt' root = do
   fileHashes <- root `transitionsVia` "file-hashes"
   let
     go dt nid = case dt of
        File fn cs -> do
           nid' <- importData fileHashes cs
           -- TODO: possibly add handling of filename extensions, to categorize
           insertEdge (Edge nid fn nid')
        Dir fn [] -> do
           _ <- nid `transitionsVia` fn
           pure ()
        Dir fn (x:xs) -> do
           nid' <- nid `transitionsVia` fn
           -- dfs down x
           go x nid'
           -- then continue evaluating at this point
           go (Dir fn xs) nid
        Failed _ _ -> pure ()
   go dt' root
