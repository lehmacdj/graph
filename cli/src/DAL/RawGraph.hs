{-# LANGUAGE TemplateHaskell #-}

module DAL.RawGraph where

import MyPrelude
import Polysemy.Input

-- | Effect for getting the filepath of the graph; useful for doing low level
-- operations together with Graph.Serialize* modules.
data RawGraph m a where
  GetGraphFilePath :: RawGraph m FilePath

makeSem ''RawGraph

runRawGraphAsInput ::
  Member (Input FilePath) r => Sem (RawGraph : r) a -> Sem r a
runRawGraphAsInput = transform @_ @(Input FilePath) (\GetGraphFilePath -> Input)

runRawGraphWithPath :: FilePath -> Sem (RawGraph : r) a -> Sem r a
runRawGraphWithPath p = runInputConst p . runRawGraphAsInput . raiseUnder
