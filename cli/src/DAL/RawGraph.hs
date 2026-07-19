{-# LANGUAGE TemplateHaskell #-}

module DAL.RawGraph where

import Effect.Input
import MyPrelude

-- | Effect for getting the filepath of the graph; useful for doing low level
-- operations together with Graph.Serialize* modules.
data RawGraph :: Effect where
  GetGraphFilePath :: RawGraph m FilePath

makeEffect ''RawGraph

runRawGraphAsInput ::
  (Input FilePath :> r) => Eff (RawGraph : r) a -> Eff r a
runRawGraphAsInput = interpret \_ -> \case
  GetGraphFilePath -> input

runRawGraphWithPath :: FilePath -> Eff (RawGraph : r) a -> Eff r a
runRawGraphWithPath p = reinterpret (runInputConst p) \_ -> \case
  GetGraphFilePath -> input
