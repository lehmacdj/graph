{-# LANGUAGE TemplateHaskell #-}

module DAL.RawGraph where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.TH
import MyPrelude

-- | Effect for getting the filepath of the graph; useful for doing low level
-- operations together with Graph.Serialize* modules.
data RawGraph :: Effect where
  GetGraphFilePath :: RawGraph m FilePath

makeEffect ''RawGraph

runRawGraphAsReader ::
  (Reader FilePath :> es) => Eff (RawGraph : es) a -> Eff es a
runRawGraphAsReader = interpret $ \_ -> \case
  GetGraphFilePath -> ask

runRawGraphWithPath :: FilePath -> Eff (RawGraph : es) a -> Eff es a
runRawGraphWithPath p = runReader p . runRawGraphAsReader . raiseUnder
