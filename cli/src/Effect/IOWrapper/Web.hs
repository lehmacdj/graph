{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.Web where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.TH
import Error.UserError
import MyPrelude
import Network.Wreq (defaults, getWith, responseBody)

data Web :: Effect where
  GetHttp :: URI -> Web m ByteString

makeEffect ''Web

runWebIO ::
  (IOE :> es, Error UserError :> es) =>
  Eff (Web : es) a ->
  Eff es a
runWebIO = interpret $ \_ -> \case
  GetHttp uri -> embedCatchingErrors do
    let s = renderURIString uri
    let opts = defaults
    response <- getWith opts s
    pure . toStrict $ response ^. responseBody
