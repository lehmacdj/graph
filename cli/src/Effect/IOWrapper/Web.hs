{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.Web where

import Error.UserError
import MyPrelude
import Network.Wreq (defaults, getWith, responseBody)

data Web m r where
  GetHttp :: URI -> Web m ByteString

makeSem ''Web

runWebIO ::
  (Members [Embed IO, Error UserError] effs) =>
  Sem (Web : effs) ~> Sem effs
runWebIO = interpret $ \(GetHttp uri) -> embedCatchingErrors do
  let s = renderURIString uri
  let opts = defaults
  response <- getWith opts s
  pure . toStrict $ response ^. responseBody
