{-# LANGUAGE TemplateHaskell #-}

module Effect.Web where

import Error.Utils
import MyPrelude
import Network.Wreq (defaults, getWith, responseBody)

data Web m r where
  GetHttp :: String -> Web m ByteString

makeSem ''Web

runWebIO ::
  (Members [Embed IO, Error UserError] effs) =>
  Sem (Web : effs) ~> Sem effs
runWebIO = interpret $ \(GetHttp s) -> fromExceptionToUserError do
  let opts = defaults
  response <- getWith opts s
  pure . toStrict $ response ^. responseBody
