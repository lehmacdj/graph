{-# LANGUAGE TemplateHaskell #-}

module Effect.Web where

import MyPrelude
import UserError
import Network.Wreq (getWith, defaults, responseBody)
import Control.Lens

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
