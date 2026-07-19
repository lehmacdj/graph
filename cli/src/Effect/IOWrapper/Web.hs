{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.Web where

import Error.UserError
import MyPrelude
import Network.Wreq (defaults, getWith, responseBody)

data Web :: Effect where
  GetHttp :: URI -> Web m ByteString

makeEffect ''Web

runWebIO ::
  (Members [IOE, Error UserError] effs) =>
  Eff (Web : effs) ~> Eff effs
  GetHttp uri -> embedCatchingErrors do
  let s = renderURIString uri
  let opts = defaults
  response <- getWith opts s
  pure . toStrict $ response ^. responseBody
