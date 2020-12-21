{-# LANGUAGE TemplateHaskell #-}

module Effect.Web where

import MyPrelude
import Network.HTTP.Conduit (simpleHttp)
import UserError

data Web m r where
  GetHttp :: String -> Web m LByteString

makeSem ''Web

runWebIO ::
  (Member (Embed IO) effs, Member (Error UserError) effs) =>
  Sem (Web : effs) ~> Sem effs
runWebIO = interpret $ \(GetHttp s) -> fromExceptionToUserError (simpleHttp s)
