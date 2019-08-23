{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.Web where

import ClassyPrelude

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Effect.Throw

import Network.HTTP.Conduit (simpleHttp)

data Web r where
  GetHttp :: String -> Web LByteString
makeEffect ''Web

runWebIO
  :: (LastMember m effs, MonadIO m, Member Throw effs)
  => Eff (Web : effs) ~> Eff effs
runWebIO = interpret $ \case
  GetHttp s -> do
    r <- liftIO $ try (simpleHttp s)
    case r of
      Left e -> throwErr (WebError s e)
      Right x -> pure x
