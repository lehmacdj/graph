{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.DisplayImage where

import MyPrelude
import System.IO.Term.Image

data DisplayImage m a where
  -- | print a LBS as an image
  DisplayImage :: LByteString -> DisplayImage m ()

makeSem ''DisplayImage

interpretDisplayImageIO ::
  (Member (Embed IO) effs) =>
  Sem (DisplayImage : effs) a ->
  Sem effs a
interpretDisplayImageIO = interpret $ \case
  DisplayImage i -> liftIO $ printImage i
