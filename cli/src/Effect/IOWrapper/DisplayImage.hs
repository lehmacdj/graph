{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.DisplayImage where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import MyPrelude
import System.IO.Term.Image

data DisplayImage :: Effect where
  -- | print a LBS as an image
  DisplayImage :: LByteString -> DisplayImage m ()

makeEffect ''DisplayImage

interpretDisplayImageIO ::
  (IOE :> es) =>
  Eff (DisplayImage : es) a ->
  Eff es a
interpretDisplayImageIO = interpret $ \_ -> \case
  DisplayImage i -> liftIO $ printImage i
