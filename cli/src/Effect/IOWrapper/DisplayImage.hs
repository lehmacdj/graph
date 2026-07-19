{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.DisplayImage where

import MyPrelude
import System.IO.Term.Image

data DisplayImage :: Effect where
  -- | print a LBS as an image
  DisplayImage :: LByteString -> DisplayImage m ()

makeEffect ''DisplayImage

interpretDisplayImageIO ::
  (IOE :> effs) =>
  Eff (DisplayImage : effs) a ->
  Eff effs a
interpretDisplayImageIO = interpret \_ -> \case
  DisplayImage i -> liftIO $ printImage i
