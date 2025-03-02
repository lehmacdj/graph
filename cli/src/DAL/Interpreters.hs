module DAL.Interpreters where

import Control.Exception qualified as E
import DAL.RawGraph
import Error.UserError
import MyPrelude
import Polysemy.Error

-- | Function for "unlifting" a computation that uses the RawGraph effect
runRawGraphUserErrorIO ::
  FilePath ->
  Sem [RawGraph, Error UserError, Embed IO, Final IO] a ->
  IO (Either UserError a)
runRawGraphUserErrorIO path =
  runFinal . embedToFinal . errorToIOFinal . runRawGraphWithPath path

newtype UnliftRawGraphUserErrorIO = UnliftRawGraphUserErrorIO
  { unRunRawGraphUserErrorIO ::
      forall a.
      Sem [RawGraph, Error UserError, Embed IO, Final IO] a ->
      IO a
  }

withUnliftIORawGraphUserError ::
  (Members [RawGraph, Error UserError, Embed IO] r) =>
  (UnliftRawGraphUserErrorIO -> IO a) ->
  Sem r a
withUnliftIORawGraphUserError f = do
  graphPath <- getGraphFilePath
  fromException @UserError
    $ f
      ( UnliftRawGraphUserErrorIO $ \action -> do
          result <- runRawGraphUserErrorIO graphPath action
          either E.throw pure result
      )
