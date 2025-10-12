module DAL.Interpreters where

import Control.Exception qualified as E
import DAL.RawGraph
import Error.UserError
import MyPrelude
import Polysemy.Error

type RawGraphUserErrorIO = [RawGraph, Error UserError, Embed IO, Final IO]

-- | Function for "unlifting" a computation that uses the RawGraph effect
runRawGraphUserErrorIO ::
  FilePath ->
  Sem RawGraphUserErrorIO a ->
  IO (Either UserError a)
runRawGraphUserErrorIO path =
  runFinal . embedToFinal . errorToIOFinal . runRawGraphWithPath path

withUnliftIORawGraphUserError ::
  (Members [RawGraph, Error UserError, Embed IO] r) =>
  (UnliftIO (Sem RawGraphUserErrorIO) -> IO a) ->
  Sem r a
withUnliftIORawGraphUserError f = do
  graphPath <- getGraphFilePath
  fromException @UserError $
    f
      ( UnliftIO $ \action -> do
          result <- runRawGraphUserErrorIO graphPath action
          either E.throw pure result
      )

withUnliftIORawGraphUserError' ::
  (Members [RawGraph, Error UserError, Embed IO] r) =>
  (forall x. m x -> Sem RawGraphUserErrorIO x) ->
  (UnliftIO m -> IO a) ->
  Sem r a
withUnliftIORawGraphUserError' extraInterpreters f = do
  graphPath <- getGraphFilePath
  fromException @UserError $
    f
      ( UnliftIO $ \action -> do
          result <- runRawGraphUserErrorIO graphPath $ extraInterpreters action
          either E.throw pure result
      )
