module DAL.Interpreters where

import Control.Exception qualified as E
import DAL.RawGraph
import Effectful
import Effectful.Error.Static
import Error.UserError
import MyPrelude

type RawGraphUserErrorIO = [RawGraph, Error UserError, IOE]

-- | Function for "unlifting" a computation that uses the RawGraph effect
runRawGraphUserErrorIO ::
  FilePath ->
  Eff RawGraphUserErrorIO a ->
  IO (Either UserError a)
runRawGraphUserErrorIO path =
  runEff . runError . runRawGraphWithPath path

withUnliftIORawGraphUserError ::
  (RawGraph :> es, Error UserError :> es, IOE :> es) =>
  (UnliftIO (Eff RawGraphUserErrorIO) -> IO a) ->
  Eff es a
withUnliftIORawGraphUserError f = do
  graphPath <- getGraphFilePath
  fromException @UserError $
    f
      ( UnliftIO $ \action -> do
          result <- runRawGraphUserErrorIO graphPath action
          either E.throw pure result
      )

withUnliftIORawGraphUserError' ::
  (RawGraph :> es, Error UserError :> es, IOE :> es) =>
  (forall x. m x -> Eff RawGraphUserErrorIO x) ->
  (UnliftIO m -> IO a) ->
  Eff es a
withUnliftIORawGraphUserError' extraInterpreters f = do
  graphPath <- getGraphFilePath
  fromException @UserError $
    f
      ( UnliftIO $ \action -> do
          result <- runRawGraphUserErrorIO graphPath $ extraInterpreters action
          either E.throw pure result
      )
