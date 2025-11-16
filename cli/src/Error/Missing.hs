module Error.Missing
  ( module Error.Missing,
    module Effectful.Error.Static,
  )
where

import Effectful
import Effectful.Error.Static hiding (fromException, try)
import Error.UserError
import Models.NID
import MyPrelude

data Missing = Missing
  { nid :: NID,
    reason :: Maybe Text
  }
  deriving (Show, Eq, Ord)

instance ToUserError Missing where
  toUserError (Missing nid reason) = MissingNode nid reason

throwMissing :: (Error Missing :> es) => NID -> Eff es a
throwMissing nid = throwError $ Missing nid Nothing
