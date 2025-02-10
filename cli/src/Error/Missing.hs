module Error.Missing
  ( module Error.Missing,
    module Polysemy.Error
  )
where

import Error.UserError
import Models.NID
import MyPrelude
import Polysemy.Error hiding (fromException, try)

data Missing = Missing
  { nid :: NID,
    reason :: Maybe Text
  }
  deriving (Show, Eq, Ord)

instance ToUserError Missing where
  toUserError (Missing nid reason) = MissingNode nid reason

throwMissing :: Member (Error Missing) effs => NID -> Sem effs a
throwMissing nid = throw $ Missing nid Nothing
