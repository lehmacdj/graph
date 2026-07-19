module Error.Missing
  ( module Error.Missing,
  )
where

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

throwMissing :: (Error Missing :> effs) => NID -> Eff effs a
throwMissing nid = throwError $ Missing nid Nothing
