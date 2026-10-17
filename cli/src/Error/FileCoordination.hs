-- | The error that file coordination gives, apart from
-- "System.FileCoordination", which calls the system for it: so that
-- 'Error.UserError.UserError' can carry one without importing file
-- coordination.
module Error.FileCoordination (NSErrorException (..)) where

import MyPrelude

-- | An error that file coordination gave: its domain and code as they are,
-- and what it means in words
data NSErrorException = NSErrorException
  { domain :: Text,
    code :: Int,
    description :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData, Exception)
