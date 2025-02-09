module DAL.DecodeJSON where

import MyPrelude
import Data.Aeson (eitherDecode, FromJSON)
import Error.Utils

decodeJSON ::
  (FromJSON a, Member (Error UserError) effs) =>
  ByteString -> Sem effs a
decodeJSON = throwLeft . left AesonDeserialize . eitherDecode . fromStrict
