module DAL.JSON (
  decodeJSON,
  encodeJSON,
) where

import MyPrelude
import Data.Aeson (eitherDecode, FromJSON, encode, ToJSON)
import Error.UserError

decodeJSON ::
  (FromJSON a, Member (Error UserError) effs) =>
  ByteString -> Sem effs a
decodeJSON = throwLeft . left AesonDeserialize . eitherDecode . fromStrict

encodeJSON :: ToJSON a => a -> LByteString
encodeJSON = encode
