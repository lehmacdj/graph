module DAL.JSON
  ( decodeJSON,
    encodeJSON,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Error.UserError
import MyPrelude

decodeJSON ::
  (FromJSON a, Member (Error UserError) effs) =>
  ByteString ->
  Sem effs a
decodeJSON = throwLeft . left AesonDeserialize . eitherDecode . fromStrict

encodeJSON :: ToJSON a => a -> LByteString
encodeJSON = encode
