-- | The functions the WebAssembly reactor exports to JavaScript, through
-- GHC's JavaScript FFI. Each takes a string and returns JSON text, which
-- js/graph.mjs parses; "Wasm.Api" says what the JSON is.
--
-- The exports are synchronous: they compute pure functions, and a caller
-- completing as the user types should not wait for a promise. A
-- synchronous export cannot pass a Haskell exception on to JavaScript, so
-- one is returned as @{"error": message}@ instead.
module Main (main) where

import GHC.Wasm.Prim (JSString (..), fromJSString, toJSString)
import MyPrelude
import Wasm.Api (completeJson, parsePathJson)

foreign export javascript "parsePath sync"
  parsePath :: JSString -> IO JSString

foreign export javascript "complete sync"
  complete :: JSString -> IO JSString

parsePath :: JSString -> IO JSString
parsePath = exported parsePathJson

complete :: JSString -> IO JSString
complete = exported completeJson

exported :: (Text -> Text) -> JSString -> IO JSString
exported f s = do
  result <- tryAny (evaluate (force (f (pack (fromJSString s)))))
  pure . toJSString . unpack $ either errorJson id result
  where
    errorJson e =
      decodeUtf8 . toStrict . encodeJSON $
        mapFromList @(Map Text Text) [("error", pack (displayException e))]

-- | Not called: a reactor has no main, but cabal's executable needs one.
main :: IO ()
main = pure ()
