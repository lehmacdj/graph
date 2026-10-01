{-# LANGUAGE TemplateHaskell #-}

module Effect.IOWrapper.Web
  ( Web (..),
    tryGetHttp,
    getHttp,
    runWebIO,
    describeHttpException,
    test_describeHttpException,
  )
where

import Control.Exception qualified as E
import Error.UserError
import MyPrelude
import Network.HTTP.Conduit
  ( HttpException (..),
    HttpExceptionContent (..),
    defaultRequest,
    responseStatus,
  )
import Network.HTTP.Types.Status (statusCode)
import Network.Wreq (defaults, getWith, responseBody)
import Utils.Testing

data Web :: Effect where
  -- | A failed request is returned rather than thrown: an error thrown by
  -- the interpreter would go to the handler where 'runWebIO' was installed
  -- (ending @ge@) instead of the failing command's.
  TryGetHttp :: URI -> Web m (Either UserError ByteString)

makeEffect ''Web

-- | Download a URL, failing with a readable 'UserError'.
getHttp :: (Members [Web, Error UserError] effs) => URI -> Eff effs ByteString
getHttp = either throwError pure <=< tryGetHttp

runWebIO :: (IOE :> effs) => Eff (Web : effs) ~> Eff effs
runWebIO = interpret \_ -> \case
  TryGetHttp uri -> liftIO do
    let s = renderURIString uri
    let opts = defaults
    E.try (getWith opts s) <&> \case
      Left e -> Left (describeHttpException uri e)
      Right response -> Right . toStrict $ response ^. responseBody

-- | A readable description of a failed download, keeping the raw reason in
-- parentheses for diagnostics (without the full request dump).
describeHttpException :: URI -> HttpException -> UserError
describeHttpException uri e =
  OtherError $
    "Couldn't download "
      ++ renderURI uri
      ++ ": "
      ++ reason
      ++ ". ("
      ++ raw
      ++ ")"
  where
    (reason, raw) = case e of
      HttpExceptionRequest _ content -> (describeContent content, rawContent content)
      InvalidUrlException _ why ->
        ("the URL is invalid: " ++ pack why, "InvalidUrlException")
    describeContent = \case
      StatusCodeException response _ ->
        "the server responded with HTTP status "
          ++ tshow (statusCode (responseStatus response))
      ConnectionFailure _ -> "couldn't connect to the server"
      InternalException inner
        -- https connections fail with crypton-connection's
        -- HostCannotConnect, which isn't a dependency of ours
        | "HostCannotConnect" `isPrefixOf` show inner ->
            "couldn't connect to the server"
        | otherwise -> "the connection failed"
      ConnectionTimeout -> "connecting to the server timed out"
      ResponseTimeout -> "the server took too long to respond"
      TooManyRedirects _ -> "there were too many redirects"
      _ -> "the request failed"
    -- the full Response (with headers and body) is too long to show
    rawContent = \case
      StatusCodeException response _ ->
        "StatusCodeException " ++ tshow (statusCode (responseStatus response))
      content -> tshow content

test_describeHttpException :: TestTree
test_describeHttpException =
  testGroup
    "describeHttpException"
    [ testCase "names the URL and reason, keeping the raw reason" $
        tshow
          ( describeHttpException
              [quri|https://example.com/x|]
              (HttpExceptionRequest defaultRequest ConnectionTimeout)
          )
          @?= "Couldn't download https://example.com/x: connecting to the "
          <> "server timed out. (ConnectionTimeout)"
    ]
