module Http.Client.Internal
  ( -- * Internal HTTP access
    -- | WARNING: Does NOT enforce HTTPS. Use 'Http.Client.getSecure' for external APIs.
    -- This module is for internal use only (e.g., localhost OAuth2 discovery per ADR-0018).
    getRaw,
  )
where

import Array (Array)
import Array qualified
import Basics
import Bytes (Bytes)
import Bytes qualified
import Data.ByteString (ByteString)
import Data.CaseInsensitive qualified as CI
import Data.Either qualified as GhcEither
import Http.Client (Request (..), Response (..))
import Http.Client qualified as Http
import Log qualified
import Map qualified
import Maybe (Maybe (..))
import Maybe qualified
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Simple qualified as HttpSimple
import Network.HTTP.Simple (setRequestIgnoreStatus)
import System.IO qualified as GhcIO
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)


-- | Performs a GET request without JSON decoding, returning raw Bytes.
-- Unlike 'Http.Client.get', this does NOT throw on non-2xx status codes because it uses
-- setRequestIgnoreStatus to disable http-conduit's default status-code exception behavior.
-- Use this when you need to inspect status codes before decoding (e.g., for 401/429 handling).
--
-- WARNING: Does NOT enforce HTTPS. For external APIs, use 'Http.Client.getSecure' instead.
-- This function exists for trusted internal callers (e.g., localhost OAuth2 discovery).
getRaw ::
  Http.Request ->
  Task Http.Error (Http.Response Bytes)
getRaw options = do
  Log.debug "HTTP GET (raw/internal)" |> Task.ignoreError
  response <- getRawInternalIO options
    |> Task.fromFailableIO @HttpClient.HttpException
    |> Task.mapError sanitizeInternalHttpError
  case options.maxResponseBytes of
    Nothing -> Task.yield response
    Just maxBytes ->
      case response.body |> Bytes.length |> (\len -> len > maxBytes) of
        True -> Task.throw (Http.ResponseTooLarge maxBytes)
        False -> Task.yield response


-- | Internal IO action for raw GET request
getRawInternalIO ::
  Http.Request ->
  GhcIO.IO (Http.Response Bytes)
getRawInternalIO options = do
  baseReq <- parseRequestUrl options
  let req =
        baseReq
          |> applyRequestOptions options
          |> setRequestIgnoreStatus
  httpResponse <- HttpSimple.httpBS req
  pure (extractResponseBytes httpResponse)


-- | Parse the URL from request options into an http-client Request.
parseRequestUrl :: Http.Request -> GhcIO.IO HttpClient.Request
parseRequestUrl options = do
  let url = options.url |> Maybe.withDefault (panic "url is required")
  Text.toLinkedList url |> HttpSimple.parseRequest


-- | Apply common request options: headers, timeout, redirect limit, and proxy settings.
-- SECURITY: Proxies disabled to prevent SSRF via HTTP_PROXY/HTTPS_PROXY env vars
applyRequestOptions :: Http.Request -> HttpClient.Request -> HttpClient.Request
applyRequestOptions options baseReq = do
  let withHeaders =
        options.headers
          |> Map.reduce baseReq (\key value acc ->
            HttpSimple.addRequestHeader (Text.convert key) (Text.convert value) acc)
  let withTimeout = case options.timeoutSeconds of
        Nothing -> withHeaders
        Just seconds -> do
          let microseconds = seconds * 1000000
          HttpSimple.setRequestResponseTimeout
            (HttpClient.responseTimeoutMicro microseconds)
            withHeaders
  withTimeout
    { HttpClient.redirectCount = options.maxRedirects,
      HttpClient.proxy = Nothing
    }


-- | Extract Response with Bytes body from http-client Response
extractResponseBytes :: HttpSimple.Response ByteString -> Http.Response Bytes
extractResponseBytes httpResponse =
  Http.Response
    { statusCode = HttpSimple.getResponseStatusCode httpResponse
    , headers = extractHeaders httpResponse
    , body = Bytes.fromLegacy (HttpSimple.getResponseBody httpResponse)
    }


-- | Extract headers from HTTP response as Array of (Text, Text).
extractHeaders :: HttpSimple.Response body -> Array (Text, Text)
extractHeaders httpResponse =
  HttpSimple.getResponseHeaders httpResponse
    |> fmap (\(name, value) ->
        ( name |> CI.original |> Bytes.fromLegacy |> Text.fromBytes
        , value |> Bytes.fromLegacy |> Text.fromBytes
        ))
    |> Array.fromLinkedList


-- | Convert HttpException to a sanitized error message for internal callers.
-- SECURITY: Does not include URL path/query in error messages to prevent credential leakage.
sanitizeInternalHttpError :: HttpClient.HttpException -> Http.Error
sanitizeInternalHttpError exception = do
  let msg = case exception of
        HttpClient.HttpExceptionRequest req content -> do
          let host = toText (show (HttpClient.host req))
          let port = toText (show (HttpClient.port req))
          [fmt|HTTP request failed: #{host}:#{port} - #{categorizeException content}|]
        HttpClient.InvalidUrlException url _reason -> do
          -- Truncate URL to scheme://host only
          let truncated = case HttpSimple.parseRequest url of
                GhcEither.Left _ -> "<malformed URL>"
                GhcEither.Right req -> do
                  let scheme = case HttpClient.secure req of
                        True -> "https://" :: Text
                        False -> "http://" :: Text
                  let host = toText (show (HttpClient.host req))
                  Text.append scheme host
          [fmt|Invalid URL: #{truncated}|]
  Http.Error msg


-- | Categorize HTTP exception content without revealing sensitive details
categorizeException :: HttpClient.HttpExceptionContent -> Text
categorizeException content = case content of
  HttpClient.StatusCodeException _ _ -> "unexpected status code"
  HttpClient.TooManyRedirects _ -> "too many redirects"
  HttpClient.OverlongHeaders -> "overlong headers"
  HttpClient.ResponseTimeout -> "response timeout"
  HttpClient.ConnectionTimeout -> "connection timeout"
  HttpClient.ConnectionFailure _ -> "connection failed"
  HttpClient.InvalidStatusLine _ -> "invalid status line"
  HttpClient.InvalidHeader _ -> "invalid header"
  HttpClient.InvalidRequestHeader _ -> "invalid request header"
  HttpClient.InternalException _ -> "internal error"
  HttpClient.ProxyConnectException _ _ _ -> "proxy connection error"
  HttpClient.NoResponseDataReceived -> "no response data"
  HttpClient.TlsNotSupported -> "TLS not supported"
  HttpClient.WrongRequestBodyStreamSize _ _ -> "wrong request body size"
  HttpClient.ResponseBodyTooShort _ _ -> "response body too short"
  HttpClient.InvalidChunkHeaders -> "invalid chunk headers"
  HttpClient.IncompleteHeaders -> "incomplete headers"
  HttpClient.InvalidDestinationHost _ -> "invalid destination host"
  HttpClient.HttpZlibException _ -> "decompression error"
  HttpClient.InvalidProxyEnvironmentVariable _ _ -> "invalid proxy config"
  HttpClient.ConnectionClosed -> "connection closed"
  HttpClient.InvalidProxySettings _ -> "invalid proxy settings"
  HttpClient.TooManyHeaderFields -> "too many header fields"
