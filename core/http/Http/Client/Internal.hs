{-| Byte-level HTTP requests for trusted framework callers.
Every function returns the status code, the headers, and the undecoded body of
any response, including 4xx and 5xx. No function here enforces HTTPS, so the
caller must validate the endpoint first.

'getRaw' and 'sendRaw' reach trusted loopback endpoints and ignore environment
proxies. 'postFormRaw' is the status-preserving twin of 'Http.Client.postForm'
for callers that must read the status before they decode the body, such as the
OAuth2 token requests in "Auth.OAuth2.Client".
-}
module Http.Client.Internal (
  -- * Internal HTTP access

  -- | WARNING: Does NOT enforce HTTPS. Use 'Http.Client.getSecure' for external APIs.
  -- This module is for internal use only (e.g., localhost OAuth2 discovery per ADR-0018).
  getRaw,
  sendRaw,

  -- * Status-preserving form requests
  postFormRaw,
) where

import Array (Array)
import Array qualified
import Basics
import Bytes (Bytes)
import Bytes qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified as GhcBS
import Data.CaseInsensitive qualified as CI
import Data.Either qualified as GhcEither
import Http.Client (Method, Request (..), Response (..))
import Http.Client qualified as Http
import LinkedList qualified
import Log qualified
import Map qualified
import Maybe (Maybe (..))
import Maybe qualified
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpTls
import Network.HTTP.Simple (setRequestIgnoreStatus)
import Network.HTTP.Simple qualified as HttpSimple
import Result (Result (..))
import System.IO qualified as GhcIO
import System.IO.Unsafe qualified as GhcUnsafe
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
getRaw options = sendRaw Http.Get options Bytes.empty


-- | Performs a byte-level request with any 'Method' without enforcing HTTPS.
-- The plain-HTTP twin of 'Http.Client.sendSecure': same non-throwing status
-- handling, same response-size limit, same redirect/proxy defaults.
--
-- WARNING: Does NOT enforce HTTPS. Only for trusted loopback endpoints
-- (local object-store fixtures, OAuth2 discovery per ADR-0018).
sendRaw ::
  Method ->
  Http.Request ->
  Bytes ->
  Task Http.Error (Http.Response Bytes)
sendRaw method options body = do
  let verb = Http.methodName method
  Log.debug [fmt|HTTP #{verb} (raw/internal)|] |> Task.ignoreError
  result <- sendRawInternal method options body
  case result of
    Err error -> Task.throw error
    Ok response -> Task.yield response


-- | Performs a form-urlencoded POST and returns the response without decoding it.
-- The status-preserving twin of 'Http.Client.postForm': it sends the same
-- method, @Content-Type@ and field encoding through the same process-wide
-- connection manager. TLS verification and environment-proxy handling are
-- therefore those of 'Http.Client.postForm', not those of 'sendRaw'.
-- It honors the request headers, the response timeout and 'maxRedirects'.
--
-- Unlike 'Http.Client.postForm', this never throws on a non-2xx status and never
-- parses the body, so a caller can classify the status first. It sends no
-- @Accept@ header of its own; add one with 'Http.Client.addHeader'. The body is
-- bounded by 'maxResponseBytes' like every other byte-level request. Transport
-- failures are sanitized: the error never carries the form fields, the request
-- headers, or the URL path and query.
--
-- WARNING: Does NOT enforce HTTPS. The caller must validate the endpoint first.
--
-- @
-- let request = Http.request |> Http.withUrl tokenEndpoint
-- response <- HttpInternal.postFormRaw request [("grant_type", "refresh_token")]
-- if response.statusCode == 200
--   then decodeTokens response.body
--   else decodeProviderError response.statusCode response.body
-- @
postFormRaw ::
  Http.Request ->
  Array (Text, Text) ->
  Task Http.Error (Http.Response Bytes)
postFormRaw options formParams = do
  Log.debug "HTTP POST (form, raw/internal)" |> Task.ignoreError
  manager <- HttpTls.getGlobalManager |> Task.fromIO
  let buildRequest baseReq =
        baseReq
          |> applyFormBody formParams
          |> applyRequestOptions options
  result <- sendBounded manager Http.Post options buildRequest
  case result of
    Err error -> Task.throw error
    Ok response -> Task.yield response


{-# NOINLINE rawManager #-}
-- | Isolate direct byte requests from the library global proxy-aware manager.
rawManager :: HttpClient.Manager
rawManager = GhcUnsafe.unsafePerformIO do
  HttpClient.newManager (HttpClient.managerSetProxy HttpClient.noProxy HttpTls.tlsManagerSettings)


-- | Send a raw body through the proxy-free manager.
sendRawInternal :: Method -> Http.Request -> Bytes -> Task Http.Error (Result Http.Error (Http.Response Bytes))
sendRawInternal method options body = do
  let buildRequest baseReq =
        baseReq
          |> applyRequestOptions options
          |> Http.applyMethodAndBody method body
  sendBounded rawManager method options buildRequest


-- | Own the response until its bounded body has been read or rejected.
sendBounded ::
  HttpClient.Manager ->
  Method ->
  Http.Request ->
  (HttpClient.Request -> HttpClient.Request) ->
  Task Http.Error (Result Http.Error (Http.Response Bytes))
sendBounded manager method options buildRequest = Task.withCancellationDeferred \restore -> do
  baseReq <-
    parseRequestUrl options
      |> Task.fromFailableIO @HttpClient.HttpException
      |> Task.mapError sanitizeInternalHttpError
  let req =
        baseReq
          |> buildRequest
          |> setRequestIgnoreStatus
  response <-
    HttpClient.responseOpen req manager
      |> Task.fromFailableIO @HttpClient.HttpException
      |> Task.mapError sanitizeInternalHttpError
  readLimitedResponse method options response
    |> Task.fromFailableIO @HttpClient.HttpException
    |> Task.mapError sanitizeInternalHttpError
    |> restore
    |> Task.finally (HttpClient.responseClose response |> Task.fromIO)


-- | Set the POST method, the form @Content-Type@ and the url-encoded fields.
-- Mirrors the private encoding of 'Http.Client.postForm'. Kept private here to
-- avoid widening the public Http.Client API for plumbing.
applyFormBody :: Array (Text, Text) -> HttpClient.Request -> HttpClient.Request
applyFormBody formParams baseReq = do
  let formData =
        formParams
          |> Array.toLinkedList
          |> fmap (\(key, value) -> (Text.toBytes key |> Bytes.unwrap, Text.toBytes value |> Bytes.unwrap))
  baseReq
    |> HttpSimple.setRequestMethod "POST"
    |> HttpSimple.setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
    |> HttpSimple.setRequestBodyURLEncoded formData


-- | HEAD bypasses advertised length; delivered chunks still obey the byte cap.
readLimitedResponse ::
  Method ->
  Http.Request ->
  HttpClient.Response HttpClient.BodyReader ->
  GhcIO.IO (Result Http.Error (Http.Response Bytes))
readLimitedResponse method options response = do
  let declaredLimit = case method of
        Http.Head -> Nothing
        _ -> Http.checkContentLengthHeaderLimit options.maxResponseBytes (HttpClient.responseHeaders response)
  case declaredLimit of
    Just maxBytes -> pure (Err (Http.ResponseTooLarge maxBytes))
    Nothing -> do
      bodyResult <- readBodyChunks options.maxResponseBytes (HttpClient.responseBody response) 0 []
      pure (bodyResult |> fmap (\body -> extractResponseBytes (response {HttpClient.responseBody = body})))


-- Kept private here to avoid widening the public Http.Client API for plumbing.
readBodyChunks :: Maybe Int -> HttpClient.BodyReader -> Int -> [ByteString] -> GhcIO.IO (Result Http.Error ByteString)
readBodyChunks limit reader received chunks = do
  chunk <- HttpClient.brRead reader
  if GhcBS.null chunk
    then pure (Ok (GhcBS.concat (LinkedList.reverse chunks)))
    else do
      let chunkBytes = GhcBS.length chunk
      case limit of
        Just maxBytes | chunkBytes > maxBytes - received -> pure (Err (Http.ResponseTooLarge maxBytes))
        _ -> readBodyChunks limit reader (received + chunkBytes) (chunk : chunks)


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
          |> Map.reduce
            baseReq
            ( \key value acc ->
                HttpSimple.addRequestHeader (Text.convert key) (Text.convert value) acc
            )
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
    { statusCode = HttpSimple.getResponseStatusCode httpResponse,
      headers = extractHeaders httpResponse,
      body = Bytes.fromLegacy (HttpSimple.getResponseBody httpResponse)
    }


-- | Extract headers from HTTP response as Array of (Text, Text).
extractHeaders :: HttpSimple.Response body -> Array (Text, Text)
extractHeaders httpResponse =
  HttpSimple.getResponseHeaders httpResponse
    |> fmap
      ( \(name, value) ->
          ( name |> CI.original |> Bytes.fromLegacy |> Text.fromBytes,
            value |> Bytes.fromLegacy |> Text.fromBytes
          )
      )
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
                  let scheme = if HttpClient.secure req
                        then "https://" :: Text
                        else "http://" :: Text
                  let host = toText (show (HttpClient.host req))
                  scheme |> Text.append host
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
