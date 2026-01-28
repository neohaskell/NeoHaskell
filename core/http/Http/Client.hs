module Http.Client (
  -- * Request Configuration
  Request (..),
  request,
  withUrl,
  addHeader,
  withTimeout,
  withRedirects,

  -- * Response
  Response (..),

  -- * HTTP Methods
  get,
  post,
  postForm,
  postRaw,
  put,
  patch,
  delete,

  -- * Errors
  Error (..),
) where

import Array (Array)
import Array qualified
import Basics
import Bytes qualified
import Char (Char)
import Data.CaseInsensitive qualified as CI
import Data.Either qualified as GhcEither
import Default (Default (..))
import GHC.Int qualified as GhcInt
import Json qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Maybe qualified
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Simple qualified as HttpSimple
import System.IO qualified as GhcIO
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)


data Request = Request
  { url :: Maybe Text,
    headers :: Map Text Text,
    timeoutSeconds :: Maybe GhcInt.Int,
    -- ^ Request timeout in seconds (default: 10 seconds)
    maxRedirects :: GhcInt.Int
    -- ^ Maximum redirects to follow (default: 0 for SSRF protection)
  }


-- | Redacted Show instance to prevent header/secret leakage in logs.
-- SECURITY: Headers may contain Authorization, cookies, API keys.
-- URL is sanitized to scheme://host:port only (query params may contain secrets).
instance Show Request where
  show req = do
    let urlText = case req.url of
          Nothing -> "<no url>"
          Just u -> Text.toLinkedList (sanitizeUrlText u)
    let timeout = show req.timeoutSeconds
    let redirects = show req.maxRedirects
    [fmt|Request {url = #{urlText}, headers = <REDACTED>, timeoutSeconds = #{timeout}, maxRedirects = #{redirects}}|]


-- | Sanitize URL Text to scheme://host:port only, removing path/query/credentials.
-- SECURITY: Query params may contain state, tokens, or other secrets.
sanitizeUrlText :: Text -> Text
sanitizeUrlText urlText = sanitizeUrl (Text.toLinkedList urlText)


instance Default Request where
  def =
    Request
      { url = Nothing,
        headers = Map.empty,
        timeoutSeconds = Just 10,
        -- ^ Default 10 second timeout for production safety.
        -- Prevents indefinite hangs under load. Override with withTimeout if needed.
        maxRedirects = 0
        -- ^ SECURITY: Default 0 redirects to prevent SSRF bypass.
        -- Attackers can redirect validated URLs to internal IPs (169.254.169.254).
        -- Use withRedirects to explicitly opt-in when needed.
      }


request :: Request
request = def


withUrl :: Text -> Request -> Request
withUrl url options =
  options
    { url = Just url
    }


-- | Set request timeout in seconds.
-- SECURITY: Always set timeouts for external requests to prevent hangs.
-- FAILS FAST: Raises error if seconds <= 0 to catch misuse at call site.
withTimeout :: GhcInt.Int -> Request -> Request
withTimeout seconds options =
  case seconds > 0 of
    True ->
      options
        { timeoutSeconds = Just seconds
        }
    False ->
      panic [fmt|withTimeout requires positive seconds, got: #{seconds}|]


addHeader :: Text -> Text -> Request -> Request
addHeader key value options =
  let newHeaders = options.headers |> Map.set key value
  in  Request
        { url = options.url
        , headers = newHeaders
        , timeoutSeconds = options.timeoutSeconds
        , maxRedirects = options.maxRedirects
        }


-- | Set maximum number of redirects to follow.
-- SECURITY: Default is 0 (no redirects) to prevent SSRF bypass attacks.
-- Only enable redirects for trusted endpoints where redirect targets are known-safe.
-- FAILS FAST: Raises error if count < 0 to catch misuse at call site.
withRedirects :: GhcInt.Int -> Request -> Request
withRedirects count options =
  case count >= 0 of
    True ->
      options
        { maxRedirects = count
        }
    False ->
      panic [fmt|withRedirects requires non-negative count, got: #{count}|]


data Error = Error Text
  deriving (Show)


-- | HTTP response with status code, headers, and parsed body.
--
-- All HTTP methods return this type, allowing access to response metadata.
--
-- @
-- response <- Http.get request
-- if response.statusCode == 200
--   then process response.body
--   else handleError response.statusCode
-- @
data Response body = Response
  { -- | HTTP status code (e.g., 200, 201, 404)
    statusCode :: Int
  , -- | Response headers as key-value pairs
    headers :: Array (Text, Text)
  , -- | Parsed response body
    body :: body
  }
  deriving (Show, Generic)


-- | Convert HttpException to a sanitized error message.
-- SECURITY: Never include request body or headers in error messages,
-- as they may contain secrets (client_secret, tokens, etc.)
-- InvalidUrlException URLs are sanitized to scheme://host only to prevent
-- leaking credentials in query params or path.
sanitizeHttpError :: HttpClient.HttpException -> Error
sanitizeHttpError exception = do
  let msg = case exception of
        HttpClient.HttpExceptionRequest req content -> do
          let host = toText (show (HttpClient.host req))
          let port = toText (show (HttpClient.port req))
          let category = categorizeException content
          [fmt|HTTP request failed: #{host}:#{port} - #{category}|]
        HttpClient.InvalidUrlException url reason -> do
          -- SECURITY: Sanitize URL to scheme://host only
          -- Full URL might contain credentials in query params
          let sanitizedUrl = sanitizeUrl url
          [fmt|Invalid URL: #{sanitizedUrl} - #{toText reason}|]
  Error msg


-- | Sanitize URL to scheme://host only, removing path/query/credentials.
-- SECURITY: Prevents leaking credentials that might be in query params.
sanitizeUrl :: [Char] -> Text
sanitizeUrl url = do
  case HttpSimple.parseRequest url of
    GhcEither.Left _ -> "<malformed URL>"
    GhcEither.Right req -> do
      let schemeText = case HttpClient.secure req of
            True -> "https://" :: Text
            False -> "http://" :: Text
      let hostText = toText (show (HttpClient.host req))
      let portText = toText (show (HttpClient.port req))
      Text.append schemeText (Text.append hostText (Text.append ":" portText))


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


get ::
  (Json.FromJSON response) =>
  Request ->
  Task Error (Response response)
get options =
  getIO options
    |> Task.fromFailableIO @HttpClient.HttpException
    |> Task.mapError sanitizeHttpError


-- | Internal IO action for GET request (can throw HttpException)
getIO ::
  (Json.FromJSON response) =>
  Request ->
  GhcIO.IO (Response response)
getIO options = do
  baseReq <- parseRequestUrl options
  let req = applyRequestOptions options baseReq
  httpResponse <- HttpSimple.httpJSON req
  pure (extractResponse httpResponse)


-- | Performs a POST request with JSON body.
post ::
  (Json.FromJSON response, Json.ToJSON requestBody) =>
  Request ->
  requestBody ->
  Task Error (Response response)
post options body =
  postIO options body
    |> Task.fromFailableIO @HttpClient.HttpException
    |> Task.mapError sanitizeHttpError


-- | Internal IO action for POST request (can throw HttpException)
postIO ::
  (Json.FromJSON response, Json.ToJSON requestBody) =>
  Request ->
  requestBody ->
  GhcIO.IO (Response response)
postIO options body = do
  baseReq <- parseRequestUrl options
  let withBody =
        baseReq
          |> HttpSimple.setRequestMethod "POST"
          |> HttpSimple.setRequestBodyJSON body
  let req = applyRequestOptions options withBody
  httpResponse <- HttpSimple.httpJSON req
  pure (extractResponse httpResponse)


-- | Performs a POST request with form-urlencoded body.
--
-- Used for OAuth2 token endpoints which require @application/x-www-form-urlencoded@.
--
-- @
-- Http.request
--   |> Http.withUrl "https://api.example.com/oauth/token"
--   |> Http.postForm
--       [ ("grant_type", "authorization_code")
--       , ("code", authCode)
--       , ("redirect_uri", redirectUri)
--       ]
-- @
postForm ::
  forall response.
  (Json.FromJSON response) =>
  Request ->
  Array (Text, Text) ->
  Task Error (Response response)
postForm options formParams =
  postFormIO options formParams
    |> Task.fromFailableIO @HttpClient.HttpException
    |> Task.mapError sanitizeHttpError


-- | Internal IO action for POST form request (can throw HttpException)
postFormIO ::
  forall response.
  (Json.FromJSON response) =>
  Request ->
  Array (Text, Text) ->
  GhcIO.IO (Response response)
postFormIO options formParams = do
  baseReq <- parseRequestUrl options
  let formData =
        formParams
          |> Array.toLinkedList
          |> fmap (\(key, value) -> (Text.toBytes key |> Bytes.unwrap, Text.toBytes value |> Bytes.unwrap))
  let withForm =
        baseReq
          |> HttpSimple.setRequestMethod "POST"
          |> HttpSimple.setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
          |> HttpSimple.setRequestBodyURLEncoded formData
  let req = applyRequestOptions options withForm
  httpResponse <- HttpSimple.httpJSON req
  pure (extractResponse httpResponse)


-- | Performs a POST request with raw body.
--
-- Used for XML, plain text, or other non-JSON content types.
--
-- @
-- Http.request
--   |> Http.withUrl "https://api.example.com/orders"
--   |> Http.postRaw "application/xml" "<order><id>123</id></order>"
-- @
postRaw ::
  forall response.
  (Json.FromJSON response) =>
  Request ->
  Text ->  -- ^ Content-Type
  Text ->  -- ^ Body content
  Task Error (Response response)
postRaw options contentType bodyContent =
  postRawIO options contentType bodyContent
    |> Task.fromFailableIO @HttpClient.HttpException
    |> Task.mapError sanitizeHttpError


-- | Internal IO action for POST raw request
postRawIO ::
  forall response.
  (Json.FromJSON response) =>
  Request ->
  Text ->
  Text ->
  GhcIO.IO (Response response)
postRawIO options contentType bodyContent = do
  baseReq <- parseRequestUrl options
  let withBody =
        baseReq
          |> HttpSimple.setRequestMethod "POST"
          |> HttpSimple.setRequestHeader "Content-Type" [Text.convert contentType]
          |> HttpSimple.setRequestBodyLBS (bodyContent |> Text.toBytes |> Bytes.toLazyLegacy)
  let req = applyRequestOptions options withBody
  httpResponse <- HttpSimple.httpJSON req
  pure (extractResponse httpResponse)


-- | Performs a PUT request with JSON body.
--
-- PUT replaces the entire resource at the target URL.
--
-- @
-- Http.request
--   |> Http.withUrl "https://api.example.com/users/123"
--   |> Http.put { name = "New Name", email = "new@example.com" }
-- @
put ::
  (Json.FromJSON response, Json.ToJSON requestBody) =>
  Request ->
  requestBody ->
  Task Error (Response response)
put options body =
  putIO options body
    |> Task.fromFailableIO @HttpClient.HttpException
    |> Task.mapError sanitizeHttpError


-- | Internal IO action for PUT request
putIO ::
  (Json.FromJSON response, Json.ToJSON requestBody) =>
  Request ->
  requestBody ->
  GhcIO.IO (Response response)
putIO options body = do
  baseReq <- parseRequestUrl options
  let withBody =
        baseReq
          |> HttpSimple.setRequestMethod "PUT"
          |> HttpSimple.setRequestBodyJSON body
  let req = applyRequestOptions options withBody
  httpResponse <- HttpSimple.httpJSON req
  pure (extractResponse httpResponse)


-- | Performs a PATCH request with JSON body.
--
-- PATCH partially updates the resource at the target URL.
--
-- @
-- Http.request
--   |> Http.withUrl "https://api.example.com/orders/123"
--   |> Http.patch { status = "shipped" }
-- @
patch ::
  (Json.FromJSON response, Json.ToJSON requestBody) =>
  Request ->
  requestBody ->
  Task Error (Response response)
patch options body =
  patchIO options body
    |> Task.fromFailableIO @HttpClient.HttpException
    |> Task.mapError sanitizeHttpError


-- | Internal IO action for PATCH request
patchIO ::
  (Json.FromJSON response, Json.ToJSON requestBody) =>
  Request ->
  requestBody ->
  GhcIO.IO (Response response)
patchIO options body = do
  baseReq <- parseRequestUrl options
  let withBody =
        baseReq
          |> HttpSimple.setRequestMethod "PATCH"
          |> HttpSimple.setRequestBodyJSON body
  let req = applyRequestOptions options withBody
  httpResponse <- HttpSimple.httpJSON req
  pure (extractResponse httpResponse)


-- | Performs a DELETE request.
--
-- DELETE removes the resource at the target URL.
--
-- @
-- Http.request
--   |> Http.withUrl "https://api.example.com/subscriptions/123"
--   |> Http.delete
-- @
delete ::
  (Json.FromJSON response) =>
  Request ->
  Task Error (Response response)
delete options =
  deleteIO options
    |> Task.fromFailableIO @HttpClient.HttpException
    |> Task.mapError sanitizeHttpError


-- | Internal IO action for DELETE request
deleteIO ::
  (Json.FromJSON response) =>
  Request ->
  GhcIO.IO (Response response)
deleteIO options = do
  baseReq <- parseRequestUrl options
  let withMethod =
        baseReq
          |> HttpSimple.setRequestMethod "DELETE"
  let req = applyRequestOptions options withMethod
  httpResponse <- HttpSimple.httpJSON req
  pure (extractResponse httpResponse)


-- ============================================================================
-- Internal helpers
-- ============================================================================

-- | Extract status code, headers, and body from HTTP response.
extractResponse ::
  forall body.
  HttpSimple.Response body ->
  Response body
extractResponse httpResponse = Response
  { statusCode = HttpSimple.getResponseStatusCode httpResponse
  , headers = extractHeaders httpResponse
  , body = HttpSimple.getResponseBody httpResponse
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


-- | Parse the URL from request options into an http-client Request.
parseRequestUrl :: Request -> GhcIO.IO HttpClient.Request
parseRequestUrl options = do
  let url = options.url |> Maybe.withDefault (panic "url is required")
  Text.toLinkedList url |> HttpSimple.parseRequest


-- | Apply common request options: headers, timeout, redirect limit, and proxy settings.
-- SECURITY:
-- - Redirect limit defaults to 0 for SSRF protection
-- - Proxies explicitly disabled to prevent SSRF via HTTP_PROXY/HTTPS_PROXY env vars
applyRequestOptions :: Request -> HttpClient.Request -> HttpClient.Request
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
  -- SECURITY: Apply redirect limit (default 0 for SSRF protection)
  -- SECURITY: Explicitly disable proxy to prevent SSRF via HTTP_PROXY/HTTPS_PROXY env vars
  -- This prevents InvalidProxyEnvironmentVariable and InvalidProxySettings errors
  -- and ensures requests go directly to the validated endpoint.
  withTimeout
    { HttpClient.redirectCount = options.maxRedirects,
      HttpClient.proxy = Nothing
    }