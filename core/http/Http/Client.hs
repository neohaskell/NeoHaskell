module Http.Client (
  -- * Request Configuration
  Request (..),
  request,
  withUrl,
  addHeader,
  withTimeout,
  withRedirects,
  withMaxResponseSize,

  -- * Response
  Response (..),

  -- * HTTP Methods
  Method (..),
  methodName,
  get,
  getSecure,
  sendSecure,
  applyMethodAndBody,
  secureTlsSupportedVersions,
  checkContentLengthHeaderLimit,
  -- ^ Enforces HTTPS (rejects http:// URLs). Use for all external API calls.
  -- getRaw is intentionally NOT exported here; use Http.Client.Internal for trusted/localhost callers.
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
import Bytes (Bytes)
import Bytes qualified
import Char (Char)
import Control.Exception qualified as GhcException
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as GhcBSChar
import Data.CaseInsensitive qualified as CI
import Data.Either qualified as GhcEither
import Default (Default (..))
import GHC.Int qualified as GhcInt
import Json qualified
import LinkedList qualified
import Log qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Maybe qualified
import Network.Connection qualified as Connection
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpTls
import Network.HTTP.Simple (setRequestIgnoreStatus)
import Network.HTTP.Simple qualified as HttpSimple
import Network.HTTP.Types (hContentLength)
import Network.HTTP.Types.Status qualified as HttpStatus
import Network.TLS qualified as TLS
import Result (Result (..))
import System.IO qualified as GhcIO
import System.IO.Unsafe qualified as GhcUnsafe
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)


data Request = Request
  { url :: Maybe Text,
    headers :: Map Text Text,
    -- | Request timeout in seconds (default: 10 seconds)
    timeoutSeconds :: Maybe GhcInt.Int,
    -- | Maximum redirects to follow (default: 0 for SSRF protection)
    maxRedirects :: GhcInt.Int,
    -- | Maximum response body size in bytes (default: 10MB).
    -- Both byte-level paths count delivered chunks before retaining them and
    -- close the response on overflow. Transport buffers may hold an extra chunk;
    -- this is not a fixed process-memory bound. JSON methods do not use this cap.
    -- Set to Nothing to disable the limit (not recommended for external APIs).
    maxResponseBytes :: !(Maybe GhcInt.Int)
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
    let maxBytes = show req.maxResponseBytes
    [fmt|Request {url = #{urlText}, headers = <REDACTED>, timeoutSeconds = #{timeout}, maxRedirects = #{redirects}, maxResponseBytes = #{maxBytes}}|]


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
        -- \^ Default 10 second timeout for production safety.
        -- Prevents indefinite hangs under load. Override with withTimeout if needed.
        maxRedirects = 0,
        -- \^ SECURITY: Default 0 redirects to prevent SSRF bypass.
        -- Attackers can redirect validated URLs to internal IPs (169.254.169.254).
        -- Use withRedirects to explicitly opt-in when needed.
        maxResponseBytes = Just (10 * 1024 * 1024)
      }


-- \^ Default 10MB limit to prevent OOM from malicious servers.

-- | An empty request: no URL, no headers, default timeout and limits.
request :: Request
request = def


-- | Set the request URL.
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


-- | Add one header; later additions win on duplicate names.
addHeader :: Text -> Text -> Request -> Request
addHeader key value options =
  let newHeaders = options.headers |> Map.set key value
  in  Request
        { url = options.url,
          headers = newHeaders,
          timeoutSeconds = options.timeoutSeconds,
          maxRedirects = options.maxRedirects,
          maxResponseBytes = options.maxResponseBytes
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


-- | Set the maximum response body size in bytes.
-- Default is 10MB. Use this to override for specific endpoints.
{-# INLINE withMaxResponseSize #-}


-- | Cap byte-level responses while reading; JSON methods do not use this limit.
withMaxResponseSize :: GhcInt.Int -> Request -> Request
withMaxResponseSize maxBytes options =
  case maxBytes > 0 of
    True ->
      options
        { maxResponseBytes = Just maxBytes
        }
    False ->
      panic [fmt|withMaxResponseSize requires positive bytes, got: #{maxBytes}|]


-- | HTTP method for byte-level requests ('sendSecure', 'Http.Client.Internal.sendRaw').
-- A closed set: protocol clients (object stores, webhooks) pick a verb from here
-- instead of spelling the wire token themselves.
data Method = Get | Head | Post | Put | Patch | Delete
  deriving (Eq, Show)


-- | The wire token of a 'Method' ("GET", "HEAD", ...).
methodName :: Method -> Text
methodName method = case method of
  Get -> "GET"
  Head -> "HEAD"
  Post -> "POST"
  Put -> "PUT"
  Patch -> "PATCH"
  Delete -> "DELETE"


data Error
  = Error Text
  | -- | The URL does not use HTTPS. Use getSecure only with https:// URLs.
    -- The URL is sanitized (query params stripped) to prevent secret leakage in logs.
    InvalidUrl Text
  | -- | Response body exceeded the configured size limit.
    -- The Int is the limit in bytes that was exceeded.
    ResponseTooLarge GhcInt.Int
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
    statusCode :: Int,
    -- | Response headers as key-value pairs
    headers :: Array (Text, Text),
    -- | Parsed response body
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
      schemeText |> Text.append hostText |> Text.append ":" |> Text.append portText


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
  HttpClient.ProxyConnectException {} -> "proxy connection error"
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


-- | Extract sanitized host for logging from request options.
requestHost :: Request -> Text
requestHost options = options.url |> Maybe.map sanitizeUrlText |> Maybe.withDefault "<no url>"


-- | GET a JSON document from any URL (plain HTTP allowed; prefer getSecure).
get ::
  (Json.FromJSON response) =>
  Request ->
  Task Error (Response response)
get options = do
  let host = requestHost options
  Log.debug [fmt|HTTP GET #{host}|] |> Task.ignoreError
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


-- | Performs a secure GET request, enforcing HTTPS and TLS 1.2+ minimum.
-- Rejects any URL that does not start with 'https://' with an 'InvalidUrl' error.
-- The error message sanitizes the URL via 'sanitizeUrlText' to prevent secret leakage.
-- Uses a TLS manager pinned to TLS 1.2+ for defense-in-depth.
-- Use 'Http.Client.Internal.getRaw' for trusted localhost callers (e.g., OAuth2 discovery).
secureTlsSupportedVersions :: [TLS.Version]
secureTlsSupportedVersions = [TLS.TLS13, TLS.TLS12]


-- | Build the TLS 1.2+ connection manager shared by every secure request.
secureTlsManager :: GhcIO.IO HttpClient.Manager
secureTlsManager = do
  let tlsSettings =
        Connection.TLSSettingsSimple
          { Connection.settingDisableCertificateValidation = False,
            Connection.settingDisableSession = False,
            Connection.settingUseServerName = True,
            Connection.settingClientSupported =
              TLS.defaultSupported {TLS.supportedVersions = secureTlsSupportedVersions}
          }
  let managerSettings = HttpTls.mkManagerSettings tlsSettings Nothing
  HttpClient.newManager (HttpClient.managerSetProxy HttpClient.noProxy managerSettings)


{-# NOINLINE cachedSecureTlsManager #-}


-- | The process-wide secure manager, built once.
cachedSecureTlsManager :: HttpClient.Manager
cachedSecureTlsManager = GhcUnsafe.unsafePerformIO secureTlsManager


{-# INLINE getSecure #-}


-- | GET raw bytes over HTTPS only; returns the status code instead of throwing on 4xx/5xx.
getSecure ::
  Request ->
  Task Error (Response Bytes)
getSecure options = sendSecure Get options Bytes.empty


-- | Performs a byte-level request with any 'Method', enforcing HTTPS and TLS 1.2+.
--
-- Built for protocol clients (object stores, signed webhooks) that need the
-- status code and headers of every response, including 4xx/5xx: this never
-- throws on a non-2xx status, never follows redirects beyond 'maxRedirects',
-- and never decodes the body. The request body is sent as-is; set
-- @Content-Type@ with 'addHeader'. The response body is bounded by
-- 'maxResponseBytes' exactly like 'getSecure'.
-- Use 'Http.Client.Internal.sendRaw' only for trusted loopback endpoints.
sendSecure ::
  Method ->
  Request ->
  Bytes ->
  Task Error (Response Bytes)
sendSecure method options body = do
  let urlText = options.url |> Maybe.withDefault ""
  case Text.startsWith "https://" urlText of
    False ->
      -- SECURITY: sanitizeUrlText strips query params which may contain secrets
      Task.throw (InvalidUrl (sanitizeUrlText urlText))
    True -> do
      let host = requestHost options
      let verb = methodName method
      Log.debug [fmt|HTTP #{verb} (secure) #{host}|] |> Task.ignoreError
      secureResult <- sendSecureRawIO method options body |> Task.fromIO
      case secureResult of
        Err error -> Task.throw error
        Ok response -> checkResponseSize options response


-- | Internal IO action for a secure byte-level request using TLS 1.2+ only.
sendSecureRawIO :: Method -> Request -> Bytes -> GhcIO.IO (Result Error (Response Bytes))
sendSecureRawIO method options body = do
  httpResult <- GhcException.try @HttpClient.HttpException do
    baseReq <- parseRequestUrl options
    let req =
          baseReq
            |> applyRequestOptions options
            |> applyMethodAndBody method body
            |> setRequestIgnoreStatus
    GhcException.bracket
      (HttpClient.responseOpen req cachedSecureTlsManager)
      HttpClient.responseClose
      (readLimitedResponse method options)
  case httpResult of
    GhcEither.Left httpException -> pure (Err (sanitizeHttpError httpException))
    GhcEither.Right responseResult -> pure responseResult


-- | HEAD advertises an unsent body; other methods retain the early header guard.
readLimitedResponse ::
  Method -> Request -> HttpClient.Response HttpClient.BodyReader -> GhcIO.IO (Result Error (Response Bytes))
readLimitedResponse method options rawResponse = do
  let declaredLimit = case method of
        Head -> Nothing
        _ -> checkContentLengthHeaderLimit options.maxResponseBytes (HttpClient.responseHeaders rawResponse)
  case declaredLimit of
    Just maxBytes -> pure (Err (ResponseTooLarge maxBytes))
    Nothing -> do
      bodyResult <- readBodyFully options.maxResponseBytes (HttpClient.responseBody rawResponse)
      pure (bodyResult |> fmap (extractResponseBytesFromOpenResponse rawResponse))


-- | Set the wire method and the raw request body. Shared with 'Http.Client.Internal'.
applyMethodAndBody :: Method -> Bytes -> HttpClient.Request -> HttpClient.Request
applyMethodAndBody method body req =
  req
    |> HttpSimple.setRequestMethod (methodName method |> Text.convert)
    |> HttpSimple.setRequestBodyLBS (Bytes.toLazyLegacy body)


-- | Stop before retaining a chunk that would exceed the response limit.
readBodyFully :: Maybe GhcInt.Int -> HttpClient.BodyReader -> GhcIO.IO (Result Error ByteString)
readBodyFully limit reader = readBodyChunks limit reader 0 []


-- | Check remaining capacity before retaining a chunk; concatenate once at EOF.
readBodyChunks ::
  Maybe GhcInt.Int -> HttpClient.BodyReader -> GhcInt.Int -> [ByteString] -> GhcIO.IO (Result Error ByteString)
readBodyChunks limit reader received chunks = do
  chunk <- HttpClient.brRead reader
  if GhcBSChar.null chunk
    then pure (Ok (GhcBSChar.concat (LinkedList.reverse chunks)))
    else do
      let chunkBytes = GhcBSChar.length chunk
      case limit of
        Just maxBytes | chunkBytes > maxBytes - received -> pure (Err (ResponseTooLarge maxBytes))
        _ -> readBodyChunks limit reader (received + chunkBytes) (chunk : chunks)


-- | Reject early when @Content-Length@ already exceeds the limit.
-- This advertised-size shortcut precedes the delivered/decompressed chunk count;
-- HEAD bypasses it because its advertised body is not sent.
checkContentLengthHeaderLimit :: Maybe GhcInt.Int -> [(CI.CI ByteString, ByteString)] -> Maybe GhcInt.Int
checkContentLengthHeaderLimit maxResponseLimit responseHeaders =
  case maxResponseLimit of
    Nothing -> Nothing
    Just maxBytes -> do
      let parsedContentLength =
            case findHeaderValue hContentLength responseHeaders of
              Nothing -> Nothing
              Just rawContentLength -> parseContentLengthHeader rawContentLength
      case parsedContentLength of
        Nothing -> Nothing
        Just contentLength ->
          case contentLength > maxBytes of
            True -> Just maxBytes
            False -> Nothing


-- | Parse a @Content-Length@ value, if well-formed.
parseContentLengthHeader :: ByteString -> Maybe GhcInt.Int
parseContentLengthHeader rawContentLength =
  case GhcBSChar.readInt rawContentLength of
    Nothing -> Nothing
    Just (parsedContentLength, remainingBytes) ->
      case GhcBSChar.null remainingBytes && parsedContentLength >= 0 of
        True -> Just parsedContentLength
        False -> Nothing


-- | First value for a header name, if present.
findHeaderValue :: CI.CI ByteString -> [(CI.CI ByteString, ByteString)] -> Maybe ByteString
findHeaderValue headerName headers =
  case headers of
    [] -> Nothing
    (currentName, currentValue) : remainingHeaders ->
      case currentName == headerName of
        True -> Just currentValue
        False -> findHeaderValue headerName remainingHeaders


-- | Drain an open response into bytes under the size limit.
extractResponseBytesFromOpenResponse :: HttpClient.Response HttpClient.BodyReader -> ByteString -> Response Bytes
extractResponseBytesFromOpenResponse rawResponse responseBody =
  Response
    { statusCode = rawResponse |> HttpClient.responseStatus |> HttpStatus.statusCode,
      headers = rawResponse |> HttpClient.responseHeaders |> extractRawHeaders,
      body = responseBody |> Bytes.fromLegacy
    }


-- | Convert wire headers to @(name, value)@ text pairs.
extractRawHeaders :: [(CI.CI ByteString, ByteString)] -> Array (Text, Text)
extractRawHeaders rawHeaders =
  rawHeaders
    |> fmap
      ( \(name, value) ->
          ( name |> CI.original |> Bytes.fromLegacy |> Text.fromBytes,
            value |> Bytes.fromLegacy |> Text.fromBytes
          )
      )
    |> Array.fromLinkedList


-- | Post-read check: validates response body size against the configured limit.
-- Returns ResponseTooLarge if the body exceeds maxResponseBytes.
checkResponseSize :: Request -> Response Bytes -> Task Error (Response Bytes)
checkResponseSize options response =
  case options.maxResponseBytes of
    Nothing -> Task.yield response
    Just maxBytes ->
      case response.body |> Bytes.length |> (\len -> len > maxBytes) of
        True -> Task.throw (ResponseTooLarge maxBytes)
        False -> Task.yield response


-- | Performs a POST request with JSON body.
post ::
  (Json.FromJSON response, Json.ToJSON requestBody) =>
  Request ->
  requestBody ->
  Task Error (Response response)
post options body = do
  let host = requestHost options
  Log.debug [fmt|HTTP POST #{host}|] |> Task.ignoreError
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
postForm options formParams = do
  let host = requestHost options
  Log.debug [fmt|HTTP POST (form) #{host}|] |> Task.ignoreError
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
  -- | Content-Type
  Text ->
  -- | Body content
  Text ->
  Task Error (Response response)
postRaw options contentType bodyContent = do
  let host = requestHost options
  Log.debug [fmt|HTTP POST (raw) #{host}|] |> Task.ignoreError
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
put options body = do
  let host = requestHost options
  Log.debug [fmt|HTTP PUT #{host}|] |> Task.ignoreError
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
patch options body = do
  let host = requestHost options
  Log.debug [fmt|HTTP PATCH #{host}|] |> Task.ignoreError
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
delete options = do
  let host = requestHost options
  Log.debug [fmt|HTTP DELETE #{host}|] |> Task.ignoreError
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
extractResponse httpResponse =
  Response
    { statusCode = HttpSimple.getResponseStatusCode httpResponse,
      headers = extractHeaders httpResponse,
      body = HttpSimple.getResponseBody httpResponse
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
  -- SECURITY: Apply redirect limit (default 0 for SSRF protection)
  -- SECURITY: Explicitly disable proxy to prevent SSRF via HTTP_PROXY/HTTPS_PROXY env vars
  -- This prevents InvalidProxyEnvironmentVariable and InvalidProxySettings errors
  -- and ensures requests go directly to the validated endpoint.
  withTimeout
    { HttpClient.redirectCount = options.maxRedirects,
      HttpClient.proxy = Nothing
    }
