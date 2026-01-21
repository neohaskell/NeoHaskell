module Http.Client (
  Request (..),
  Error (..),
  get,
  post,
  postForm,
  request,
  withUrl,
  addHeader,
  withTimeout,
  withRedirects,
) where

import Array (Array)
import Array qualified
import Basics
import Bytes qualified
import Char (Char)
import Data.Either qualified as GhcEither
import Default (Default (..))
import GHC.Int qualified as GhcInt
import Json qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Maybe qualified
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Simple qualified as Http
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
sanitizeUrlText urlText = do
  let urlString = Text.toLinkedList urlText
  case HttpSimple.parseRequest urlString of
    GhcEither.Left _ -> "<malformed URL>"
    GhcEither.Right req -> do
      let schemeText = case HttpClient.secure req of
            True -> "https://" :: Text
            False -> "http://" :: Text
      let hostText = toText (show (HttpClient.host req))
      let portText = toText (show (HttpClient.port req))
      Text.append schemeText (Text.append hostText (Text.append ":" portText))


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
  options
    { headers = options.headers |> Map.set key value
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
  Task Error response
get options =
  getIO options
    |> Task.fromFailableIO @HttpClient.HttpException
    |> Task.mapError sanitizeHttpError


-- | Internal IO action for GET request (can throw HttpException)
getIO ::
  (Json.FromJSON response) =>
  Request ->
  GhcIO.IO response
getIO options = do
  let url = options.url |> Maybe.withDefault (panic "url is required")
  r <- Text.toLinkedList url |> HttpSimple.parseRequest
  let withHeaders =
        options.headers
          |> Map.reduce r \key value acc ->
            HttpSimple.addRequestHeader (Text.convert key) (Text.convert value) acc
  let withTimeout = case options.timeoutSeconds of
        Nothing -> withHeaders
        Just seconds -> do
          let microseconds = seconds * 1000000
          HttpSimple.setRequestResponseTimeout
            (HttpClient.responseTimeoutMicro microseconds)
            withHeaders
  -- SECURITY: Apply redirect limit (default 0 for SSRF protection)
  let req = withTimeout {HttpClient.redirectCount = options.maxRedirects}
  response <- HttpSimple.httpJSON req
  Http.getResponseBody response
    |> pure


-- | Performs a POST request with JSON body.
post ::
  (Json.FromJSON response, Json.ToJSON requestBody) =>
  Request ->
  requestBody ->
  Task Error response
post options body =
  postIO options body
    |> Task.fromFailableIO @HttpClient.HttpException
    |> Task.mapError sanitizeHttpError


-- | Internal IO action for POST request (can throw HttpException)
postIO ::
  (Json.FromJSON response, Json.ToJSON requestBody) =>
  Request ->
  requestBody ->
  GhcIO.IO response
postIO options body = do
  let url = options.url |> Maybe.withDefault (panic "url is required")
  r <- Text.toLinkedList url |> HttpSimple.parseRequest
  let withHeaders =
        options.headers
          |> Map.reduce r \key value acc ->
            HttpSimple.addRequestHeader (Text.convert key) (Text.convert value) acc
              |> HttpSimple.setRequestMethod "POST"
              |> HttpSimple.setRequestBodyJSON body
  let withTimeout = case options.timeoutSeconds of
        Nothing -> withHeaders
        Just seconds -> do
          let microseconds = seconds * 1000000
          HttpSimple.setRequestResponseTimeout
            (HttpClient.responseTimeoutMicro microseconds)
            withHeaders
  -- SECURITY: Apply redirect limit (default 0 for SSRF protection)
  let req = withTimeout {HttpClient.redirectCount = options.maxRedirects}
  response <- HttpSimple.httpJSON req
  Http.getResponseBody response
    |> pure


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
  Task Error response
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
  GhcIO.IO response
postFormIO options formParams = do
  let url = options.url |> Maybe.withDefault (panic "url is required")
  r <- Text.toLinkedList url |> HttpSimple.parseRequest
  let formData =
        formParams
          |> Array.toLinkedList
          |> fmap (\(key, value) -> (Text.toBytes key |> Bytes.unwrap, Text.toBytes value |> Bytes.unwrap))
  let withHeaders =
        options.headers
          |> Map.reduce r \key value acc ->
            HttpSimple.addRequestHeader (Text.convert key) (Text.convert value) acc
  let withForm =
        withHeaders
          |> HttpSimple.setRequestMethod "POST"
          |> HttpSimple.setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
          |> HttpSimple.setRequestBodyURLEncoded formData
  let withTimeout = case options.timeoutSeconds of
        Nothing -> withForm
        Just seconds -> do
          let microseconds = seconds * 1000000
          HttpSimple.setRequestResponseTimeout
            (HttpClient.responseTimeoutMicro microseconds)
            withForm
  -- SECURITY: Apply redirect limit (default 0 for SSRF protection)
  let req = withTimeout {HttpClient.redirectCount = options.maxRedirects}
  response <- HttpSimple.httpJSON req
  Http.getResponseBody response
    |> pure