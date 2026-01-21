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
) where

import Array (Array)
import Array qualified
import Basics
import Bytes qualified
import Console (log)
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
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


data Request = Request
  { url :: Maybe Text,
    headers :: Map Text Text,
    timeoutSeconds :: Maybe GhcInt.Int
    -- ^ Request timeout in seconds (default: no timeout)
  }
  deriving (Show)


instance Default Request where
  def =
    Request
      { url = Nothing,
        headers = Map.empty,
        timeoutSeconds = Nothing
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


data Error = Error Text
  deriving (Show)


get ::
  (Json.FromJSON response) =>
  Request ->
  Task Error response
get options = Task.fromIO do
  let url = options.url |> Maybe.withDefault (panic "url is required")

  log "Parsing request"
  r <- Text.toLinkedList url |> HttpSimple.parseRequest

  log "Setting headers"
  let withHeaders =
        options.headers
          |> Map.reduce r \key value acc ->
            HttpSimple.addRequestHeader (Text.convert key) (Text.convert value) acc

  -- Apply timeout if specified (convert seconds to microseconds)
  -- withTimeout validates positive values, so we can safely apply here
  let req = case options.timeoutSeconds of
        Nothing -> withHeaders
        Just seconds -> do
          let microseconds = seconds * 1000000
          HttpSimple.setRequestResponseTimeout
            (HttpClient.responseTimeoutMicro microseconds)
            withHeaders

  log "Performing request"
  response <- HttpSimple.httpJSON req

  log "Returning"
  Http.getResponseBody response
    |> pure


-- | Performs a POST request with JSON body.
post ::
  (Json.FromJSON response, Json.ToJSON requestBody) =>
  Request ->
  requestBody ->
  Task Error response
post options body = Task.fromIO do
  let url = options.url |> Maybe.withDefault (panic "url is required")

  log "Parsing request"
  r <- Text.toLinkedList url |> HttpSimple.parseRequest

  log "Setting headers"
  let withHeaders =
        options.headers
          |> Map.reduce r \key value acc ->
            HttpSimple.addRequestHeader (Text.convert key) (Text.convert value) acc
              |> HttpSimple.setRequestMethod "POST"
              |> HttpSimple.setRequestBodyJSON body

  -- Apply timeout if specified (convert seconds to microseconds)
  -- withTimeout validates positive values, so we can safely apply here
  let req = case options.timeoutSeconds of
        Nothing -> withHeaders
        Just seconds -> do
          let microseconds = seconds * 1000000
          HttpSimple.setRequestResponseTimeout
            (HttpClient.responseTimeoutMicro microseconds)
            withHeaders

  log "Performing request"
  response <- HttpSimple.httpJSON req

  log "Returning"
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
postForm options formParams = Task.fromIO do
  let url = options.url |> Maybe.withDefault (panic "url is required")

  log "Parsing request"
  r <- Text.toLinkedList url |> HttpSimple.parseRequest

  log "Setting headers and form body"
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

  -- Apply timeout if specified (convert seconds to microseconds)
  let req = case options.timeoutSeconds of
        Nothing -> withForm
        Just seconds -> do
          let microseconds = seconds * 1000000
          HttpSimple.setRequestResponseTimeout
            (HttpClient.responseTimeoutMicro microseconds)
            withForm

  log "Performing request"
  response <- HttpSimple.httpJSON req

  log "Returning"
  Http.getResponseBody response
    |> pure