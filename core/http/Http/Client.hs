module Http.Client (
  Request (..),
  Error (..),
  get,
  post,
  request,
  withUrl,
  addHeader,
  withTimeout,
) where

import Basics
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
withTimeout :: GhcInt.Int -> Request -> Request
withTimeout seconds options =
  options
    { timeoutSeconds = Just seconds
    }


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


-- | Performs a POST request
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
