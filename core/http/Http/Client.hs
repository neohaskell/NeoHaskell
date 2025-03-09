module Http.Client (
  Request (..),
  Error (..),
  get,
  post,
  request,
  withUrl,
  addHeader,
) where

import Core
import Json qualified
import Map qualified
import Maybe qualified
import Network.HTTP.Simple qualified as Http
import Network.HTTP.Simple qualified as HttpSimple
import Task qualified
import Text qualified


data Request = Request
  { url :: Maybe Text,
    headers :: Map Text Text
  }
  deriving (Show)


instance Default Request where
  def =
    Request
      { url = Nothing,
        headers = Map.empty
      }


request :: Request
request = def


withUrl :: Text -> Request -> Request
withUrl url options =
  options
    { url = Just url
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
  let req =
        options.headers
          |> Map.reduce r \key value acc ->
            HttpSimple.addRequestHeader (Text.convert key) (Text.convert value) acc

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
  let req =
        options.headers
          |> Map.reduce r \key value acc ->
            HttpSimple.addRequestHeader (Text.convert key) (Text.convert value) acc
              |> HttpSimple.setRequestMethod "POST"
              |> HttpSimple.setRequestBodyJSON body

  log "Performing request"
  response <- HttpSimple.httpJSON req

  log "Returning"
  Http.getResponseBody response
    |> pure
