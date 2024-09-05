module Http.Client (
  Request (..),
  Error (..),
  get,
  getActionHandler,
  getActionName,
  expectText,
  request,
  withUrl,
  addHeader,
) where

import Action qualified
import Bytes (Bytes (INTERNAL_CORE_BYTES_CONSTRUCTOR), unwrap)
import Core
import IO qualified
import Map qualified
import Maybe qualified
import Network.HTTP.Simple qualified as Http
import Network.HTTP.Simple qualified as HttpSimple
import Result qualified
import Text qualified
import Unknown qualified


moduleName :: Text
moduleName = "Http.Client"


data Request (event :: Type) = Request
  { url :: Maybe Text,
    headers :: Map Text Text,
    mapper :: Maybe (Result Error Bytes -> event)
  }
  deriving (Show)


instance Default (Request event) where
  def =
    Request
      { url = Nothing,
        mapper = Nothing,
        headers = Map.empty
      }


request :: Request event
request = def


withUrl :: Text -> Request event -> Request event
withUrl url options =
  options
    { url = Just url
    }


addHeader :: Text -> Text -> Request event -> Request event
addHeader key value options =
  options
    { headers = options.headers |> Map.set key value
    }


data Error = Error Text


expectText :: (Result Error Text -> event) -> Request event -> Request event
expectText userMapper options = do
  let mapper res = res |> Result.map (Text.fromBytes) |> userMapper
  options
    { mapper = Just mapper
    }


getActionName :: Text
getActionName = [fmt|{moduleName}.get|]


get :: (Unknown.Convertible event) => Request event -> Action event
get options = Action.named getActionName options


getActionHandler :: Request event -> IO event
getActionHandler options = do
  let errorHandler err = do
        log [fmt|Error occurred! {toText err}|]
        toText err |> Error |> Err |> pure
  let mapper = options.mapper |> Maybe.withDefault (dieWith "mapper is required")
  let url = options.url |> Maybe.withDefault (dieWith "url is required")

  log "Parsing request"
  r <- Text.toLinkedList url |> HttpSimple.parseRequest

  log "Setting headers"
  let req =
        options.headers
          |> Map.reduce r \key value acc ->
            HttpSimple.addRequestHeader (Text.convert key) (Text.toBytes value |> Bytes.unwrap) acc

  log "Performing request"
  res <- HttpSimple.httpBS req |> IO.try
  response <-
    case res of
      Err err -> errorHandler err
      Ok response -> pure (Ok response)

  log "Returning"
  response
    |> Result.map Http.getResponseBody
    |> Result.map Bytes.INTERNAL_CORE_BYTES_CONSTRUCTOR
    |> mapper
    |> pure


log :: Text -> IO ()
log msg = print [fmt|[{moduleName}] {msg}|]
