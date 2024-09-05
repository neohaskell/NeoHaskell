module Http.Client (
  Request (..),
  get,
  expectText,
  getActionHandler,
) where

import Action qualified
import Bytes (Bytes (INTERNAL_CORE_BYTES_CONSTRUCTOR))
import Core
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
    mapper :: Maybe (Result Error Bytes -> event)
  }


data Error


expectText :: (Result Error Text -> event) -> Request event -> Request event
expectText userMapper options = do
  let mapper res = res |> Result.map (Text.fromBytes) |> userMapper
  options
    { mapper = Just mapper
    }


get :: (Unknown.Convertible event) => Request event -> Action event
get options = Action.named [fmt|{moduleName}.get|] options


getActionHandler :: Request event -> IO event
getActionHandler options = do
  let mapper = options.mapper |> Maybe.withDefault (dieWith "mapper is required")
  let url = options.url |> Maybe.withDefault (dieWith "url is required")

  request <- Text.toLinkedList url |> HttpSimple.parseRequest
  response <-
    request
      |> HttpSimple.httpBS

  response
    |> Http.getResponseBody
    |> Bytes.INTERNAL_CORE_BYTES_CONSTRUCTOR
    |> Result.Ok
    |> mapper
    |> pure
