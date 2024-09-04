module Http.Client (
  RequestOptions (..),
  get,
  expectText,
  getActionHandler,
) where

import Action qualified
import Bytes (Bytes)
import Core
import Maybe qualified
import Network.HTTP.Simple as HttpSimple
import Text qualified
import Unknown qualified


moduleName :: Text
moduleName = "Http.Client"


getActionName :: Text
getActionName = [fmt|{moduleName}.get|]


data RequestOptions (event :: Type) = RequestOptions
  { url :: Maybe Text,
    expectFormat :: Maybe ExpectFormat,
    mapper :: Maybe (Result Error Bytes -> event)
  }


data Error


data ExpectFormat
  = ExpectText


expectText :: (Result Error Text -> event) -> RequestOptions event -> RequestOptions event
expectText mapper options =
  options
    { expectFormat = Just ExpectText,
      mapper = Just \res ->
        case res of
          Ok bytes -> Ok (Bytes.toText bytes |)
          Err err -> Err err
    }


get :: (Unknown.Convertible event) => RequestOptions event -> Action event
get options = Action.named getActionName options


getActionHandler :: RequestOptions event -> IO event
getActionHandler options = do
  let url = options.url |> Maybe.withDefault (dieWith "url is required")
  request <- Text.toLinkedList url |> HttpSimple.parseRequest
  response <-
    request
      |> HttpSimple.httpJSONEither
  dieWith "lol"