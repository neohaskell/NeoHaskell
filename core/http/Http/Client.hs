module Http.Client (get) where

import Action qualified
import Core
import Network.HTTP.Simple as HttpSimple
import Unknown qualified

moduleName :: Text
moduleName = "Http.Client"

getActionName :: Text
getActionName = [fmt|{moduleName}.get|]


data RequestOptions (event :: Type) = RequestOptions

get :: (Unknown.Convertible event) => RequestOptions event -> Action event
get options = Action.named getActionName options

getActionHandler :: RequestOptions event -> IO event
getActionHandler options = do
  request <- HttpSimple.parseRequest options.url
  response <-
        request
        |> HttpSimple.httpJSONEither
  dieWith "lol"