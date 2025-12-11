module Service.Api.WebApi (
  WebApi (..),
  server,
) where

import Basics
import Bytes (Bytes)
import Console qualified
import GHC.TypeLits qualified as GHC
import Record (KnownHash (..))
import Record qualified
import Service.Api.ApiBuilder (ApiBuilder (..), ApiEndpointHandler, ApiEndpoints)
import Service.Command.Core (Command, NameOf)
import Service.CommandHandler.TH (deriveKnownHash)
import Task (Task)
import Text (Text)
import Text qualified


data WebApi = WebApi
  { port :: Int
  }


type instance NameOf WebApi = "WebApi"


deriveKnownHash "WebApi"


server :: WebApi
server =
  WebApi
    { port = 8080
    }


instance ApiBuilder WebApi where
  type RunnableApi WebApi = Bytes -> (Bytes -> Task Text Unit) -> Task Text Unit


  assembleApi :: WebApi -> ApiEndpoints -> RunnableApi WebApi
  assembleApi _api _endpoints = panic "lol"


  buildCommandHandler ::
    forall command name.
    ( Command command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    WebApi ->
    Record.Proxy command ->
    ApiEndpointHandler
  buildCommandHandler api _ body respond = do
    let port = api.port
    let n =
          GHC.symbolVal (Record.Proxy @name)
            |> Text.fromLinkedList
    Console.print [fmt|Running #{n} on port #{port}|]
    Console.print (body |> Text.fromBytes)
    respond (Text.toBytes n)
