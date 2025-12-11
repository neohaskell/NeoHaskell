module Service.Api.WebApi (
  WebApi (..),
  server,
) where

import Basics
import Console qualified
import GHC.TypeLits qualified as GHC
import Record qualified
import Service.Api.ApiBuilder (ApiBuilder (..), ApiEndpointHandler)
import Service.Command.Core (Command, NameOf)
import Text qualified


data WebApi = WebApi


type instance NameOf WebApi = "WebApi"


server :: WebApi
server = WebApi


instance ApiBuilder WebApi where
  buildCommandHandler ::
    forall command name.
    ( Command command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    WebApi ->
    Record.Proxy command ->
    ApiEndpointHandler
  buildCommandHandler _ _ body respond = do
    let n =
          GHC.symbolVal (Record.Proxy @name)
            |> Text.fromLinkedList
    Console.print n
    Console.print (body |> Text.fromBytes)
    respond (Text.toBytes n)
