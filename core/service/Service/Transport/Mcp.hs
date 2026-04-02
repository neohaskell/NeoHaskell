module Service.Transport.Mcp (
  McpTransport (..),
) where

import Core
import GHC.TypeLits qualified
import Json qualified
import Record qualified
import Service.Auth (RequestContext)
import Service.CommandExecutor.TH (deriveKnownHash)
import Service.Response (CommandResponse)
import Service.Response qualified as Response
import Service.Transport (EndpointHandler, Endpoints (..), Transport (..))
import Task qualified
import Text qualified


data McpTransport = McpTransport
  { serverName :: Text
  , serverVersion :: Text
  }


type instance NameOf McpTransport = "McpTransport"


deriveKnownHash "McpTransport"


instance Transport McpTransport where
  type Request McpTransport = Bytes
  type Response McpTransport = Bytes
  type RunnableTransport McpTransport = Task Text Unit


  buildHandler ::
    forall command name.
    ( Command command,
      Json.FromJSON command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    McpTransport ->
    Record.Proxy command ->
    (RequestContext -> command -> Task Text CommandResponse) ->
    EndpointHandler
  buildHandler _transport _ handler requestContext body respond = do
    let commandName =
          GHC.TypeLits.symbolVal (Record.Proxy @name)
            |> Text.fromLinkedList
    let commandValue = body |> Json.decodeBytes @command
    case commandValue of
      Ok cmd -> do
        response <- handler requestContext cmd
        let responseJson = Json.encodeText response |> Text.toBytes
        respond (response, responseJson)
      Err _err -> do
        let errorResponse =
              Response.Failed
                { error = [fmt|Invalid input for command #{commandName}|]
                }
        let responseJson = Json.encodeText errorResponse |> Text.toBytes
        respond (errorResponse, responseJson)


  assembleTransport ::
    Endpoints McpTransport ->
    Task Text Unit
  assembleTransport _endpoints = do
    Task.yield unit


  runTransport :: McpTransport -> Task Text Unit -> Task Text Unit
  runTransport _transport task = task
