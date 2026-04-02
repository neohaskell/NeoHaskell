module Service.Transport.Mcp (
  McpTransport (..),
) where

import Bytes qualified
import Core
import Data.ByteString qualified as GhcByteString
import Data.ByteString.Char8 qualified as GhcByteStringChar8
import GHC.TypeLits qualified
import Json qualified
import Record qualified
import Service.Auth (RequestContext)
import Service.CommandExecutor.TH (deriveKnownHash)
import Service.Response (CommandResponse)
import Service.Response qualified as Response
import Service.Transport (EndpointHandler, Endpoints (..), Transport (..))
import Service.Transport.Mcp.JsonRpc qualified as JsonRpc
import Service.Transport.Mcp.Protocol qualified as Protocol
import System.IO qualified as GhcIO
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
  assembleTransport endpoints = do
    let mcpTransport = endpoints.transport
    serverState <- Protocol.newServerState
      mcpTransport.serverName
      mcpTransport.serverVersion
      endpoints.commandEndpoints
      endpoints.queryEndpoints
      endpoints.commandSchemas
      endpoints.querySchemas
    -- STDIO event loop: read JSON-RPC from stdin, dispatch, write to stdout
    let loop = do
          isEof <- Task.fromIO GhcIO.isEOF
          if isEof
            then Task.yield unit
            else do
              line <- Task.fromIO (GhcByteStringChar8.hGetLine GhcIO.stdin)
              let request = JsonRpc.parseRequest (Bytes.fromLegacy line)
              case request of
                Err errResp -> do
                  let encoded = JsonRpc.encodeResponse errResp
                  Task.fromIO (GhcByteString.hPut GhcIO.stdout (Bytes.unwrap encoded))
                  Task.fromIO (GhcIO.hFlush GhcIO.stdout)
                  loop
                Ok req -> do
                  maybeResp <- Protocol.handleRequest serverState req
                  case maybeResp of
                    Nothing -> loop
                    Just resp -> do
                      let encoded = JsonRpc.encodeResponse resp
                      Task.fromIO (GhcByteString.hPut GhcIO.stdout (Bytes.unwrap encoded))
                      Task.fromIO (GhcIO.hFlush GhcIO.stdout)
                      loop
    loop


  runTransport :: McpTransport -> Task Text Unit -> Task Text Unit
  runTransport _transport task = task
