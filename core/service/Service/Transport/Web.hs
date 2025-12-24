module Service.Transport.Web (
  WebTransport (..),
  server,
) where

import Basics
import Bytes (Bytes)
import Bytes qualified
import ConcurrentVar qualified
import Console qualified
import Data.ByteString qualified as GhcBS
import Data.IORef qualified as GhcIORef
import Data.List qualified as GhcList
import GHC.TypeLits qualified as GHC
import Json qualified
import Map qualified
import Maybe (Maybe (..))
import Network.HTTP.Types.Header qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Record (KnownHash (..))
import Record qualified
import Result (Result (..))
import Service.Command.Core (Command, NameOf)
import Service.CommandExecutor.TH (deriveKnownHash)
import Service.Response (CommandResponse)
import Service.Response qualified as Response
import Service.Transport (EndpointHandler, Endpoints (..), Transport (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | HTTP/JSON transport using WAI/Warp.
data WebTransport = WebTransport
  { port :: Int,
    maxBodySize :: Int
  }


type instance NameOf WebTransport = "WebTransport"


deriveKnownHash "WebTransport"


-- | Default WebTransport configuration.
-- Port defaults to 8080.
-- Max body size defaults to 1MB (1048576 bytes) to prevent DoS attacks.
server :: WebTransport
server =
  WebTransport
    { port = 8080,
      maxBodySize = 1048576
    }


-- | Read request body with a size limit to prevent DoS attacks.
-- Returns Err if the body exceeds the limit, Ok with the body bytes otherwise.
readBodyWithLimit :: Int -> Wai.Request -> Task Text (Result Text Bytes)
readBodyWithLimit maxSize request = Task.fromIO do
  sizeRef <- GhcIORef.newIORef 0
  chunksRef <- GhcIORef.newIORef []

  let readChunks = do
        chunk <- Wai.getRequestBodyChunk request
        if GhcBS.null chunk
          then do
            chunks <- GhcIORef.readIORef chunksRef
            let reversedChunks = GhcList.reverse chunks
            let concatenated = GhcBS.concat reversedChunks
            let body = Bytes.fromLegacy concatenated
            pure (Result.Ok body)
          else do
            currentSize <- GhcIORef.readIORef sizeRef
            let chunkSize = GhcBS.length chunk
            let newSize = currentSize + chunkSize
            if newSize > maxSize
              then do
                -- Drain remaining body to avoid connection issues
                drainBody
                pure (Result.Err [fmt|Request body exceeds maximum size of #{maxSize} bytes|])
              else do
                GhcIORef.writeIORef sizeRef newSize
                GhcIORef.modifyIORef chunksRef (\chunks -> chunk : chunks)
                readChunks

      drainBody = do
        chunk <- Wai.getRequestBodyChunk request
        if GhcBS.null chunk
          then pure ()
          else drainBody

  readChunks


-- | Map a CommandResponse to its corresponding HTTP status code.
-- This avoids needing to decode the response bytes to determine the status.
commandResponseToHttpStatus :: CommandResponse -> HTTP.Status
commandResponseToHttpStatus response = case response of
  Response.Accepted {} -> HTTP.status200
  Response.Rejected {} -> HTTP.status400
  Response.Failed {} -> HTTP.status500


instance Transport WebTransport where
  type Request WebTransport = Wai.Request
  type Response WebTransport = Wai.Response
  type
    RunnableTransport WebTransport =
      Wai.Request ->
      (Wai.Response -> Task Text Wai.ResponseReceived) ->
      Task Text Wai.ResponseReceived


  assembleTransport ::
    Endpoints WebTransport ->
    Wai.Request ->
    (Wai.Response -> Task Text Wai.ResponseReceived) ->
    Task Text Wai.ResponseReceived
  assembleTransport endpoints request respond = do
    let maxBodySize = endpoints.transport.maxBodySize

    -- Helper function for 404 responses
    let notFound message = do
          let response404 =
                message
                  |> Text.toBytes
                  |> Bytes.toLazyLegacy
                  |> Wai.responseLBS HTTP.status404 [(HTTP.hContentType, "application/json")]
          respond response404

    -- Helper function for 413 Payload Too Large responses
    let payloadTooLarge message = do
          let response413 =
                message
                  |> Text.toBytes
                  |> Bytes.toLazyLegacy
                  |> Wai.responseLBS HTTP.status413 [(HTTP.hContentType, "application/json")]
          respond response413

    -- Parse the request path to check if it matches /commands/<name>
    case Wai.pathInfo request of
      ["commands", commandName] -> do
        -- Look up the command handler in the endpoints map
        case Map.get commandName endpoints.commandEndpoints of
          Maybe.Just handler -> do
            -- Read the request body with size limit to prevent DoS attacks
            bodyResult <- readBodyWithLimit maxBodySize request
            case bodyResult of
              Result.Err errorMessage ->
                payloadTooLarge errorMessage
              Result.Ok bodyBytes -> do
                respondVar <- ConcurrentVar.new
                -- We need to capture the ResponseReceived from the handler's callback
                -- Since handler returns Unit, we'll use andThen to chain the result
                handler
                  bodyBytes
                  ( \(commandResponse, responseBytes) -> do
                      -- Map the CommandResponse directly to HTTP status (no decoding needed)
                      let httpStatus = commandResponseToHttpStatus commandResponse

                      let responseBody =
                            responseBytes
                              |> Bytes.toLazyLegacy
                              |> Wai.responseLBS httpStatus [(HTTP.hContentType, "application/json")]
                      respondValue <- respond responseBody
                      respondVar |> ConcurrentVar.set respondValue
                      Task.yield ()
                  )
                  |> Task.andThen
                    ( \_ -> do
                        ConcurrentVar.get respondVar
                    )
          Maybe.Nothing ->
            notFound [fmt|Command not found: #{commandName}|]
      _ ->
        notFound "Not found"


  runTransport :: WebTransport -> RunnableTransport WebTransport -> Task Text Unit
  runTransport transport runnableTransport = do
    -- RunnableTransport WebTransport is: Wai.Request -> (Wai.Response -> Task Text ResponseReceived) -> Task Text ResponseReceived
    -- We need to convert this to a WAI Application: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
    let waiApp :: Wai.Application
        waiApp request respond = do
          -- Create a Task-based response handler that wraps the IO respond function
          let taskRespond response = Task.fromIO (respond response)

          -- Run the Task-based application and return the ResponseReceived
          runnableTransport request taskRespond
            |> Task.runOrPanic

    -- Start the Warp server on the specified port
    let port = transport.port
    Console.print [fmt|Starting WebTransport server on port #{port}|]
    Warp.run transport.port waiApp |> Task.fromIO


  buildHandler ::
    forall command name.
    ( Command command,
      Json.FromJSON command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    WebTransport ->
    Record.Proxy command ->
    (command -> Task Text CommandResponse) ->
    EndpointHandler
  buildHandler transport _ handler body respond = do
    let port = transport.port
    let n =
          GHC.symbolVal (Record.Proxy @name)
            |> Text.fromLinkedList

    -- Parse the command bytes using Aeson FromJSON
    let commandValue = body |> Json.decodeBytes @command

    case commandValue of
      Result.Ok cmd -> do
        -- Log that we're executing the command
        Console.print [fmt|Executing #{n} on port #{port}|]

        -- Execute the command and get the response
        response <- handler cmd
        let responseJson = Json.encodeText response |> Text.toBytes
        respond (response, responseJson)
      Result.Err _err -> do
        -- Handle parsing error - return a Failed response
        Console.print [fmt|Failed to parse command #{n} on port #{port}|]
        let errorResponse =
              Response.Failed
                { error = [fmt|Invalid JSON format for command #{n}|]
                }
        let responseJson = Json.encodeText errorResponse |> Text.toBytes
        respond (errorResponse, responseJson)
