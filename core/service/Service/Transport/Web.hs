module Service.Transport.Web (
  WebTransport (..),
  AuthEnabled (..),
  server,
) where

import Auth.Config qualified
import Auth.Jwks (JwksManager)
import Auth.Middleware qualified as Middleware
import Auth.Options (AuthOptions (..))
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
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Maybe qualified
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


-- | Auth configuration for WebTransport endpoints.
data AuthEnabled = AuthEnabled
  { jwksManager :: JwksManager,
    authConfig :: Auth.Config.AuthConfig,
    -- | Auth requirements per command (by PascalCase name)
    commandAuthOptions :: Map Text AuthOptions,
    -- | Auth requirements per query (by PascalCase name)
    queryAuthOptions :: Map Text AuthOptions
  }


-- | HTTP/JSON transport using WAI/Warp.
data WebTransport = WebTransport
  { port :: Int,
    maxBodySize :: Int,
    -- | Optional JWT authentication. Set via Application.withAuth.
    authEnabled :: Maybe AuthEnabled
  }


type instance NameOf WebTransport = "WebTransport"


deriveKnownHash "WebTransport"


-- | Default WebTransport configuration.
-- Port defaults to 8080.
-- Max body size defaults to 1MB (1048576 bytes) to prevent DoS attacks.
-- Auth is disabled by default - use Application.withAuth to enable.
server :: WebTransport
server =
  WebTransport
    { port = 8080,
      maxBodySize = 1048576,
      authEnabled = Nothing
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
    let webTransport = endpoints.transport
    let maxBodySize = webTransport.maxBodySize

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

    -- Helper function for 200 OK JSON responses
    let okJson responseText = do
          let response200 =
                responseText
                  |> Text.toBytes
                  |> Bytes.toLazyLegacy
                  |> Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "application/json")]
          respond response200

    -- Helper function for 500 Internal Server Error responses
    let internalError message = do
          let response500 =
                message
                  |> Text.toBytes
                  |> Bytes.toLazyLegacy
                  |> Wai.responseLBS HTTP.status500 [(HTTP.hContentType, "application/json")]
          respond response500

    -- Parse the request path to check if it matches /commands/<name> or /queries/<name>
    case Wai.pathInfo request of
      ["commands", commandNameKebab] -> do
        -- Convert kebab-case URL path to PascalCase for lookup
        -- URL: /commands/reserve-stock -> lookup key: "ReserveStock"
        let commandName = commandNameKebab |> Text.toPascalCase
        -- Look up the command handler in the endpoints map
        case Map.get commandName endpoints.commandEndpoints of
          Maybe.Just handler -> do
            -- Helper to process command after auth passes
            let processCommand = do
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

            -- Check authentication/authorization before processing
            case webTransport.authEnabled of
              Nothing ->
                -- No auth configured, allow everyone
                processCommand
              Just auth -> do
                -- SECURITY: Default to Authenticated when auth is enabled
                -- Endpoints must explicitly opt-out with Everyone if they should be public
                let authOptions =
                      Map.get commandName auth.commandAuthOptions
                        |> Maybe.withDefault Authenticated

                -- Check auth
                authResult <- Middleware.checkAuth (Just auth.jwksManager) auth.authConfig authOptions request

                case authResult of
                  Result.Err authErr ->
                    -- Return 401/403 response
                    Middleware.respondWithAuthError authErr respond
                  Result.Ok _authContext ->
                    -- Auth passed, process the request
                    processCommand
          Maybe.Nothing ->
            notFound [fmt|Command not found: #{commandName}|]
      ["queries", queryNameKebab] -> do
        -- Convert kebab-case URL path to PascalCase for lookup
        -- URL: /queries/stock-level -> lookup key: "StockLevel"
        let queryName = queryNameKebab |> Text.toPascalCase
        -- Look up the query handler in the endpoints map
        case Map.get queryName endpoints.queryEndpoints of
          Maybe.Just handler -> do
            -- Helper to process query after auth passes
            let processQuery = do
                  -- Execute the query handler with error recovery
                  result <- handler |> Task.asResult
                  case result of
                    Result.Ok responseText -> okJson responseText
                    Result.Err errorText -> internalError [fmt|Query #{queryName} failed: #{errorText}|]

            -- Check authentication/authorization before processing
            case webTransport.authEnabled of
              Nothing ->
                -- No auth configured, allow everyone
                processQuery
              Just auth -> do
                -- SECURITY: Default to Authenticated when auth is enabled
                -- Endpoints must explicitly opt-out with Everyone if they should be public
                let authOptions =
                      Map.get queryName auth.queryAuthOptions
                        |> Maybe.withDefault Authenticated

                -- Check auth
                authResult <- Middleware.checkAuth (Just auth.jwksManager) auth.authConfig authOptions request

                case authResult of
                  Result.Err authErr ->
                    -- Return 401/403 response
                    Middleware.respondWithAuthError authErr respond
                  Result.Ok _authContext ->
                    -- Auth passed, execute the query
                    processQuery
          Maybe.Nothing ->
            notFound [fmt|Query not found: #{queryName}|]
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
