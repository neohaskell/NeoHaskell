module Service.Transport.Web (
  WebTransport (..),
  AuthEnabled (..),
  OAuth2Config (..),
  server,
) where

import Auth.Config qualified
import Auth.Error (AuthError (..))
import Auth.Jwks (JwksManager)
import Auth.Middleware qualified as Middleware
import Auth.OAuth2.Provider ()
import Auth.OAuth2.Routes (OAuth2Routes (..), OAuth2RouteError (..))
import Auth.Options (AuthOptions (Authenticated))
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
import LinkedList qualified
import Map qualified
import Maybe (Maybe (..))
import Network.HTTP.Types.Header qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Record (KnownHash (..))
import Record qualified
import Result (Result (..))
import Service.Auth (RequestContext)
import Service.Auth qualified as Auth
import Service.Command.Core (Command, NameOf)
import Service.CommandExecutor.TH (deriveKnownHash)
import Service.Query.Auth (QueryAuthError (..), QueryEndpointError (..))
import Service.Response (CommandResponse)
import Service.Response qualified as Response
import Service.Transport (EndpointHandler, Endpoints (..), Transport (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)


-- | Auth configuration for WebTransport endpoints.
-- When enabled, all endpoints require a valid JWT.
-- Permission checks should be done in the command's decide method.
data AuthEnabled = AuthEnabled
  { jwksManager :: JwksManager,
    authConfig :: Auth.Config.AuthConfig
  }


-- | OAuth2 provider configuration for WebTransport.
-- When set, enables /connect/{provider}, /callback/{provider}, /disconnect/{provider} routes.
data OAuth2Config = OAuth2Config
  { -- | OAuth2 route handlers
    routes :: OAuth2Routes
  }


-- | HTTP/JSON transport using WAI/Warp.
data WebTransport = WebTransport
  { port :: Int,
    maxBodySize :: Int,
    -- | Optional JWT authentication. Set via Application.withAuth.
    authEnabled :: Maybe AuthEnabled,
    -- | Optional OAuth2 provider routes. Set via Application.withOAuth2Provider.
    oauth2Config :: Maybe OAuth2Config
  }


type instance NameOf WebTransport = "WebTransport"


deriveKnownHash "WebTransport"


-- | Default WebTransport configuration.
-- Port defaults to 8080.
-- Max body size defaults to 1MB (1048576 bytes) to prevent DoS attacks.
-- Auth is disabled by default - use Application.withAuth to enable.
-- OAuth2 is disabled by default - use Application.withOAuth2Provider to enable.
server :: WebTransport
server =
  WebTransport
    { port = 8080,
      maxBodySize = 1048576,
      authEnabled = Nothing,
      oauth2Config = Nothing
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

    -- Helper function for 401 Unauthorized responses
    let unauthorized message = do
          let response401 =
                message
                  |> Text.toBytes
                  |> Bytes.toLazyLegacy
                  |> Wai.responseLBS HTTP.status401 [(HTTP.hContentType, "application/json")]
          respond response401

    -- Helper function for 403 Forbidden responses
    let forbidden message = do
          let response403 =
                message
                  |> Text.toBytes
                  |> Bytes.toLazyLegacy
                  |> Wai.responseLBS HTTP.status403 [(HTTP.hContentType, "application/json")]
          respond response403

    -- Helper function for 500 Internal Server Error responses
    let internalError message = do
          let response500 =
                message
                  |> Text.toBytes
                  |> Bytes.toLazyLegacy
                  |> Wai.responseLBS HTTP.status500 [(HTTP.hContentType, "application/json")]
          respond response500

    -- Helper function for 400 Bad Request responses
    let badRequest message respondFn = do
          let response400 =
                message
                  |> Text.toBytes
                  |> Bytes.toLazyLegacy
                  |> Wai.responseLBS HTTP.status400 [(HTTP.hContentType, "application/json")]
          respondFn response400

    -- Helper function for 302 redirect responses
    -- SECURITY: Sanitize URL to prevent header injection (CRLF attacks)
    let redirect302 url respondFn = do
          -- Remove any CR/LF characters that could allow header injection
          let sanitizedUrl = url |> Text.filter (\c -> c != '\r' && c != '\n')
          let locationHeader = (HTTP.hLocation, sanitizedUrl |> Text.toBytes |> Bytes.unwrap)
          let response302 = Wai.responseLBS HTTP.status302 [locationHeader] ""
          respondFn response302

    -- Helper function for 429 Too Many Requests responses
    let tooManyRequests retryAfter respondFn = do
          let retryHeader = ("Retry-After", retryAfter |> toText |> Text.toBytes |> Bytes.unwrap)
          let response429 =
                "{\"error\":\"Too many requests\"}"
                  |> Text.toBytes
                  |> Bytes.toLazyLegacy
                  |> Wai.responseLBS HTTP.status429 [(HTTP.hContentType, "application/json"), retryHeader]
          respondFn response429

    -- Helper function for OAuth2 route errors
    -- SECURITY: Error messages are generic to avoid information leakage
    let handleOAuth2Error err respondFn = do
          case err of
            -- Generic "not found" for all provider-related errors (prevents enumeration)
            ProviderNotFound _ -> notFound "OAuth2 route not available"
            StateValidationFailed _ -> unauthorized "Invalid or expired state token"
            StateNotFound -> unauthorized "State token not found or already used"
            TokenExchangeFailed _ -> internalError "Token exchange failed"
            -- Generic mismatch error (doesn't reveal expected/actual)
            ProviderMismatch _ _ -> badRequest "Invalid request" respondFn
            MissingParameter _ -> badRequest "Invalid request" respondFn
            RateLimited retryAfter -> tooManyRequests retryAfter respondFn

    -- Parse the request path to check if it matches /commands/<name> or /queries/<name>
    case Wai.pathInfo request of
      ["commands", commandNameKebab] -> do
        -- Convert kebab-case URL path to PascalCase for lookup
        -- URL: /commands/reserve-stock -> lookup key: "ReserveStock"
        let commandName = commandNameKebab |> Text.toPascalCase
        -- Look up the command handler in the endpoints map
        case Map.get commandName endpoints.commandEndpoints of
          Maybe.Just handler -> do
            -- Helper to process command with a RequestContext
            let processCommandWithContext :: RequestContext -> Task Text Wai.ResponseReceived
                processCommandWithContext requestContext = do
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
                        requestContext
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

            -- Check authentication and build RequestContext
            case webTransport.authEnabled of
              Nothing -> do
                -- No auth configured, use anonymous context
                requestContext <- Auth.anonymousContext
                processCommandWithContext requestContext
              Just auth -> do
                -- Validate JWT token (permission checks done in command's decide method)
                authResult <- Middleware.checkAuth (Just auth.jwksManager) auth.authConfig Authenticated request

                case authResult of
                  Result.Err authErr ->
                    -- Return 401/403 response
                    Middleware.respondWithAuthError authErr respond
                  Result.Ok authContext -> do
                    -- Build RequestContext from AuthContext claims
                    requestContext <- case authContext.claims of
                      Maybe.Just claims -> Auth.authenticatedContext claims
                      Maybe.Nothing -> Auth.anonymousContext
                    processCommandWithContext requestContext
          Maybe.Nothing ->
            notFound [fmt|Command not found: #{commandName}|]
      ["queries", queryNameKebab] -> do
        -- Convert kebab-case URL path to PascalCase for lookup
        -- URL: /queries/stock-level -> lookup key: "StockLevel"
        let queryName = queryNameKebab |> Text.toPascalCase
        -- Look up the query handler in the endpoints map
        case Map.get queryName endpoints.queryEndpoints of
          Maybe.Just handler -> do
            -- Helper to process query with given user claims
            let processQueryWithClaims userClaims = do
                  -- Execute the query handler with error recovery
                  -- Handler performs internal canAccess/canView checks
                  result <- handler userClaims |> Task.asResult
                  case result of
                    Result.Ok responseText -> okJson responseText
                    Result.Err endpointError ->
                      -- Pattern match on typed error for proper HTTP status
                      case endpointError of
                        AuthorizationError authErr ->
                          case authErr of
                            Unauthenticated -> unauthorized "Authentication required"
                            Forbidden -> forbidden "Access denied"
                            InsufficientPermissions _ -> forbidden "Insufficient permissions"
                        StorageError _msg ->
                          -- Don't expose internal error details to client
                          -- The msg is logged server-side by the transport layer
                          internalError "Internal server error"

            -- Extract user claims (if auth configured)
            case webTransport.authEnabled of
              Maybe.Nothing ->
                -- No auth configured, pass Nothing (query decides if that's OK)
                processQueryWithClaims Maybe.Nothing
              Maybe.Just auth -> do
                -- Try to validate JWT token
                -- The query's canAccessImpl decides if authentication is required
                authResult <- Middleware.checkAuth (Maybe.Just auth.jwksManager) auth.authConfig Authenticated request

                case authResult of
                  Result.Err authErr ->
                    -- Check if token was missing vs invalid
                    case authErr of
                      TokenMissing ->
                        -- No token provided - let query handler decide if that's OK
                        processQueryWithClaims Maybe.Nothing
                      _ ->
                        -- Invalid token provided - reject with appropriate error
                        Middleware.respondWithAuthError authErr respond
                  Result.Ok authContext ->
                    -- Token valid - pass claims to handler
                    processQueryWithClaims authContext.claims
          Maybe.Nothing ->
            notFound [fmt|Query not found: #{queryName}|]
      -- OAuth2 routes: /connect/{provider}, /callback/{provider}, /disconnect/{provider}
      ["connect", providerName] -> do
        case webTransport.oauth2Config of
          Maybe.Nothing -> notFound "OAuth2 not configured"
          Maybe.Just oauth2 -> do
            -- Connect requires authentication (need userId)
            case webTransport.authEnabled of
              Maybe.Nothing -> unauthorized "Authentication required for OAuth2 connect"
              Maybe.Just auth -> do
                authResult <- Middleware.checkAuth (Maybe.Just auth.jwksManager) auth.authConfig Authenticated request
                case authResult of
                  Result.Err authErr -> Middleware.respondWithAuthError authErr respond
                  Result.Ok authContext -> do
                    case authContext.claims of
                      Maybe.Nothing -> unauthorized "Authentication required"
                      Maybe.Just claims -> do
                        -- Extract userId from claims (sub field)
                        let userId = claims.sub
                        result <- oauth2.routes.handleConnect providerName userId |> Task.asResult
                        case result of
                          Result.Err routeErr -> handleOAuth2Error routeErr respond
                          Result.Ok authUrl -> redirect302 authUrl respond
      ["callback", providerName] -> do
        case webTransport.oauth2Config of
          Maybe.Nothing -> notFound "OAuth2 not configured"
          Maybe.Just oauth2 -> do
            -- Callback does NOT require auth - state token carries userId
            -- Extract code and state from query params
            let queryParams = Wai.queryString request
            let getParam name = do
                  let matches = queryParams |> LinkedList.filter (\(k, _) -> k == name)
                  case matches of
                    [] -> Maybe.Nothing
                    ((_, v) : _) -> v |> fmap (\bs -> Bytes.fromLegacy bs |> Text.fromBytes)
            case (getParam "code", getParam "state") of
              (Maybe.Just code, Maybe.Just state) -> do
                result <- oauth2.routes.handleCallback providerName code state |> Task.asResult
                case result of
                  Result.Err routeErr -> handleOAuth2Error routeErr respond
                  Result.Ok (redirectUrl, _maybeAction) -> do
                    -- TODO: Dispatch action to command handler if present
                    redirect302 redirectUrl respond
              _ -> badRequest "Missing code or state parameter" respond
      ["disconnect", providerName] -> do
        case webTransport.oauth2Config of
          Maybe.Nothing -> notFound "OAuth2 not configured"
          Maybe.Just oauth2 -> do
            -- Disconnect requires authentication (need userId)
            case webTransport.authEnabled of
              Maybe.Nothing -> unauthorized "Authentication required for OAuth2 disconnect"
              Maybe.Just auth -> do
                authResult <- Middleware.checkAuth (Maybe.Just auth.jwksManager) auth.authConfig Authenticated request
                case authResult of
                  Result.Err authErr -> Middleware.respondWithAuthError authErr respond
                  Result.Ok authContext -> do
                    case authContext.claims of
                      Maybe.Nothing -> unauthorized "Authentication required"
                      Maybe.Just claims -> do
                        let userId = claims.sub
                        result <- oauth2.routes.handleDisconnect providerName userId |> Task.asResult
                        case result of
                          Result.Err routeErr -> handleOAuth2Error routeErr respond
                          Result.Ok _action -> do
                            -- TODO: Dispatch action to command handler
                            okJson [fmt|{"status":"disconnected","provider":"#{providerName}"}|]
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
    (RequestContext -> command -> Task Text CommandResponse) ->
    EndpointHandler
  buildHandler transport _ handler requestContext body respond = do
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

        -- Execute the command with RequestContext
        response <- handler requestContext cmd
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
