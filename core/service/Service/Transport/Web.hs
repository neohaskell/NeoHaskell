module Service.Transport.Web (
  WebTransport (..),
  AuthEnabled (..),
  OAuth2Config (..),
  FileUploadEnabled (..),
  server,
) where

import Auth.Config qualified
import Auth.Error (AuthError (..))
import Auth.Jwks (JwksManager)
import Auth.Middleware qualified as Middleware
import Auth.OAuth2.Provider (OAuth2Action (..))
import Auth.OAuth2.Routes (OAuth2Routes (..), OAuth2RouteError (..))
import Auth.Options (AuthOptions (Authenticated, Everyone))
import Array (Array)
import Array qualified
import Basics
import Bytes (Bytes)
import Bytes qualified
import ConcurrentVar qualified
import Console qualified
import Data.ByteString qualified as GhcBS
import Data.IORef qualified as GhcIORef
import Data.List qualified as GhcList
import Data.Yaml qualified as Yaml
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
import Schema.OpenApi qualified as OpenApi
import Service.Application (ApiInfo (..))
import Service.Application qualified
import Service.Auth (RequestContext)
import Service.Auth qualified as Auth
import Service.Command.Core (Command, NameOf)
import Service.FileUpload.Core (FileRef (..))
import Service.FileUpload.Core qualified as FileUpload
import Service.FileUpload.Web (FileUploadRoutes (..))
import Service.CommandExecutor.TH (deriveKnownHash)
import Service.Query.Auth (QueryAuthError (..), QueryEndpointError (..))
import Service.Response (CommandResponse)
import Service.Response qualified as Response
import Service.Transport (EndpointHandler, Endpoints (..), Transport (..))
import Service.Transport.Web.SwaggerUI qualified as SwaggerUI
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
  , -- | Action dispatcher for OAuth2 callbacks.
    -- Called with the JSON-encoded action from onSuccess/onFailure/onDisconnect.
    -- The application layer provides this to dispatch actions to command handlers.
    dispatchAction :: Text -> Task Text Unit
  }


-- | File upload configuration for WebTransport.
-- When set, enables POST /files/upload and GET /files/:fileRef routes.
data FileUploadEnabled = FileUploadEnabled
  { -- | File upload route handlers from Service.FileUpload.Web
    fileUploadRoutes :: FileUploadRoutes
  }


-- | HTTP/JSON transport using WAI/Warp.
data WebTransport = WebTransport
  { port :: Int,
    maxBodySize :: Int,
    -- | Optional JWT authentication. Set via Application.withAuth.
    authEnabled :: Maybe AuthEnabled,
    -- | Optional OAuth2 provider routes. Set via Application.withOAuth2Provider.
    oauth2Config :: Maybe OAuth2Config,
    -- | Optional file upload routes. Set via Application.withFileUpload.
    fileUploadEnabled :: Maybe FileUploadEnabled,
    -- | Optional API metadata for OpenAPI spec generation. Set via Application.withApiInfo.
    apiInfo :: Maybe Service.Application.ApiInfo
  }


type instance NameOf WebTransport = "WebTransport"


deriveKnownHash "WebTransport"


-- | Default WebTransport configuration.
-- Port defaults to 8080.
-- Max body size defaults to 1MB (1048576 bytes) to prevent DoS attacks.
-- Auth is disabled by default - use Application.withAuth to enable.
-- OAuth2 is disabled by default - use Application.withOAuth2Provider to enable.
-- File uploads are disabled by default - use Application.withFileUpload to enable.
server :: WebTransport
server =
  WebTransport
    { port = 8080,
      maxBodySize = 1048576,
      authEnabled = Nothing,
      oauth2Config = Nothing,
      fileUploadEnabled = Nothing,
      apiInfo = Nothing
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


-- | Parse multipart form data to extract file upload.
-- Returns (filename, contentType, content) on success.
-- This is a simplified parser for the common case of a single file upload.
--
-- IMPORTANT: This parser operates on raw Bytes to correctly handle binary files.
-- Binary files (PNG, JPEG, PDF, etc.) contain arbitrary byte sequences including CRLF.
-- Converting to Text would corrupt the data.
parseMultipartUpload :: Text -> Bytes -> Task Text (Result Text (Text, Text, Bytes))
parseMultipartUpload contentTypeHeader bodyBytes = Task.fromIO do
  -- Extract boundary from Content-Type header
  let boundaryResult = extractBoundary contentTypeHeader
  case boundaryResult of
    Result.Err err -> pure (Result.Err err)
    Result.Ok boundary -> do
      -- Parse the multipart body using byte-level operations
      pure (parseMultipartBodyBytes boundary bodyBytes)


-- | Extract boundary string from Content-Type header
-- Format: multipart/form-data; boundary=----WebKitFormBoundary...
extractBoundary :: Text -> Result Text Bytes
extractBoundary contentType = do
  let parts = contentType |> Text.split ";"
  let partsWithBoundary = parts |> Array.takeIf (\p -> Text.contains "boundary=" (Text.trim p))
  case Array.first partsWithBoundary of
    Maybe.Nothing -> Result.Err "Missing boundary in Content-Type"
    Maybe.Just p -> do
      let trimmed = Text.trim p
      let eqParts = Text.split "=" trimmed
      case Array.get 1 eqParts of
        Maybe.Nothing -> Result.Err "Invalid boundary format"
        Maybe.Just boundaryValue -> do
          let cleaned = boundaryValue 
                |> Text.filter (\c -> c != '"')
                |> Text.trim
          -- SECURITY: Reject empty boundaries to prevent DoS via infinite splitOn
          -- Also enforce RFC 2046 boundary length limit (1-70 characters)
          if Text.isEmpty cleaned
            then Result.Err "Boundary cannot be empty"
            else if Text.length cleaned > 70
              then Result.Err "Boundary exceeds maximum length"
              else do
                let boundaryWithPrefix = Text.append "--" cleaned
                Result.Ok (boundaryWithPrefix |> Text.toBytes)


-- | Parse multipart body operating entirely on Bytes to preserve binary content.
parseMultipartBodyBytes :: Bytes -> Bytes -> Result Text (Text, Text, Bytes)
parseMultipartBodyBytes boundary bodyBytes = do
  -- Split by boundary (as bytes)
  let sections = Bytes.splitOn boundary bodyBytes |> Array.fromLinkedList
  -- Skip first empty section and find one with "filename=" in its headers
  let sectionsAfterFirst = sections |> Array.drop 1
  -- For each section, check if the header part contains "filename="
  -- Headers are ASCII, so we can safely decode just the header portion
  let hasFilename section = 
        case Bytes.splitOnce crlfCrlf section of
          Maybe.Nothing -> False
          Maybe.Just (headerBytes, _) -> do
            let headerText = headerBytes |> Text.fromBytes
            Text.contains "filename=" headerText
  let fileSections = sectionsAfterFirst |> Array.takeIf hasFilename
  case Array.first fileSections of
    Maybe.Nothing -> Result.Err "No file found in upload"
    Maybe.Just section -> parseFileSectionBytes section


-- | CRLF CRLF byte sequence (separates headers from content)
crlfCrlf :: Bytes
crlfCrlf = Bytes.pack [0x0D, 0x0A, 0x0D, 0x0A]  -- "\r\n\r\n"


-- | CRLF byte sequence
crlf :: Bytes
crlf = Bytes.pack [0x0D, 0x0A]  -- "\r\n"


-- | Double dash byte sequence (end of multipart marker)
doubleDash :: Bytes
doubleDash = Bytes.pack [0x2D, 0x2D]  -- "--"


-- | Parse a single file section from multipart data.
-- Headers are decoded as text (they're ASCII), content stays as raw bytes.
parseFileSectionBytes :: Bytes -> Result Text (Text, Text, Bytes)
parseFileSectionBytes section = do
  -- Split into headers and content (separated by CRLF CRLF)
  case Bytes.splitOnce crlfCrlf section of
    Maybe.Nothing -> Result.Err "Invalid multipart section: no header/content separator"
    Maybe.Just (headerBytes, contentBytes) -> do
      -- Headers are ASCII, safe to decode as text
      let headerText = headerBytes |> Text.fromBytes
      let headers = headerText |> Text.split "\r\n"
      let filename = extractFilenameFromHeaders headers
      let contentType = extractContentTypeFromHeaders headers
      
      -- Clean up trailing boundary markers from content (as bytes)
      let cleanedContent = cleanTrailingBoundaryBytes contentBytes
      
      case filename of
        Result.Err e -> Result.Err e
        Result.Ok fname -> 
          Result.Ok (fname, contentType, cleanedContent)


-- | Clean trailing boundary markers from content (operates on bytes)
cleanTrailingBoundaryBytes :: Bytes -> Bytes
cleanTrailingBoundaryBytes content = do
  -- Remove trailing "--" if present
  let c1 = if Bytes.isSuffixOf doubleDash content 
           then Bytes.dropEnd 2 content 
           else content
  -- Remove trailing CRLF (separator before boundary) if present
  let c2 = if Bytes.isSuffixOf crlf c1 
           then Bytes.dropEnd 2 c1 
           else c1
  c2


-- | Extract filename from Content-Disposition header
extractFilenameFromHeaders :: Array Text -> Result Text Text
extractFilenameFromHeaders headers = do
  let dispositionHeaders = headers |> Array.takeIf (\h -> Text.contains "Content-Disposition" h)
  case Array.first dispositionHeaders of
    Maybe.Nothing -> Result.Err "Missing Content-Disposition header"
    Maybe.Just h -> extractFilenameFromDisposition h


-- | Extract filename from a Content-Disposition header line
extractFilenameFromDisposition :: Text -> Result Text Text
extractFilenameFromDisposition header = do
  let parts = header |> Text.split ";"
  let filenameParts = parts |> Array.takeIf (\p -> Text.contains "filename=" (Text.trim p))
  case Array.first filenameParts of
    Maybe.Nothing -> Result.Ok ""  -- Empty filename is allowed
    Maybe.Just p -> do
      let trimmed = Text.trim p
      let eqParts = Text.split "=" trimmed
      case Array.get 1 eqParts of
        Maybe.Nothing -> Result.Ok ""
        Maybe.Just value -> Result.Ok (value |> Text.filter (\c -> c != '"') |> Text.trim)


-- | Extract content type from Content-Type header
extractContentTypeFromHeaders :: Array Text -> Text
extractContentTypeFromHeaders headers = do
  let ctHeaders = headers 
        |> Array.takeIf (\h -> Text.contains "Content-Type" h && not (Text.contains "Content-Disposition" h))
  case Array.first ctHeaders of
    Maybe.Nothing -> "application/octet-stream"  -- Default
    Maybe.Just h -> do
      let colonParts = Text.split ":" h
      case Array.get 1 colonParts of
        Maybe.Nothing -> "application/octet-stream"
        Maybe.Just value -> value |> Text.trim


-- | Map a CommandResponse to its corresponding HTTP status code.
-- This avoids needing to decode the response bytes to determine the status.
-- Note: Failed returns 400 (Bad Request) because it typically indicates
-- client errors like parse failures or invalid input, not server errors.
commandResponseToHttpStatus :: CommandResponse -> HTTP.Status
commandResponseToHttpStatus response = case response of
  Response.Accepted {} -> HTTP.status200
  Response.Rejected {} -> HTTP.status400
  Response.Failed {} -> HTTP.status400


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
            -- Callback does NOT require auth - userId is retrieved from server-side TransactionStore
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
                  Result.Ok (redirectUrl, maybeAction) -> do
                    -- Dispatch action to command handler if present
                    case maybeAction of
                      Maybe.Nothing -> redirect302 redirectUrl respond
                      Maybe.Just action -> do
                        case action of
                          SuccessAction actionJson -> do
                            -- Success actions MUST succeed - failing means tokens are lost
                            dispatchResult <- oauth2.dispatchAction actionJson |> Task.asResult
                            case dispatchResult of
                              Result.Err _ -> do
                                -- Log for ops (sanitized - no user data in message)
                                Console.print [fmt|[OAuth2] Success action dispatch failed|] |> Task.ignoreError
                                -- Return 500 - DO NOT redirect to success URL
                                internalError "Connection failed. Please try again."
                              Result.Ok _ -> redirect302 redirectUrl respond
                          FailureAction actionJson -> do
                            -- Failure actions can fail silently - user already knows flow failed
                            dispatchResult <- oauth2.dispatchAction actionJson |> Task.asResult
                            case dispatchResult of
                              Result.Err err -> Console.print [fmt|[OAuth2] Failure action dispatch failed: #{err}|] |> Task.ignoreError
                              Result.Ok _ -> pass
                            redirect302 redirectUrl respond
                          DisconnectAction _ -> do
                            -- DisconnectAction shouldn't appear in callback - defensive redirect
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
                          Result.Ok action -> do
                            -- Dispatch disconnect action to command handler
                            let actionJson = case action of
                                  SuccessAction json -> json
                                  FailureAction json -> json
                                  DisconnectAction json -> json
                            dispatchResult <- oauth2.dispatchAction actionJson |> Task.asResult
                            case dispatchResult of
                              Result.Err err -> do
                                Console.print [fmt|[OAuth2] Disconnect action dispatch failed: #{err}|] |> Task.ignoreError
                                internalError "Disconnect failed"
                              Result.Ok _ -> okJson [fmt|{"status":"disconnected","provider":"#{providerName}"}|]
      -- File upload routes: POST /files/upload, GET /files/:fileRef
      ["files", "upload"] -> do
        case webTransport.fileUploadEnabled of
          Maybe.Nothing -> notFound "File uploads not configured"
          Maybe.Just fileUpload -> do
            -- Extract owner identity from auth context
            -- When auth is enabled, require valid JWT for file uploads (security)
            -- When auth is disabled (dev mode), use shared anonymous identity
            ownerHashResult <- case webTransport.authEnabled of
              Maybe.Nothing -> do
                -- No auth configured (dev mode) - use shared anonymous identity
                -- WARNING: All anonymous uploads are accessible by anyone in this mode
                Task.yield (Result.Ok "anonymous")
              Maybe.Just auth -> do
                -- Auth enabled - require valid JWT for uploads
                authResult <- Middleware.checkAuth (Maybe.Just auth.jwksManager) auth.authConfig Authenticated request
                case authResult of
                  Result.Err authErr -> Task.yield (Result.Err authErr)
                  Result.Ok authContext -> case authContext.claims of
                    Maybe.Nothing -> Task.yield (Result.Err (Auth.Error.TokenMalformed "No claims in token"))
                    Maybe.Just claims -> Task.yield (Result.Ok claims.sub)
            
            case ownerHashResult of
              Result.Err authErr -> Middleware.respondWithAuthError authErr respond
              Result.Ok ownerHash -> do
                -- Read request body (multipart form data)
                bodyResult <- readBodyWithLimit maxBodySize request
                case bodyResult of
                  Result.Err errorMessage -> payloadTooLarge errorMessage
                  Result.Ok bodyBytes -> do
                    -- Parse multipart form data
                    let contentTypeHeader = request 
                          |> Wai.requestHeaders 
                          |> LinkedList.filter (\(name, _) -> name == HTTP.hContentType)
                          |> LinkedList.head
                    case contentTypeHeader of
                      Maybe.Nothing -> badRequest "{\"error\":\"Missing Content-Type header\"}" respond
                      Maybe.Just (_, ctValue) -> do
                        let contentType = ctValue |> Bytes.fromLegacy |> Text.fromBytes
                        if not (Text.contains "multipart/form-data" contentType)
                          then badRequest "{\"error\":\"Expected multipart/form-data\"}" respond
                          else do
                            -- Parse multipart - extract file from form data
                            parseResult <- parseMultipartUpload contentType bodyBytes
                            case parseResult of
                              Result.Err errMsg -> badRequest [fmt|{"error":"#{errMsg}"}|] respond
                              Result.Ok (filename, fileContentType, fileContent) -> do
                                -- Call upload handler with parsed metadata
                                uploadResult <- fileUpload.fileUploadRoutes.handleUpload ownerHash filename fileContentType fileContent
                                  |> Task.mapError (\err -> [fmt|{"error":"#{err}"}|])
                                  |> Task.asResult
                                case uploadResult of
                                  Result.Err errJson -> internalError errJson
                                  Result.Ok response -> do
                                    -- Return success response
                                    let responseJson = Json.encodeText response
                                    okJson responseJson
      ["files", fileRefText] -> do
        case webTransport.fileUploadEnabled of
          Maybe.Nothing -> notFound "File uploads not configured"
          Maybe.Just fileUpload -> do
            let fileRef = FileRef fileRefText
            -- Extract owner identity from auth context
            -- Use Everyone auth option to allow both anonymous and authenticated downloads
            ownerHashResult <- case webTransport.authEnabled of
              Maybe.Nothing -> Task.yield "anonymous"
              Maybe.Just auth -> do
                -- Check for JWT but don't require it (Everyone allows anonymous)
                authResult <- Middleware.checkAuth (Maybe.Just auth.jwksManager) auth.authConfig Everyone request
                case authResult of
                  Result.Err _ -> Task.yield "anonymous"  -- No valid token, treat as anonymous
                  Result.Ok authContext -> case authContext.claims of
                    Maybe.Nothing -> Task.yield "anonymous"
                    Maybe.Just claims -> Task.yield claims.sub  -- Use JWT subject as owner
            
            let ownerHash = ownerHashResult
            
            downloadResult <- fileUpload.fileUploadRoutes.handleDownload ownerHash fileRef
              |> Task.asResult
            case downloadResult of
              Result.Err fileErr -> do
                case fileErr of
                  FileUpload.FileNotFound _ -> notFound "{\"error\":\"File not found\"}"
                  FileUpload.NotOwner _ -> forbidden "{\"error\":\"Access denied\"}"
                  FileUpload.FileExpired _ -> notFound "{\"error\":\"File expired\"}"
                  FileUpload.FileIsDeleted _ -> notFound "{\"error\":\"File not found\"}"
                  FileUpload.BlobMissing _ -> internalError "{\"error\":\"File content missing\"}"
                  -- Don't leak internal error details to clients
                  FileUpload.StorageError _ -> internalError "{\"error\":\"Internal server error\"}"
                  FileUpload.StateLookupFailed _ _ -> internalError "{\"error\":\"Internal server error\"}"
              Result.Ok (content, contentType, filename) -> do
                -- Return file content with appropriate headers
                -- Sanitize filename to prevent CRLF injection in Content-Disposition header
                let sanitizedFilename = filename 
                      |> Text.filter (\c -> c != '\r' && c != '\n' && c != '"' && c != '\\')
                      |> Text.replace ";" "_"
                let contentDispositionText = [fmt|attachment; filename="#{sanitizedFilename}"|]
                let contentDisposition = contentDispositionText |> Text.toBytes |> Bytes.unwrap
                let ctBytes = contentType |> Text.toBytes |> Bytes.unwrap
                let headers = 
                      [ (HTTP.hContentType, ctBytes)
                      , ("Content-Disposition", contentDisposition)
                      ]
                let responseBody = content |> Bytes.toLazyLegacy
                let response200 = Wai.responseLBS HTTP.status200 headers responseBody
                respond response200
      -- OpenAPI routes: /openapi.json, /openapi.yaml, /docs
      ["openapi.json"] -> do
        -- Extract API info (use default if not configured)
        let apiInfo = case webTransport.apiInfo of
              Maybe.Nothing -> Service.Application.defaultApiInfo
              Maybe.Just info -> info
        -- Generate OpenAPI spec from endpoint schemas
        let spec = OpenApi.toOpenApiSpec apiInfo endpoints.commandSchemas endpoints.querySchemas
        -- Encode to JSON and return
        let jsonText = Json.encodeText spec
        okJson jsonText
      ["openapi.yaml"] -> do
        -- Extract API info (use default if not configured)
        let apiInfo = case webTransport.apiInfo of
              Maybe.Nothing -> Service.Application.defaultApiInfo
              Maybe.Just info -> info
        -- Generate OpenAPI spec from endpoint schemas
        let spec = OpenApi.toOpenApiSpec apiInfo endpoints.commandSchemas endpoints.querySchemas
        -- Encode to YAML
        let yamlBytes = Yaml.encode spec
        let responseBody = yamlBytes |> Bytes.fromLegacy |> Bytes.toLazyLegacy
        let response200 = Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "application/x-yaml")] responseBody
        respond response200
      ["docs"] -> do
        -- Extract API title for documentation page
        let apiTitle = case webTransport.apiInfo of
              Maybe.Nothing -> Service.Application.defaultApiInfo.apiTitle
              Maybe.Just info -> info.apiTitle
        -- Generate Scalar HTML documentation
        let htmlText = SwaggerUI.scalarHtml apiTitle
        let htmlBytes = htmlText |> Text.toBytes |> Bytes.toLazyLegacy
        let response200 = Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "text/html; charset=utf-8")] htmlBytes
        respond response200
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
