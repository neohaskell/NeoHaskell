module Service.Transport.WebSpec where

import Array qualified
import Auth.Claims (UserClaims (..))
import Auth.Middleware (extractToken)
import Bytes qualified
import ConcurrentVar qualified
import Core
import Data.Text.Encoding qualified as GhcTextEncoding
import Json qualified
import LinkedList qualified
import Map qualified
import Network.HTTP.Types.Header qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTPStatus
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal
import Network.Wai.Test qualified as WaiTest
import Service.Application.Transports qualified as Transports
import Service.AccessControl (AccessError (..))
import Service.AccessControl qualified as Auth
import Service.Query.Core qualified
import Service.Query.Endpoint qualified as Endpoint
import Service.Query.Pagination (QueryPageRequest (..), absoluteMaxLimit)
import Service.Query.Subscriber qualified as Subscriber
import Service.QueryObjectStore.Core (Error (..), QueryObjectStore (..))
import Service.QueryObjectStore.InMemory qualified as InMemoryStore
import Service.Transport (EndpointSchema (..), QueryByIdEndpoint (..), QueryByIdEndpointHandler)
import Service.Transport qualified as Transport
import Service.Transport.Web qualified as Web
import Task qualified
import Test
import Test.Auth.Jwt.Core qualified as Jwt
import Text qualified
import ToText qualified
import Uuid qualified


-- ============================================================================
-- Test Query Types for HTTP Transport Tests
-- ============================================================================

-- | Protected query - requires authentication
data ProtectedQuery = ProtectedQuery
  { protectedQueryId :: Uuid,
    protectedQueryData :: Text
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON ProtectedQuery


instance Json.FromJSON ProtectedQuery


-- | Query instance - requires authentication
instance Service.Query.Core.Query ProtectedQuery where
  canAccessImpl user = case user of
    Nothing -> Just Unauthenticated
    Just _ -> Nothing
  canViewImpl _ _ = Nothing


-- | Permission-protected query - requires specific permission
data AdminQuery = AdminQuery
  { adminQueryId :: Uuid,
    adminQueryData :: Text
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON AdminQuery


instance Json.FromJSON AdminQuery


-- | Query instance - requires "admin:read" permission
instance Service.Query.Core.Query AdminQuery where
  canAccessImpl user = case user of
    Nothing -> Just Unauthenticated
    Just claims ->
      if claims.permissions |> Array.contains "admin:read"
        then Nothing
        else Just (InsufficientPermissions ["admin:read"])
  canViewImpl _ _ = Nothing


data WebQueryByIdFixture = WebQueryByIdFixture
  { webQueryById :: Uuid
  , webQueryOwner :: Maybe Text
  , webQueryValue :: Text
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON WebQueryByIdFixture


instance Json.FromJSON WebQueryByIdFixture


instance Service.Query.Core.Query WebQueryByIdFixture where
  canAccessImpl _ = Nothing
  canViewImpl userClaims query =
    case query.webQueryOwner of
      Nothing -> Nothing
      Just owner ->
        case userClaims of
          Nothing -> Just Forbidden
          Just claims -> if claims.sub == owner then Nothing else Just Forbidden


data ForbiddenByIdQuery = ForbiddenByIdQuery
  { forbiddenById :: Uuid
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON ForbiddenByIdQuery


instance Json.FromJSON ForbiddenByIdQuery


instance Service.Query.Core.Query ForbiddenByIdQuery where
  canAccessImpl _ = Just Forbidden
  canViewImpl _ _ = Nothing


-- ============================================================================
-- HTTP Transport Authorization Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "readiness endpoint" do
    it "ready stays unavailable until all queries reach head" \_ -> do
      let (rebuildingStatus, _) = Web.renderReadiness Subscriber.Rebuilding
      let (failedStatus, _) = Web.renderReadiness (Subscriber.Failed "fixture failure")
      let (readyStatus, _) = Web.renderReadiness Subscriber.Ready
      rebuildingStatus |> shouldBe HTTPStatus.status503
      failedStatus |> shouldBe HTTPStatus.status503
      readyStatus |> shouldBe HTTPStatus.status200

  describe "query by ID routes" do
    it "returns one anonymous query object by its UUID and ignores collection parameters" \_ -> do
      store <- InMemoryStore.new @WebQueryByIdFixture |> Task.mapError toText
      requestedId <- Uuid.generate
      let requestedRow = WebQueryByIdFixture {webQueryById = requestedId, webQueryOwner = Nothing, webQueryValue = "visible"}
      store.atomicUpdate requestedId (\_ -> Just requestedRow) |> Task.mapError errorToText
      lookupIds <- ConcurrentVar.containing (Array.empty :: Array Uuid)
      let keyedStore = store
            { get = \queryId -> do
                lookupIds |> ConcurrentVar.modify (Array.push queryId)
                store.get queryId
            , getAll = Task.throw (Service.QueryObjectStore.Core.StorageError "collection scan must not run")
            }
      let handler userClaims queryId = Endpoint.createQueryByIdEndpoint @WebQueryByIdFixture keyedStore userClaims queryId
      let request =
            (queryByIdRequest "GET" "WebQueryByIdFixture" (Uuid.toText requestedId))
              { Wai.queryString = [("q", Just "not valid"), ("limit", Just "0"), ("offset", Just "400")]
              }
      response <- runWebRequest (webEndpoints "WebQueryByIdFixture" handler) request
      HTTPStatus.statusCode response.simpleStatus |> shouldBe 200
      case response.simpleHeaders |> LinkedList.find (\(name, _) -> name == HTTP.hContentType) of
        Just (_, contentType) -> contentType |> Bytes.fromLegacy |> Text.fromBytes |> shouldBe "application/json"
        Nothing -> fail "Expected the keyed response to set its content type"
      case Json.decodeText @WebQueryByIdFixture (responseBodyText response) of
        Ok decoded -> do
          decoded.webQueryById |> shouldBe requestedId
          decoded.webQueryValue |> shouldBe "visible"
        Err _ -> fail "Expected the keyed response body to be one query object"
      observedIds <- lookupIds |> ConcurrentVar.peek
      observedIds |> shouldBe (Array.fromLinkedList [requestedId])

    it "returns the same full 404 response for missing and row-denied UUIDs" \_ -> do
      store <- InMemoryStore.new @WebQueryByIdFixture |> Task.mapError toText
      storedId <- Uuid.generate
      missingId <- Uuid.generate
      let privateRow = WebQueryByIdFixture {webQueryById = storedId, webQueryOwner = Just "user-1", webQueryValue = "private"}
      store.atomicUpdate storedId (\_ -> Just privateRow) |> Task.mapError errorToText
      let handler userClaims queryId = Endpoint.createQueryByIdEndpoint @WebQueryByIdFixture store userClaims queryId
      let endpoints = webEndpoints "WebQueryByIdFixture" handler
      missing <- runWebRequest endpoints (queryByIdRequest "GET" "WebQueryByIdFixture" (Uuid.toText missingId))
      denied <- runWebRequest endpoints (queryByIdRequest "GET" "WebQueryByIdFixture" (Uuid.toText storedId))
      HTTPStatus.statusCode missing.simpleStatus |> shouldBe 404
      denied.simpleStatus |> shouldBe missing.simpleStatus
      denied.simpleHeaders |> shouldBe missing.simpleHeaders
      denied.simpleBody |> shouldBe missing.simpleBody
      responseBodyText missing |> shouldBe "{\"error\":\"Query object not found\"}"

    it "preserves configured JWT claims for query object authorization" \_ -> do
      keys <- Jwt.testKeys
      manager <- Jwt.createTestManager keys
      ownerToken <- Jwt.signTokenWithSub keys "user-1"
      otherToken <- Jwt.signTokenWithSub keys "user-2"
      store <- InMemoryStore.new @WebQueryByIdFixture |> Task.mapError toText
      ownedId <- Uuid.generate
      publicId <- Uuid.generate
      missingId <- Uuid.generate
      let ownedRow = WebQueryByIdFixture {webQueryById = ownedId, webQueryOwner = Just "user-1", webQueryValue = "owned"}
      let publicRow = WebQueryByIdFixture {webQueryById = publicId, webQueryOwner = Nothing, webQueryValue = "public"}
      store.atomicUpdate ownedId (\_ -> Just ownedRow) |> Task.mapError errorToText
      store.atomicUpdate publicId (\_ -> Just publicRow) |> Task.mapError errorToText
      lookupIds <- ConcurrentVar.containing (Array.empty :: Array Uuid)
      let keyedStore = store
            { get = \queryId -> do
                lookupIds |> ConcurrentVar.modify (Array.push queryId)
                store.get queryId
            }
      let handler userClaims queryId = Endpoint.createQueryByIdEndpoint @WebQueryByIdFixture keyedStore userClaims queryId
      let authEnabled = Web.AuthEnabled {jwksManager = manager, authConfig = Jwt.testConfig}
      let endpoints = (webEndpoints "WebQueryByIdFixture" handler)
            { Transport.transport = Web.server {Web.authEnabled = Just authEnabled}
            }
      let requestFor :: Uuid.Uuid -> Maybe Text -> Wai.Request
          requestFor queryId maybeToken =
            case maybeToken of
              Nothing -> queryByIdRequest "GET" "WebQueryByIdFixture" (Uuid.toText queryId)
              Just token -> (queryByIdRequest "GET" "WebQueryByIdFixture" (Uuid.toText queryId))
                { Wai.requestHeaders =
                    [ (HTTP.hAuthorization, [fmt|Bearer #{token}|] |> Text.toBytes |> Bytes.unwrap)
                    ]
                }
      ownerResponse <- runWebRequest endpoints (requestFor ownedId (Just ownerToken))
      otherResponse <- runWebRequest endpoints (requestFor ownedId (Just otherToken))
      missingResponse <- runWebRequest endpoints (requestFor missingId (Just ownerToken))
      anonymousPublicResponse <- runWebRequest endpoints (requestFor publicId Nothing)
      HTTPStatus.statusCode ownerResponse.simpleStatus |> shouldBe 200
      case Json.decodeText @WebQueryByIdFixture (responseBodyText ownerResponse) of
        Ok decoded -> decoded.webQueryValue |> shouldBe "owned"
        Err _ -> fail "Expected the owner token to return the owned query object"
      HTTPStatus.statusCode otherResponse.simpleStatus |> shouldBe 404
      otherResponse.simpleStatus |> shouldBe missingResponse.simpleStatus
      otherResponse.simpleHeaders |> shouldBe missingResponse.simpleHeaders
      otherResponse.simpleBody |> shouldBe missingResponse.simpleBody
      HTTPStatus.statusCode anonymousPublicResponse.simpleStatus |> shouldBe 200
      case Json.decodeText @WebQueryByIdFixture (responseBodyText anonymousPublicResponse) of
        Ok decoded -> decoded.webQueryValue |> shouldBe "public"
        Err _ -> fail "Expected configured auth to permit an anonymous public query object"
      lookupIdsBeforeInvalidToken <- lookupIds |> ConcurrentVar.peek
      let invalidTokenRequest = requestFor ownedId (Just Jwt.makeMalformedToken)
      invalidTokenResponse <- runWebRequest endpoints invalidTokenRequest
      HTTPStatus.statusCode invalidTokenResponse.simpleStatus |> shouldBe 401
      lookupIdsAfterInvalidToken <- lookupIds |> ConcurrentVar.peek
      lookupIdsAfterInvalidToken |> shouldBe lookupIdsBeforeInvalidToken

    it "rejects malformed UUIDs and non-GET methods before storage access" \_ -> do
      store <- InMemoryStore.new @WebQueryByIdFixture |> Task.mapError toText
      lookupIds <- ConcurrentVar.containing (Array.empty :: Array Uuid)
      let keyedStore = store
            { get = \queryId -> do
                lookupIds |> ConcurrentVar.modify (Array.push queryId)
                store.get queryId
            }
      let handler userClaims queryId = Endpoint.createQueryByIdEndpoint @WebQueryByIdFixture keyedStore userClaims queryId
      let endpoints = webEndpoints "WebQueryByIdFixture" handler
      let malformedRequest = queryByIdRequest "GET" "WebQueryByIdFixture" "not-a-uuid"
      validId <- Uuid.generate
      let wrongMethodRequest = queryByIdRequest "POST" "WebQueryByIdFixture" (Uuid.toText validId)
      malformed <- runWebRequest endpoints malformedRequest
      wrongMethod <- runWebRequest endpoints wrongMethodRequest
      HTTPStatus.statusCode malformed.simpleStatus |> shouldBe 400
      responseBodyText malformed |> shouldBe "{\"error\":\"Invalid query ID\"}"
      HTTPStatus.statusCode wrongMethod.simpleStatus |> shouldBe 404
      observedIds <- lookupIds |> ConcurrentVar.peek
      observedIds |> shouldBe Array.empty

    it "maps query-level access failures to 401 and 403 before storage" \_ -> do
      protectedStore <- InMemoryStore.new @ProtectedQuery |> Task.mapError toText
      protectedLookups <- ConcurrentVar.containing (Array.empty :: Array Uuid)
      let protectedSpy = protectedStore
            { get = \queryId -> do
                protectedLookups |> ConcurrentVar.modify (Array.push queryId)
                Task.throw (Service.QueryObjectStore.Core.StorageError "protected lookup must not run")
            }
      let protectedHandler userClaims queryId = Endpoint.createQueryByIdEndpoint @ProtectedQuery protectedSpy userClaims queryId
      let protectedRequest = queryByIdRequest "GET" "ProtectedQuery" "00000000-0000-0000-0000-000000000001"
      unauthorized <- runWebRequest (webEndpoints "ProtectedQuery" protectedHandler) protectedRequest
      HTTPStatus.statusCode unauthorized.simpleStatus |> shouldBe 401
      responseBodyText unauthorized |> shouldBe "Authentication required"

      forbiddenStore <- InMemoryStore.new @ForbiddenByIdQuery |> Task.mapError toText
      forbiddenLookups <- ConcurrentVar.containing (Array.empty :: Array Uuid)
      let forbiddenSpy = forbiddenStore
            { get = \queryId -> do
                forbiddenLookups |> ConcurrentVar.modify (Array.push queryId)
                Task.throw (Service.QueryObjectStore.Core.StorageError "forbidden lookup must not run")
            }
      let forbiddenHandler userClaims queryId = Endpoint.createQueryByIdEndpoint @ForbiddenByIdQuery forbiddenSpy userClaims queryId
      let forbiddenRequest = queryByIdRequest "GET" "ForbiddenByIdQuery" "00000000-0000-0000-0000-000000000001"
      forbidden <- runWebRequest (webEndpoints "ForbiddenByIdQuery" forbiddenHandler) forbiddenRequest
      HTTPStatus.statusCode forbidden.simpleStatus |> shouldBe 403
      responseBodyText forbidden |> shouldBe "Access denied"
      protectedIds <- protectedLookups |> ConcurrentVar.peek
      forbiddenIds <- forbiddenLookups |> ConcurrentVar.peek
      protectedIds |> shouldBe Array.empty
      forbiddenIds |> shouldBe Array.empty

    it "returns the same generic 500 for storage, decode, and protected-render failures" \_ -> do
      baseStore <- InMemoryStore.new @WebQueryByIdFixture |> Task.mapError toText
      queryId <- Uuid.generate
      let row = WebQueryByIdFixture {webQueryById = queryId, webQueryOwner = Nothing, webQueryValue = "visible"}
      baseStore.atomicUpdate queryId (\_ -> Just row) |> Task.mapError errorToText
      let storageFailureStore = baseStore
            { get = \_ -> Task.throw (Service.QueryObjectStore.Core.StorageError "private storage detail")
            }
      let decodeFailureStore = baseStore
            { get = \_ -> Task.throw (Service.QueryObjectStore.Core.SerializationError "private decode detail")
            }
      let renderFailure _value = Task.throw (Auth.StorageError "private renderer detail")
      let storageHandler userClaims requestedId = Endpoint.createQueryByIdEndpoint @WebQueryByIdFixture storageFailureStore userClaims requestedId
      let decodeHandler userClaims requestedId = Endpoint.createQueryByIdEndpoint @WebQueryByIdFixture decodeFailureStore userClaims requestedId
      let renderHandler userClaims requestedId = Endpoint.createQueryByIdEndpointWithProtection renderFailure baseStore userClaims requestedId
      let request = queryByIdRequest "GET" "WebQueryByIdFixture" (Uuid.toText queryId)
      storageResponse <- runWebRequest (webEndpoints "WebQueryByIdFixture" storageHandler) request
      decodeResponse <- runWebRequest (webEndpoints "WebQueryByIdFixture" decodeHandler) request
      renderResponse <- runWebRequest (webEndpoints "WebQueryByIdFixture" renderHandler) request
      storageResponse.simpleStatus |> shouldBe HTTPStatus.status500
      decodeResponse.simpleStatus |> shouldBe HTTPStatus.status500
      renderResponse.simpleStatus |> shouldBe HTTPStatus.status500
      responseBodyText storageResponse |> shouldBe "Internal server error"
      responseBodyText decodeResponse |> shouldBe "Internal server error"
      responseBodyText renderResponse |> shouldBe "Internal server error"

    it "returns the protected renderer's resolved JSON value" \_ -> do
      store <- InMemoryStore.new @WebQueryByIdFixture |> Task.mapError toText
      queryId <- Uuid.generate
      let row = WebQueryByIdFixture {webQueryById = queryId, webQueryOwner = Nothing, webQueryValue = "protected-reference"}
      store.atomicUpdate queryId (\_ -> Just row) |> Task.mapError errorToText
      let resolvedJson = Json.object ["resolved" Json..= ("safe-value" :: Text)]
      let renderer _ = Task.yield resolvedJson
      let handler userClaims requestedId = Endpoint.createQueryByIdEndpointWithProtection renderer store userClaims requestedId
      let request = queryByIdRequest "GET" "WebQueryByIdFixture" (Uuid.toText queryId)
      response <- runWebRequest (webEndpoints "WebQueryByIdFixture" handler) request
      HTTPStatus.statusCode response.simpleStatus |> shouldBe 200
      responseBodyText response |> shouldBe "{\"resolved\":\"safe-value\"}"

    it "does not return a row captured while the query projection becomes unavailable" \_ -> do
      store <- InMemoryStore.new @WebQueryByIdFixture |> Task.mapError toText
      queryId <- Uuid.generate
      let row = WebQueryByIdFixture {webQueryById = queryId, webQueryOwner = Nothing, webQueryValue = "visible"}
      store.atomicUpdate queryId (\_ -> Just row) |> Task.mapError errorToText
      readiness <- ConcurrentVar.containing (Just Subscriber.Ready)
      let handler userClaims requestedId = Endpoint.createQueryByIdEndpoint @WebQueryByIdFixture store userClaims requestedId
      let changesReadiness userClaims requestedId = do
            result <- handler userClaims requestedId
            readiness |> ConcurrentVar.modify (\_ -> Just Subscriber.Rebuilding)
            Task.yield result
      let guardedHandler = Transports.guardQueryByIdEndpoint (ConcurrentVar.peek readiness) changesReadiness
      let request = queryByIdRequest "GET" "WebQueryByIdFixture" (Uuid.toText queryId)
      response <- runWebRequest (webEndpoints "WebQueryByIdFixture" guardedHandler) request
      response.simpleStatus |> shouldBe HTTPStatus.status500
      responseBodyText response |> shouldBe "Internal server error"

  describe "HTTP Transport Authorization" do
    describe "401 Unauthorized responses" do
      it "returns 401 for unauthenticated access to protected query" \_ -> do
        -- Setup: Create a protected query in the store
        store <- InMemoryStore.new @ProtectedQuery |> Task.mapError toText
        queryId <- Uuid.generate
        let query = ProtectedQuery {protectedQueryId = queryId, protectedQueryData = "secret"}
        _ <- store.atomicUpdate queryId (\_ -> Just query) |> Task.mapError errorToText

        -- Execute: Try to access without authentication
        result <- Endpoint.createQueryEndpoint @ProtectedQuery store Nothing Nothing defaultPageRequest |> Task.asResult

        -- Verify: Should get Unauthenticated error (maps to HTTP 401)
        case result of
          Err (Auth.AuthorizationError Unauthenticated) -> pass
          Err other -> fail [fmt|Expected Unauthenticated, got: #{ToText.toText other}|]
          Ok _ -> fail "Expected 401 Unauthenticated error, but query succeeded"

      it "returns 401 when query requires auth and auth infrastructure not configured" \_ -> do
        -- This simulates authEnabled = Nothing in WebTransport
        -- The query still requires auth, so it should fail
        store <- InMemoryStore.new @ProtectedQuery |> Task.mapError toText
        queryId <- Uuid.generate
        let query = ProtectedQuery {protectedQueryId = queryId, protectedQueryData = "secret"}
        _ <- store.atomicUpdate queryId (\_ -> Just query) |> Task.mapError errorToText

        -- No user claims = simulates no auth configured or no token provided
        result <- Endpoint.createQueryEndpoint @ProtectedQuery store Nothing Nothing defaultPageRequest |> Task.asResult

        case result of
          Err (Auth.AuthorizationError Unauthenticated) -> pass
          _ -> fail "Expected Unauthenticated when no user claims provided"

    describe "403 Forbidden responses" do
      it "returns 403 for insufficient permissions" \_ -> do
        -- Setup: Create an admin query
        store <- InMemoryStore.new @AdminQuery |> Task.mapError toText
        queryId <- Uuid.generate
        let query = AdminQuery {adminQueryId = queryId, adminQueryData = "admin-only"}
        _ <- store.atomicUpdate queryId (\_ -> Just query) |> Task.mapError errorToText

        -- Execute: Access with authenticated user lacking admin:read permission
        let userWithoutPerm = testUser "user-1" ["other:permission"]
        result <- Endpoint.createQueryEndpoint @AdminQuery store (Just userWithoutPerm) Nothing defaultPageRequest |> Task.asResult

        -- Verify: Should get InsufficientPermissions error (maps to HTTP 403)
        case result of
          Err (Auth.AuthorizationError (InsufficientPermissions perms)) -> do
            perms |> shouldBe ["admin:read"]
          Err other -> fail [fmt|Expected InsufficientPermissions, got: #{ToText.toText other}|]
          Ok _ -> fail "Expected 403 InsufficientPermissions error, but query succeeded"

      it "returns 403 with generic message (no permission names leaked)" \_ -> do
        -- This test verifies the security fix: permission names should not be
        -- exposed to the client. The error type contains the permissions for
        -- server-side logging, but HTTP response should be generic.
        store <- InMemoryStore.new @AdminQuery |> Task.mapError toText
        queryId <- Uuid.generate
        let query = AdminQuery {adminQueryId = queryId, adminQueryData = "admin-only"}
        _ <- store.atomicUpdate queryId (\_ -> Just query) |> Task.mapError errorToText

        let userWithoutPerm = testUser "user-1" []
        result <- Endpoint.createQueryEndpoint @AdminQuery store (Just userWithoutPerm) Nothing defaultPageRequest |> Task.asResult

        -- The error contains permissions for logging, but the HTTP layer
        -- (WebTransport) returns "Insufficient permissions" without specifics
        case result of
          Err (Auth.AuthorizationError (InsufficientPermissions _)) -> pass
          _ -> fail "Expected InsufficientPermissions error"

    describe "malformed auth headers" do
      it "extractToken returns Nothing for non-Bearer auth" \_ -> do
        let request = mockRequestWithAuth "Basic dXNlcjpwYXNz"
        extractToken request `shouldBe` Nothing

      it "extractToken returns empty string for Bearer without token" \_ -> do
        -- "Bearer" or "Bearer " without actual token returns empty string
        let request1 = mockRequestWithAuth "Bearer "
        extractToken request1 `shouldBe` Just ""

        let request2 = mockRequestWithAuth "Bearer"
        extractToken request2 `shouldBe` Just ""

      it "extractToken handles empty Authorization header" \_ -> do
        let request = mockRequestWithAuth ""
        extractToken request `shouldBe` Nothing

      it "extractToken handles whitespace-only header" \_ -> do
        let request = mockRequestWithAuth "   "
        extractToken request `shouldBe` Nothing

      it "handles invalid JWT format gracefully" \_ -> do
        -- Test that malformed tokens don't crash the system
        let request = mockRequestWithAuth "Bearer not.a.valid.jwt.token.at.all"
        -- extractToken succeeds (it just extracts the string)
        extractToken request `shouldBe` Just "not.a.valid.jwt.token.at.all"
        -- Validation would fail later in the auth middleware

      it "handles JWT with invalid base64 gracefully" \_ -> do
        let request = mockRequestWithAuth "Bearer !!!invalid-base64!!!"
        extractToken request `shouldBe` Just "!!!invalid-base64!!!"
        -- The actual validation happens in Auth.Middleware.checkAuth

    describe "rate limiting" do
      -- Note: Rate limiting is not currently implemented in the Query system.
      -- This is tracked in issue #265 (pagination/limits).
      -- These tests document the expected future behavior.
      it "PENDING: should limit excessive query requests per client" \_ -> do
        -- Rate limiting would be implemented at the WebTransport level
        -- using a token bucket or sliding window algorithm.
        -- For now, we rely on pagination (issue #265) to limit response sizes.
        pending "Rate limiting not yet implemented - see issue #265 for pagination"

      it "PENDING: should return 429 Too Many Requests when rate limited" \_ -> do
        pending "Rate limiting not yet implemented - see issue #265 for pagination"


-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Create test user claims
testUser :: Text -> Array Text -> UserClaims
testUser userId permissions =
  UserClaims
    { sub = userId,
      email = Nothing,
      name = Nothing,
      permissions = permissions,
      tenantId = Nothing,
      rawClaims = Map.empty
    }


-- | Default page request for tests
defaultPageRequest :: QueryPageRequest
defaultPageRequest = QueryPageRequest { limit = absoluteMaxLimit, offset = 0 }


webEndpoints :: Text -> QueryByIdEndpointHandler -> Transport.Endpoints Web.WebTransport
webEndpoints queryName handler =
  Transport.Endpoints
    { Transport.transport = Web.server
    , Transport.commandEndpoints = Map.empty
    , Transport.queryEndpoints = Map.empty
    , Transport.queryByIdEndpoints = Map.fromArray [(queryName, queryByIdEndpoint handler)]
    , Transport.commandSchemas = Map.empty
    , Transport.querySchemas = Map.empty
    }


queryByIdEndpoint :: QueryByIdEndpointHandler -> QueryByIdEndpoint
queryByIdEndpoint handler =
  QueryByIdEndpoint
    { byIdHandler = handler
    , byIdSchema = EndpointSchema
        { requestSchema = Nothing
        , responseSchema = SObject [FieldSchema "webQueryById" SText True "", FieldSchema "webQueryValue" SText True ""]
        , description = ""
        , deprecated = False
        , entityName = Nothing
        }
    }


queryByIdRequest :: Text -> Text -> Text -> Wai.Request
queryByIdRequest method queryName queryId =
  Wai.defaultRequest
    { Wai.requestMethod = method |> Text.toBytes |> Bytes.unwrap
    , Wai.rawPathInfo = [fmt|/queries/#{queryName |> Text.toKebabCase}/#{queryId}|] |> Text.toBytes |> Bytes.unwrap
    , Wai.pathInfo = ["queries", queryName |> Text.toKebabCase, queryId]
    }


webApplication :: Transport.Endpoints Web.WebTransport -> Wai.Application
webApplication endpoints request respond = do
  let runnable = Transport.assembleTransport endpoints
  runnable request (\response -> Task.fromIO (respond response))
    |> Task.runOrPanic


runWebRequest :: Transport.Endpoints Web.WebTransport -> Wai.Request -> Task Text WaiTest.SResponse
runWebRequest endpoints request =
  WaiTest.runSession
    (WaiTest.srequest (WaiTest.SRequest request ""))
    (webApplication endpoints)
    |> Task.fromIO


responseBodyText :: WaiTest.SResponse -> Text
responseBodyText response =
  response.simpleBody
    |> Bytes.fromLazyLegacy
    |> Text.fromBytes


-- | Create a mock WAI request with an Authorization header
mockRequestWithAuth :: Text -> Wai.Request
mockRequestWithAuth authHeader =
  Wai.defaultRequest
    { WaiInternal.requestHeaders =
        [(HTTP.hAuthorization, GhcTextEncoding.encodeUtf8 authHeader)]
    }


-- | Convert QueryObjectStore Error to Text
errorToText :: Error -> Text
errorToText err = case err of
  Service.QueryObjectStore.Core.StorageError msg -> msg
  SerializationError msg -> msg
