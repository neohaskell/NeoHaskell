module Service.Query.EndpointSpec where

import Array qualified
import Auth.Claims (UserClaims (..))
import Core
import Json qualified
import Map qualified
import Service.Query.Auth (QueryAuthError (..), QueryEndpointError)
import Service.Query.Auth qualified as Auth
import Service.Query.Core qualified
import Service.Query.Endpoint qualified as Endpoint
import Service.QueryObjectStore.Core (Error (..), QueryObjectStore (..))
import Service.QueryObjectStore.InMemory qualified as InMemoryStore
import Task qualified
import Test
import ToText qualified
import Uuid qualified


-- | Test query type - public access
data TestQuery = TestQuery
  { id :: Uuid,
    name :: Text,
    value :: Int
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON TestQuery


instance Json.FromJSON TestQuery


-- | Query instance for testing - public access
instance Service.Query.Core.Query TestQuery where
  canAccessImpl _ = Nothing
  canViewImpl _ _ = Nothing


-- | Test query type - requires authentication
data AuthenticatedQuery = AuthenticatedQuery
  { authQueryId :: Uuid,
    authQueryOwnerId :: Text,
    authQueryData :: Text
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON AuthenticatedQuery


instance Json.FromJSON AuthenticatedQuery


-- | Query instance - requires user to be logged in
instance Service.Query.Core.Query AuthenticatedQuery where
  canAccessImpl user = case user of
    Nothing -> Just Unauthenticated
    Just _ -> Nothing
  canViewImpl _ _ = Nothing


-- | Test query type - owner-only access
data OwnerOnlyQuery = OwnerOnlyQuery
  { ownerQueryId :: Uuid,
    ownerId :: Text,
    ownerQueryData :: Text
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON OwnerOnlyQuery


instance Json.FromJSON OwnerOnlyQuery


-- | Query instance - filters results to owner only
instance Service.Query.Core.Query OwnerOnlyQuery where
  canAccessImpl user = case user of
    Nothing -> Just Unauthenticated
    Just _ -> Nothing
  canViewImpl user query = case user of
    Nothing -> Just Unauthenticated
    Just claims ->
      case claims.sub == query.ownerId of
        True -> Nothing
        False -> Just Forbidden


-- | Test query type - requires specific permission
data PermissionQuery = PermissionQuery
  { permQueryId :: Uuid,
    permQueryData :: Text
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON PermissionQuery


instance Json.FromJSON PermissionQuery


-- | Query instance - requires "admin:read" permission
instance Service.Query.Core.Query PermissionQuery where
  canAccessImpl user = case user of
    Nothing -> Just Unauthenticated
    Just claims ->
      case claims.permissions |> Array.contains "admin:read" of
        True -> Nothing
        False -> Just (InsufficientPermissions ["admin:read"])
  canViewImpl _ _ = Nothing


-- | Helper to create test user claims
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


spec :: Spec Unit
spec = do
  describe "Service.Query.Endpoint" do
    describe "createQueryEndpoint" do
      it "returns empty JSON array when no queries exist" \_ -> do
        store <- InMemoryStore.new |> Task.mapError toText

        result <- Endpoint.createQueryEndpoint @TestQuery store Nothing |> Task.mapError endpointErrorToText

        result |> shouldBe "[]"

      it "returns JSON array with single query" \_ -> do
        store <- InMemoryStore.new |> Task.mapError toText
        queryId <- Uuid.generate

        let query = TestQuery {id = queryId, name = "test", value = 42}
        store.atomicUpdate queryId (\_ -> Just query)
          |> Task.mapError errorToText

        result <- Endpoint.createQueryEndpoint @TestQuery store Nothing |> Task.mapError endpointErrorToText

        -- Result should be a JSON array containing the query
        let decoded = Json.decodeText @(Array TestQuery) result
        case decoded of
          Ok queries -> do
            Array.length queries |> shouldBe 1
            let firstQuery = queries |> Array.get 0
            case firstQuery of
              Just q -> q.name |> shouldBe "test"
              Nothing -> fail "Expected query not found"
          Err _err -> fail "Failed to decode result"

      it "returns JSON array with multiple queries" \_ -> do
        store <- InMemoryStore.new |> Task.mapError toText
        id1 <- Uuid.generate
        id2 <- Uuid.generate
        id3 <- Uuid.generate

        let query1 = TestQuery {id = id1, name = "first", value = 1}
        let query2 = TestQuery {id = id2, name = "second", value = 2}
        let query3 = TestQuery {id = id3, name = "third", value = 3}

        store.atomicUpdate id1 (\_ -> Just query1) |> Task.mapError errorToText
        store.atomicUpdate id2 (\_ -> Just query2) |> Task.mapError errorToText
        store.atomicUpdate id3 (\_ -> Just query3) |> Task.mapError errorToText

        result <- Endpoint.createQueryEndpoint @TestQuery store Nothing |> Task.mapError endpointErrorToText

        let decoded = Json.decodeText @(Array TestQuery) result
        case decoded of
          Ok queries -> Array.length queries |> shouldBe 3
          Err _err -> fail "Failed to decode result"

      it "returns valid JSON that can be parsed" \_ -> do
        store <- InMemoryStore.new |> Task.mapError toText
        queryId <- Uuid.generate

        let query = TestQuery {id = queryId, name = "parsable", value = 100}
        store.atomicUpdate queryId (\_ -> Just query)
          |> Task.mapError errorToText

        result <- Endpoint.createQueryEndpoint @TestQuery store Nothing |> Task.mapError endpointErrorToText

        -- Verify the result is valid JSON by parsing it
        let parsed = Json.decodeText @Json.Value result
        case parsed of
          Ok _ -> pass
          Err _err -> fail "Result is not valid JSON"


    describe "authorization" do
      describe "Unauthenticated error (401)" do
        it "returns Unauthenticated when query requires auth and no user provided" \_ -> do
          store <- InMemoryStore.new @AuthenticatedQuery |> Task.mapError toText
          queryId <- Uuid.generate

          let query = AuthenticatedQuery {authQueryId = queryId, authQueryOwnerId = "user-1", authQueryData = "test"}
          store.atomicUpdate queryId (\_ -> Just query) |> Task.mapError errorToText

          result <- Endpoint.createQueryEndpoint @AuthenticatedQuery store Nothing |> Task.asResult

          case result of
            Err (Auth.AuthorizationError Unauthenticated) -> pass
            Err other -> fail [fmt|Expected Unauthenticated, got: #{ToText.toText other}|]
            Ok _ -> fail "Expected Unauthenticated error, but query succeeded"

      describe "Forbidden error (403)" do
        it "filters out queries user doesn't own" \_ -> do
          store <- InMemoryStore.new @OwnerOnlyQuery |> Task.mapError toText
          id1 <- Uuid.generate
          id2 <- Uuid.generate

          -- Create two queries with different owners
          let query1 = OwnerOnlyQuery {ownerQueryId = id1, ownerId = "user-1", ownerQueryData = "owned"}
          let query2 = OwnerOnlyQuery {ownerQueryId = id2, ownerId = "user-2", ownerQueryData = "not-owned"}
          store.atomicUpdate id1 (\_ -> Just query1) |> Task.mapError errorToText
          store.atomicUpdate id2 (\_ -> Just query2) |> Task.mapError errorToText

          -- User-1 should only see their own query
          let user1 = testUser "user-1" []
          result <- Endpoint.createQueryEndpoint @OwnerOnlyQuery store (Just user1) |> Task.mapError endpointErrorToText

          let decoded = Json.decodeText @(Array OwnerOnlyQuery) result
          case decoded of
            Ok queries -> do
              Array.length queries |> shouldBe 1
              case queries |> Array.get 0 of
                Just q -> q.ownerId |> shouldBe "user-1"
                Nothing -> fail "Expected query not found"
            Err _err -> fail "Failed to decode result"

        it "returns empty array when user owns nothing" \_ -> do
          store <- InMemoryStore.new @OwnerOnlyQuery |> Task.mapError toText
          queryId <- Uuid.generate

          let query = OwnerOnlyQuery {ownerQueryId = queryId, ownerId = "user-1", ownerQueryData = "test"}
          store.atomicUpdate queryId (\_ -> Just query) |> Task.mapError errorToText

          -- User-2 owns nothing, should see empty array
          let user2 = testUser "user-2" []
          result <- Endpoint.createQueryEndpoint @OwnerOnlyQuery store (Just user2) |> Task.mapError endpointErrorToText

          let decoded = Json.decodeText @(Array OwnerOnlyQuery) result
          case decoded of
            Ok queries -> Array.length queries |> shouldBe 0
            Err _err -> fail "Failed to decode result"

      describe "InsufficientPermissions error (403)" do
        it "returns InsufficientPermissions when user lacks required permission" \_ -> do
          store <- InMemoryStore.new @PermissionQuery |> Task.mapError toText
          queryId <- Uuid.generate

          let query = PermissionQuery {permQueryId = queryId, permQueryData = "test"}
          store.atomicUpdate queryId (\_ -> Just query) |> Task.mapError errorToText

          -- User without admin:read permission
          let userWithoutPerm = testUser "user-1" ["other:permission"]
          result <- Endpoint.createQueryEndpoint @PermissionQuery store (Just userWithoutPerm) |> Task.asResult

          case result of
            Err (Auth.AuthorizationError (InsufficientPermissions perms)) -> do
              perms |> shouldBe ["admin:read"]
            Err other -> fail [fmt|Expected InsufficientPermissions, got: #{ToText.toText other}|]
            Ok _ -> fail "Expected InsufficientPermissions error, but query succeeded"

        it "succeeds when user has required permission" \_ -> do
          store <- InMemoryStore.new @PermissionQuery |> Task.mapError toText
          queryId <- Uuid.generate

          let query = PermissionQuery {permQueryId = queryId, permQueryData = "test"}
          store.atomicUpdate queryId (\_ -> Just query) |> Task.mapError errorToText

          -- User with admin:read permission
          let adminUser = testUser "admin-1" ["admin:read"]
          result <- Endpoint.createQueryEndpoint @PermissionQuery store (Just adminUser) |> Task.mapError endpointErrorToText

          let decoded = Json.decodeText @(Array PermissionQuery) result
          case decoded of
            Ok queries -> Array.length queries |> shouldBe 1
            Err _err -> fail "Failed to decode result"

      describe "two-phase authorization" do
        it "canAccess runs before data fetch, canView filters after" \_ -> do
          store <- InMemoryStore.new @OwnerOnlyQuery |> Task.mapError toText
          id1 <- Uuid.generate
          id2 <- Uuid.generate
          id3 <- Uuid.generate

          -- Create multiple queries with different owners
          let query1 = OwnerOnlyQuery {ownerQueryId = id1, ownerId = "user-1", ownerQueryData = "data1"}
          let query2 = OwnerOnlyQuery {ownerQueryId = id2, ownerId = "user-1", ownerQueryData = "data2"}
          let query3 = OwnerOnlyQuery {ownerQueryId = id3, ownerId = "user-2", ownerQueryData = "data3"}
          store.atomicUpdate id1 (\_ -> Just query1) |> Task.mapError errorToText
          store.atomicUpdate id2 (\_ -> Just query2) |> Task.mapError errorToText
          store.atomicUpdate id3 (\_ -> Just query3) |> Task.mapError errorToText

          -- User-1 passes canAccess (authenticated), canView filters to their data
          let user1 = testUser "user-1" []
          result <- Endpoint.createQueryEndpoint @OwnerOnlyQuery store (Just user1) |> Task.mapError endpointErrorToText

          let decoded = Json.decodeText @(Array OwnerOnlyQuery) result
          case decoded of
            Ok queries -> do
              -- User-1 should see their 2 queries, not user-2's
              Array.length queries |> shouldBe 2
              -- Check no queries belong to other users (using any with negation)
              let hasOtherOwner = queries |> Array.any (\q -> q.ownerId != "user-1")
              hasOtherOwner |> shouldBe False
            Err _err -> fail "Failed to decode result"


-- | Convert QueryObjectStore Error to Text.
errorToText :: Service.QueryObjectStore.Core.Error -> Text
errorToText err = case err of
  Service.QueryObjectStore.Core.StorageError msg -> msg
  SerializationError msg -> msg


-- | Convert QueryEndpointError to Text for tests.
endpointErrorToText :: QueryEndpointError -> Text
endpointErrorToText err = case err of
  Auth.AuthorizationError authErr -> ToText.toText authErr
  Auth.StorageError msg -> msg
