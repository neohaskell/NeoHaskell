module Service.Query.EndpointSpec where

import Array qualified
import Core
import Json qualified
import Service.Query.Auth (QueryEndpointError)
import Service.Query.Auth qualified as Auth
import Service.Query.Core qualified
import Service.Query.Endpoint qualified as Endpoint
import Service.QueryObjectStore.Core (Error (..), QueryObjectStore (..))
import Service.QueryObjectStore.InMemory qualified as InMemoryStore
import Task qualified
import Test
import ToText qualified
import Uuid qualified


-- | Test query type
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
