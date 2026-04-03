module Service.MultiTenantSpec where

import Array qualified
import Auth.Claims (UserClaims (..))
import Core
import Map qualified
import Service.Auth qualified as Auth
import Service.CommandExecutor.Core (ExecutionResult (..))
import Service.CommandExecutor qualified as CommandExecutor
import Service.EntityFetcher.Core (EntityFetcher, EntityFetchResult (..), FetchedEntity (..))
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event qualified as Event
import Service.Event.EntityName (EntityName (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId (StreamId (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Service.Query.Auth (QueryAuthError (..))
import Service.Query.Auth qualified as Auth
import Task qualified
import Test
import Test.Service.Command.Core (
  AddItemToCart (..),
  CartEntity (..),
  TenantAddItem (..),
  TenantCreateCart (..),
  TenantCartEntity (..),
  TenantCartEvent (..),
 )
import Test.Service.EventStore.Core (CartEvent (..))
import Text qualified
import Uuid qualified


spec :: Spec Unit
spec = do
  describe "Multi-Tenant Support" do
    describe "StreamId.withTenant" do
      streamIdSpecs

    describe "tenantOnly auth helper" do
      tenantOnlySpecs

    describe "Single-tenant regression" do
      singleTenantRegressionSpecs

    describe "Multi-tenant command execution" do
      multiTenantExecutionSpecs

    describe "Stream isolation" do
      streamIsolationSpecs


-- ============================================================================
-- Group 1: StreamId.withTenant
-- ============================================================================

streamIdSpecs :: Spec Unit
streamIdSpecs = do
  it "prefixes stream ID with tenant UUID" \_ -> do
    tenantUuid <- Uuid.generate
    let streamId = StreamId "entity-123"
    let result = StreamId.withTenant tenantUuid streamId
    let resultText = StreamId.toText result
    let expectedPrefix = [fmt|tenant-#{Uuid.toText tenantUuid}/|]
    resultText |> Text.startsWith expectedPrefix |> shouldBe True

  it "withTenant with nil UUID" \_ -> do
    let streamId = StreamId "abc"
    let result = StreamId.withTenant Uuid.nil streamId
    let resultText = StreamId.toText result
    resultText |> shouldBe "tenant-00000000-0000-0000-0000-000000000000/abc"

  it "preserves original stream ID text after prefix" \_ -> do
    tenantUuid <- Uuid.generate
    let original = "my-stream-id"
    let result = StreamId.withTenant tenantUuid (StreamId original)
    let resultText = StreamId.toText result
    resultText |> Text.endsWith original |> shouldBe True

  it "result stays within maxLength for typical inputs" \_ -> do
    tenantUuid <- Uuid.generate
    entityUuid <- Uuid.generate
    let entityStreamId = StreamId (Uuid.toText entityUuid)
    let result = StreamId.withTenant tenantUuid entityStreamId
    let resultLength = Text.length (StreamId.toText result)
    (resultLength < StreamId.maxLength) |> shouldBe True


-- ============================================================================
-- Group 2: tenantOnly auth helper
-- ============================================================================

data TenantQuery = TenantQuery
  { tenantQueryId :: Uuid,
    tenantId :: Text
  }
  deriving (Generic)


tenantOnlySpecs :: Spec Unit
tenantOnlySpecs = do
  it "allows access when tenant IDs match" \_ -> do
    let claims = claimsWithTenant (Just "abc-123")
    let query = TenantQuery {tenantQueryId = Uuid.nil, tenantId = "abc-123"}
    let result = Auth.tenantOnly (\q -> q.tenantId) (Just claims) query
    result |> shouldBe Nothing

  it "rejects when tenant IDs differ" \_ -> do
    let claims = claimsWithTenant (Just "abc-123")
    let query = TenantQuery {tenantQueryId = Uuid.nil, tenantId = "xyz-789"}
    let result = Auth.tenantOnly (\q -> q.tenantId) (Just claims) query
    result |> shouldBe (Just Forbidden)

  it "rejects unauthenticated users" \_ -> do
    let query = TenantQuery {tenantQueryId = Uuid.nil, tenantId = "abc-123"}
    let result = Auth.tenantOnly (\q -> q.tenantId) Nothing query
    result |> shouldBe (Just Unauthenticated)

  it "rejects when claims have no tenantId" \_ -> do
    let claims = claimsWithTenant Nothing
    let query = TenantQuery {tenantQueryId = Uuid.nil, tenantId = "abc-123"}
    let result = Auth.tenantOnly (\q -> q.tenantId) (Just claims) query
    result |> shouldBe (Just Forbidden)

  it "is case-insensitive (uppercase claim, lowercase query)" \_ -> do
    let claims = claimsWithTenant (Just "ABC-123")
    let query = TenantQuery {tenantQueryId = Uuid.nil, tenantId = "abc-123"}
    let result = Auth.tenantOnly (\q -> q.tenantId) (Just claims) query
    result |> shouldBe Nothing

  it "is case-insensitive (lowercase claim, uppercase query)" \_ -> do
    let claims = claimsWithTenant (Just "abc-123")
    let query = TenantQuery {tenantQueryId = Uuid.nil, tenantId = "ABC-123"}
    let result = Auth.tenantOnly (\q -> q.tenantId) (Just claims) query
    result |> shouldBe Nothing

  it "rejects empty string tenant in claims against non-empty query" \_ -> do
    let claims = claimsWithTenant (Just "")
    let query = TenantQuery {tenantQueryId = Uuid.nil, tenantId = "abc-123"}
    let result = Auth.tenantOnly (\q -> q.tenantId) (Just claims) query
    result |> shouldBe (Just Forbidden)

  it "allows when both tenant IDs are empty strings" \_ -> do
    let claims = claimsWithTenant (Just "")
    let query = TenantQuery {tenantQueryId = Uuid.nil, tenantId = ""}
    let result = Auth.tenantOnly (\q -> q.tenantId) (Just claims) query
    result |> shouldBe Nothing


-- ============================================================================
-- Group 3: Single-tenant regression
-- ============================================================================

singleTenantRegressionSpecs :: Spec Unit
singleTenantRegressionSpecs = do
  it "single-tenant command executes as before (no regression)" \_ -> do
    (store, fetcher) <- newCartStoreAndFetcher
    cartId <- Uuid.generate
    createCart store cartId
    let cmd = AddItemToCart {cartId = cartId, itemId = Uuid.nil, amount = 5}
    result <- CommandExecutor.execute store fetcher cartEntityName Auth.emptyContext cmd
    case result of
      CommandAccepted {} -> pass
      other -> fail [fmt|Expected CommandAccepted, got #{toText other}|]

  it "single-tenant command with auth context still works" \_ -> do
    (store, fetcher) <- newCartStoreAndFetcher
    cartId <- Uuid.generate
    createCart store cartId
    ctx <- Auth.authenticatedContext testClaims
    let cmd = AddItemToCart {cartId = cartId, itemId = Uuid.nil, amount = 5}
    result <- CommandExecutor.execute store fetcher cartEntityName ctx cmd
    case result of
      CommandAccepted {} -> pass
      other -> fail [fmt|Expected CommandAccepted, got #{toText other}|]

  it "single-tenant reject still works" \_ -> do
    (store, fetcher) <- newCartStoreAndFetcher
    let cmd = AddItemToCart {cartId = Uuid.nil, itemId = Uuid.nil, amount = 5}
    result <- CommandExecutor.execute store fetcher cartEntityName Auth.emptyContext cmd
    case result of
      CommandRejected {reason} -> reason |> shouldBe "Cart does not exist"
      other -> fail [fmt|Expected CommandRejected, got #{toText other}|]


-- ============================================================================
-- Group 4: Multi-tenant command execution
-- ============================================================================

multiTenantExecutionSpecs :: Spec Unit
multiTenantExecutionSpecs = do
  it "multi-tenant command succeeds with valid tenant" \_ -> do
    (store, fetcher) <- newTenantStoreAndFetcher
    tenantUuid <- Uuid.generate
    cartId <- Uuid.generate
    createTenantCart store tenantUuid cartId
    ctx <- tenantContext tenantUuid
    let cmd = TenantAddItem {cartId = cartId, itemId = Uuid.nil, amount = 5}
    result <- CommandExecutor.execute store fetcher tenantCartEntityName ctx cmd
    case result of
      CommandAccepted {} -> pass
      other -> fail [fmt|Expected CommandAccepted, got #{toText other}|]

  it "multi-tenant command produces tenant-prefixed stream ID" \_ -> do
    (store, fetcher) <- newTenantStoreAndFetcher
    tenantUuid <- Uuid.generate
    cartId <- Uuid.generate
    createTenantCart store tenantUuid cartId
    ctx <- tenantContext tenantUuid
    let cmd = TenantAddItem {cartId = cartId, itemId = Uuid.nil, amount = 3}
    result <- CommandExecutor.execute store fetcher tenantCartEntityName ctx cmd
    case result of
      CommandAccepted {streamId} -> do
        let streamText = StreamId.toText streamId
        let expectedPrefix = [fmt|tenant-#{Uuid.toText tenantUuid}/|]
        streamText |> Text.startsWith expectedPrefix |> shouldBe True
      other -> fail [fmt|Expected CommandAccepted, got #{toText other}|]

  it "multi-tenant creation command passes tenant UUID to decide" \_ -> do
    (store, fetcher) <- newTenantStoreAndFetcher
    tenantUuid <- Uuid.generate
    ctx <- tenantContext tenantUuid
    let cmd = TenantCreateCart
    result <- CommandExecutor.execute store fetcher tenantCartEntityName ctx cmd
    case result of
      CommandAccepted {streamId} -> do
        -- Verify the stream ID is tenant-prefixed
        let streamText = StreamId.toText streamId
        let expectedPrefix = [fmt|tenant-#{Uuid.toText tenantUuid}/|]
        streamText |> Text.startsWith expectedPrefix |> shouldBe True
      other -> fail [fmt|Expected CommandAccepted, got #{toText other}|]

  it "rejects when no user (unauthenticated)" \_ -> do
    (store, fetcher) <- newTenantStoreAndFetcher
    let cmd = TenantAddItem {cartId = Uuid.nil, itemId = Uuid.nil, amount = 5}
    result <- CommandExecutor.execute store fetcher tenantCartEntityName Auth.emptyContext cmd
    case result of
      CommandRejected {reason} -> reason |> shouldBe "Unauthorized"
      other -> fail [fmt|Expected CommandRejected, got #{toText other}|]

  it "rejects when no tenantId in claims" \_ -> do
    (store, fetcher) <- newTenantStoreAndFetcher
    ctx <- Auth.authenticatedContext (claimsWithTenant Nothing)
    let cmd = TenantAddItem {cartId = Uuid.nil, itemId = Uuid.nil, amount = 5}
    result <- CommandExecutor.execute store fetcher tenantCartEntityName ctx cmd
    case result of
      CommandRejected {reason} -> reason |> shouldBe "Forbidden"
      other -> fail [fmt|Expected CommandRejected, got #{toText other}|]

  it "rejects when tenantId is not valid UUID" \_ -> do
    (store, fetcher) <- newTenantStoreAndFetcher
    ctx <- Auth.authenticatedContext (claimsWithTenant (Just "not-a-uuid"))
    let cmd = TenantAddItem {cartId = Uuid.nil, itemId = Uuid.nil, amount = 5}
    result <- CommandExecutor.execute store fetcher tenantCartEntityName ctx cmd
    case result of
      CommandRejected {reason} -> reason |> shouldBe "Forbidden"
      other -> fail [fmt|Expected CommandRejected, got #{toText other}|]

  it "rejects when tenantId is empty string" \_ -> do
    (store, fetcher) <- newTenantStoreAndFetcher
    ctx <- Auth.authenticatedContext (claimsWithTenant (Just ""))
    let cmd = TenantAddItem {cartId = Uuid.nil, itemId = Uuid.nil, amount = 5}
    result <- CommandExecutor.execute store fetcher tenantCartEntityName ctx cmd
    case result of
      CommandRejected {reason} -> reason |> shouldBe "Forbidden"
      other -> fail [fmt|Expected CommandRejected, got #{toText other}|]


-- ============================================================================
-- Group 5: Stream isolation
-- ============================================================================

streamIsolationSpecs :: Spec Unit
streamIsolationSpecs = do
  it "different tenants produce different stream IDs for same entity" \_ -> do
    tenantA <- Uuid.generate
    tenantB <- Uuid.generate
    let entityStreamId = StreamId "entity-123"
    let streamA = StreamId.withTenant tenantA entityStreamId
    let streamB = StreamId.withTenant tenantB entityStreamId
    (streamA == streamB) |> shouldBe False

  it "same tenant and entity produces same stream ID" \_ -> do
    tenantUuid <- Uuid.generate
    let entityStreamId = StreamId "entity-123"
    let stream1 = StreamId.withTenant tenantUuid entityStreamId
    let stream2 = StreamId.withTenant tenantUuid entityStreamId
    stream1 |> shouldBe stream2

  it "multi-tenant events are fetched from tenant-scoped stream" \_ -> do
    (store, fetcher) <- newTenantStoreAndFetcher
    tenantUuid <- Uuid.generate
    cartId <- Uuid.generate
    createTenantCart store tenantUuid cartId
    ctx <- tenantContext tenantUuid
    let cmd = TenantAddItem {cartId = cartId, itemId = Uuid.nil, amount = 7}
    result <- CommandExecutor.execute store fetcher tenantCartEntityName ctx cmd
    case result of
      CommandAccepted {streamId} -> do
        -- Fetch the entity from the tenant-scoped stream
        fetchResult <-
          fetcher.fetch tenantCartEntityName streamId
            |> Task.mapError toText
        case fetchResult of
          EntityFound entity -> do
            entity.state.cartId |> shouldBe cartId
            Array.length entity.state.cartItems |> shouldBe 1
          EntityNotFound -> fail "Expected entity to be found in tenant-scoped stream"
      other -> fail [fmt|Expected CommandAccepted, got #{toText other}|]

  it "tenant A cannot see tenant B's entity" \_ -> do
    (store, fetcher) <- newTenantStoreAndFetcher
    tenantA <- Uuid.generate
    tenantB <- Uuid.generate
    cartId <- Uuid.generate
    -- Create cart as tenant A
    createTenantCart store tenantA cartId
    -- Try to add item as tenant B (same cartId)
    ctxB <- tenantContext tenantB
    let cmd = TenantAddItem {cartId = cartId, itemId = Uuid.nil, amount = 5}
    result <- CommandExecutor.execute store fetcher tenantCartEntityName ctxB cmd
    -- Tenant B should not see tenant A's cart (different stream)
    case result of
      CommandRejected {reason} -> reason |> shouldBe "Cart does not exist"
      other -> fail [fmt|Expected CommandRejected (cart not found), got #{toText other}|]


-- ============================================================================
-- Test Helpers
-- ============================================================================

testClaims :: UserClaims
testClaims =
  UserClaims
    { sub = "test-user-123",
      email = Just "test@example.com",
      name = Just "Test User",
      permissions = Array.empty,
      tenantId = Just "00000000-0000-0000-0000-000000000001",
      rawClaims = Map.empty
    }


claimsWithTenant :: Maybe Text -> UserClaims
claimsWithTenant maybeTenant =
  UserClaims
    { sub = "test-user-123",
      email = Just "test@example.com",
      name = Just "Test User",
      permissions = Array.empty,
      tenantId = maybeTenant,
      rawClaims = Map.empty
    }


tenantContext :: Uuid -> Task _ Auth.RequestContext
tenantContext tenantUuid = do
  Auth.authenticatedContext (claimsWithTenant (Just (Uuid.toText tenantUuid)))


cartEntityName :: Event.EntityName
cartEntityName = EntityName "CartEntity"


tenantCartEntityName :: Event.EntityName
tenantCartEntityName = EntityName "TenantCartEntity"


newCartStoreAndFetcher :: Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent)
newCartStoreAndFetcher = do
  store <- InMemory.new |> Task.map (EventStore.castEventStore @CartEvent) |> Task.mapError toText
  fetcher <-
    EntityFetcher.new
      store
      (initialStateImpl @CartEntity)
      (updateImpl @CartEntity)
      |> Task.mapError toText
  Task.yield (store, fetcher)


newTenantStoreAndFetcher :: Task Text (EventStore.EventStore TenantCartEvent, EntityFetcher TenantCartEntity TenantCartEvent)
newTenantStoreAndFetcher = do
  store <- InMemory.new |> Task.map (EventStore.castEventStore @TenantCartEvent) |> Task.mapError toText
  fetcher <-
    EntityFetcher.new
      store
      (initialStateImpl @TenantCartEntity)
      (updateImpl @TenantCartEntity)
      |> Task.mapError toText
  Task.yield (store, fetcher)


createCart :: EventStore.EventStore CartEvent -> Uuid -> Task Text Unit
createCart store cartId = do
  let event = CartCreated {entityId = cartId}
  eventId <- Uuid.generate
  metadata <- EventMetadata.new
  let metadata' = metadata {EventMetadata.localPosition = Just (Event.StreamPosition 0), EventMetadata.eventId = eventId}
  let insertion = Event.Insertion {id = eventId, event = event, metadata = metadata'}
  let payload =
        Event.InsertionPayload
          { streamId = cartId |> Uuid.toText |> StreamId.fromTextUnsafe,
            entityName = cartEntityName,
            insertionType = Event.StreamCreation,
            insertions = [insertion]
          }
  payload |> store.insert |> Task.mapError toText |> discard


createTenantCart :: EventStore.EventStore TenantCartEvent -> Uuid -> Uuid -> Task Text Unit
createTenantCart store tenantUuid cartId = do
  let event = TenantCartCreated {entityId = cartId, tenantId = tenantUuid}
  eventId <- Uuid.generate
  metadata <- EventMetadata.new
  let metadata' = metadata {EventMetadata.localPosition = Just (Event.StreamPosition 0), EventMetadata.eventId = eventId}
  let insertion = Event.Insertion {id = eventId, event = event, metadata = metadata'}
  let streamId = StreamId.withTenant tenantUuid (cartId |> Uuid.toText |> StreamId.fromTextUnsafe)
  let payload =
        Event.InsertionPayload
          { streamId = streamId,
            entityName = tenantCartEntityName,
            insertionType = Event.StreamCreation,
            insertions = [insertion]
          }
  payload |> store.insert |> Task.mapError toText |> discard
