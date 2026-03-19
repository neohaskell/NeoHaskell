{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module OutboundIntegrationSpec where

import Array qualified
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import ConcurrentMap qualified
import Core
import Data.Proxy (Proxy (..))
import DateTime qualified
import Default (Default (..))
import Data.Hashable qualified as GhcHashable
import GHC.Enum qualified as GhcEnum
import GHC.TypeLits (symbolVal)
import Integration qualified
import Integration.Command qualified as Command
import Json qualified
import Map qualified
import Service.Application.Integrations (createTypedOutboundRunner)
import Service.Event (Event (..))
import Service.Event.EntityName (EntityName (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.StreamId (StreamId)
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.InMemory qualified as InMemory
import Service.Integration.Types (OutboundRunner (..))
import Service.OutboundIntegration.Core (OutboundIntegration (..))
import Service.OutboundIntegration.TH (outboundIntegration)
import Task qualified
import Test
import Text qualified
import TypeName qualified
import Uuid qualified


-- ============================================================================
-- Test-only types (NOT exported, NOT part of the library)
-- ============================================================================

-- | Test event ADT
data TestEvent
  = ItemAdded {addedItemId :: Int, addedQuantity :: Int}
  | ItemRemoved {removedItemId :: Int}
  | EntityCreated {createdId :: Uuid, createdName :: Text}
  deriving (Eq, Show, Generic)


instance Json.ToJSON TestEvent


instance Json.FromJSON TestEvent


-- | Test entity
data TestEntity = TestEntity
  { entityId :: Uuid
  , entityName :: Text
  , itemCount :: Int
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON TestEntity


instance Json.FromJSON TestEntity


-- | Type family instance: TestEntity's events are TestEvent
type instance EventOf TestEntity = TestEvent


instance Entity TestEntity where
  initialStateImpl = TestEntity {entityId = Uuid.nil, entityName = "", itemCount = 0}
  updateImpl event entity =
    case event of
      ItemAdded {addedItemId = _, addedQuantity} ->
        entity {itemCount = entity.itemCount + addedQuantity}
      ItemRemoved {removedItemId = _} ->
        entity {itemCount = entity.itemCount - 1}
      EntityCreated {createdId, createdName} ->
        entity {entityId = createdId, entityName = createdName}


instance Default TestEntity where
  def = initialStateImpl @TestEntity


-- | Test command for Integration.batch actions
data TestIntegrationCommand = TestIntegrationCommand
  { commandValue :: Int
  }
  deriving (Generic, Eq, Show, Typeable)


instance Json.ToJSON TestIntegrationCommand


instance Json.FromJSON TestIntegrationCommand


type instance NameOf TestIntegrationCommand = "TestIntegrationCommand"


-- | Handler type A (happy path handler)
data TestHandlerA = TestHandlerA
  deriving (Generic, Typeable, Show)


-- | EntityOf instance for TestHandlerA
type instance EntityOf TestHandlerA = TestEntity


-- | handleEvent function for TestHandlerA.
-- Matches ItemAdded and emits a command; returns Integration.none for all others.
-- IMPORTANT: This must appear BEFORE the outboundIntegration splice.
handleEvent :: TestEntity -> TestEvent -> Integration.Outbound
handleEvent _entity event =
  case event of
    ItemAdded {addedItemId, addedQuantity} ->
      Integration.batch
        [ Integration.outbound
            Command.Emit
              { command = TestIntegrationCommand {commandValue = addedItemId + addedQuantity}
              }
        ]
    _ -> Integration.none


-- | TH splice: generates OutboundIntegration instance, NameOf, and KnownHash.
-- IMPORTANT: handleEvent and all type instances MUST appear BEFORE this splice.
outboundIntegration ''TestHandlerA


-- ============================================================================
-- Test Helper Functions
-- ============================================================================

-- | Verify OutboundIntegration instance exists (compile-time check)
instanceExists :: forall handler. (OutboundIntegration handler) => Proxy handler -> ()
instanceExists _ = ()


-- | Get the NameOf a handler type as Text
nameOfHandler :: forall handler. (KnownSymbol (NameOf handler)) => Proxy handler -> Text
nameOfHandler _ =
  symbolVal (Proxy :: Proxy (NameOf handler))
    |> Text.fromLinkedList


-- | Default test entity for use in tests
testEntity :: TestEntity
testEntity = TestEntity {entityId = Uuid.nil, entityName = "test-entity", itemCount = 0}


-- | Create an ActionContext for processEvent tests
makeContext :: Task Text Integration.ActionContext
makeContext = do
  store <- InMemorySecretStore.new
  locks <- ConcurrentMap.new
  Task.yield
    Integration.ActionContext
      { Integration.secretStore = store
      , Integration.providerRegistry = Integration.fromMap Map.empty
      , Integration.refreshLocks = locks
      , Integration.fileAccess = Nothing
      }


-- | Create a raw Event Json.Value for processEvent tests
makeRawEvent :: Text -> Text -> Json.Value -> Task Text (Event Json.Value)
makeRawEvent entityTypeName streamIdText eventPayload = do
  let streamId = StreamId.fromTextUnsafe streamIdText
  now <- DateTime.now
  Task.yield
    Event
      { entityName = EntityName entityTypeName
      , streamId = streamId
      , event = eventPayload
      , metadata =
          EventMetadata
            { eventId = Uuid.nil
            , relatedUserSub = Nothing
            , correlationId = Nothing
            , causationId = Nothing
            , createdAt = now
            , localPosition = Nothing
            , globalPosition = Nothing
            }
      }


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "OutboundIntegration" do

    describe "outboundIntegration TH macro" do

      describe "instance generation" do

        -- Test 1: The test file compiles — if it compiles, the instance exists.
        it "generates a compilable OutboundIntegration instance" \_ -> do
          let _ = instanceExists (Proxy :: Proxy TestHandlerA)
          pass

        -- Test 2: NameOf type instance reflects handler name
        it "generates a NameOf type instance with the handler's type name" \_ -> do
          symbolVal (Proxy :: Proxy (NameOf TestHandlerA))
            |> Text.fromLinkedList
            |> shouldBe "TestHandlerA"

        -- Test 3: KnownHash instance compiles and is usable
        it "generates a KnownHash instance for the handler name" \_ -> do
          let hash = hashVal (Proxy :: Proxy "TestHandlerA")
          let _ = hash
          pass

      describe "handleEventImpl dispatch" do

        -- Test 4: Matching event constructor → non-empty outbound
        it "delegates to handleEvent for a matching event constructor" \_ -> do
          let result = handleEventImpl @TestHandlerA testEntity (ItemAdded {addedItemId = 1, addedQuantity = 5})
          Integration.getActions result
            |> Array.length
            |> shouldBeGreaterThan 0

        -- Test 5: Non-matching event (wildcard) → zero actions
        it "delegates to handleEvent for a non-matching event (wildcard)" \_ -> do
          let result = handleEventImpl @TestHandlerA testEntity (ItemRemoved {removedItemId = 1})
          Integration.getActions result
            |> Array.length
            |> shouldBe 0

        -- Test 6: Third event constructor (wildcard) → zero actions
        it "delegates to handleEvent for a third event constructor" \_ -> do
          let result = handleEventImpl @TestHandlerA testEntity (EntityCreated {createdId = Uuid.nil, createdName = "test"})
          Integration.getActions result
            |> Array.length
            |> shouldBe 0

      describe "handleEventImpl edge cases" do

        -- Test 7: Event with zero field values
        it "handles event with zero field values" \_ -> do
          let result = handleEventImpl @TestHandlerA testEntity (ItemAdded {addedItemId = 0, addedQuantity = 0})
          Integration.getActions result
            |> Array.length
            |> shouldBeGreaterThan 0

        -- Test 8: Event with negative field values
        it "handles event with negative field values" \_ -> do
          let result = handleEventImpl @TestHandlerA testEntity (ItemAdded {addedItemId = -1, addedQuantity = -5})
          Integration.getActions result
            |> Array.length
            |> shouldBeGreaterThan 0

        -- Test 9: Event with maxBound Int field values
        it "handles event with maxBound Int field values" \_ -> do
          let result = handleEventImpl @TestHandlerA testEntity (ItemAdded {addedItemId = GhcEnum.maxBound, addedQuantity = GhcEnum.maxBound})
          Integration.getActions result
            |> Array.length
            |> shouldBeGreaterThan 0

        -- Test 10: Entity with empty text and zero fields
        it "handles entity with empty text and zero fields" \_ -> do
          let emptyEntity = TestEntity {entityId = Uuid.nil, entityName = "", itemCount = 0}
          let result = handleEventImpl @TestHandlerA emptyEntity (ItemAdded {addedItemId = 1, addedQuantity = 1})
          Integration.getActions result
            |> Array.length
            |> shouldBeGreaterThan 0

        -- Test 11: Default entity state
        it "handles default entity state from Default instance" \_ -> do
          let result = handleEventImpl @TestHandlerA (def :: TestEntity) (ItemAdded {addedItemId = 42, addedQuantity = 10})
          Integration.getActions result
            |> Array.length
            |> shouldBeGreaterThan 0

      describe "HandledEvent associated type" do

        -- Test 20: HandledEvent resolves to the full event ADT
        it "resolves HandledEvent to the full event ADT type" \_ -> do
          let result = handleEventImpl @TestHandlerA testEntity (ItemAdded {addedItemId = 1, addedQuantity = 1})
          Integration.getActions result
            |> Array.length
            |> shouldBeGreaterThan 0

        -- Test 21: All constructors of the event ADT are accepted
        it "accepts all constructors of the event ADT" \_ -> do
          let resultA = handleEventImpl @TestHandlerA testEntity (ItemAdded {addedItemId = 1, addedQuantity = 1})
          let resultB = handleEventImpl @TestHandlerA testEntity (ItemRemoved {removedItemId = 1})
          let resultC = handleEventImpl @TestHandlerA testEntity (EntityCreated {createdId = Uuid.nil, createdName = "test"})
          Integration.getActions resultA |> Array.length |> shouldBeGreaterThan 0
          Integration.getActions resultB |> Array.length |> shouldBe 0
          Integration.getActions resultC |> Array.length |> shouldBe 0

    describe "NameOf type instance" do

      -- Test 16: NameOf reflects exact handler type name
      it "reflects the exact handler type name as a Symbol" \_ -> do
        symbolVal (Proxy :: Proxy (NameOf TestHandlerA))
          |> Text.fromLinkedList
          |> shouldBe "TestHandlerA"

      -- Test 17: NameOf usable with KnownSymbol constraint via helper
      it "is usable with KnownSymbol constraint via helper" \_ -> do
        nameOfHandler (Proxy :: Proxy TestHandlerA)
          |> shouldBe "TestHandlerA"

    describe "KnownHash instance" do

      -- Test 18: Generated hash matches Hashable.hash of the handler name string
      it "produces a hash matching Hashable.hash of the handler name" \_ -> do
        let generatedHash = hashVal (Proxy :: Proxy "TestHandlerA")
        let expectedHash = GhcHashable.hash ("TestHandlerA" :: LinkedList Char)
        generatedHash |> shouldBe expectedHash

      -- Test 19: Non-zero hash value (sanity check)
      it "produces a non-zero hash value" \_ -> do
        let hash = hashVal (Proxy :: Proxy "TestHandlerA")
        hash |> shouldSatisfy (\h -> h != 0)

    describe "createTypedOutboundRunner" do

      describe "entityTypeName" do

        -- Test 22: entityTypeName matches TypeName.reflect @TestEntity
        it "sets entityTypeName to the entity's reflected type name" \_ -> do
          let runner = createTypedOutboundRunner @TestHandlerA
          runner.entityTypeName |> shouldBe (TypeName.reflect @TestEntity)

        -- Test 23: entityTypeName is non-empty
        it "produces a non-empty entityTypeName" \_ -> do
          let runner = createTypedOutboundRunner @TestHandlerA
          runner.entityTypeName |> shouldSatisfy (\name -> not (Text.isEmpty name))

        -- Test 24: entityTypeName contains "TestEntity"
        it "entityTypeName contains the entity type name" \_ -> do
          let runner = createTypedOutboundRunner @TestHandlerA
          runner.entityTypeName |> shouldSatisfy (Text.contains "TestEntity")

      describe "processEvent" do

        -- Test 25: processEvent decodes valid event JSON and dispatches to handler
        it "decodes valid event JSON and dispatches to handler" \_ -> do
          eventStore <- InMemory.new |> Task.mapError toText
          ctx <- makeContext
          let runner = createTypedOutboundRunner @TestHandlerA
          let encodedEvent = Json.encode (ItemAdded {addedItemId = 1, addedQuantity = 5})
          rawEvent <- makeRawEvent "TestEntity" "stream-001" encodedEvent
          result <- runner.processEvent ctx eventStore rawEvent
          -- Verify exactly one command payload was emitted
          Array.length result |> shouldBe 1
          -- Verify the payload decodes to the expected command (commandValue = 1 + 5 = 6)
          case Array.first result of
            Nothing -> fail "Expected a command payload but got none"
            Just payload -> do
              case Json.decode @TestIntegrationCommand payload.commandData of
                Err err -> fail [fmt|Failed to decode command payload: #{err}|]
                Ok cmd -> cmd.commandValue |> shouldBe 6

        -- Test 26: processEvent returns empty array for undecodable event JSON
        it "returns empty array for undecodable event JSON" \_ -> do
          eventStore <- InMemory.new |> Task.mapError toText
          ctx <- makeContext
          let runner = createTypedOutboundRunner @TestHandlerA
          rawEvent <- makeRawEvent "TestEntity" "stream-002" Json.null
          result <- runner.processEvent ctx eventStore rawEvent
          Array.length result |> shouldBe 0

        -- Test 27: processEvent handles entity with no prior events (uses initial state)
        it "handles entity with no prior events using initial state" \_ -> do
          eventStore <- InMemory.new |> Task.mapError toText
          ctx <- makeContext
          let runner = createTypedOutboundRunner @TestHandlerA
          let encodedEvent = Json.encode (ItemRemoved {removedItemId = 1})
          rawEvent <- makeRawEvent "TestEntity" "stream-003" encodedEvent
          result <- runner.processEvent ctx eventStore rawEvent
          Array.length result |> shouldBe 0

    describe "Integration.Outbound via handleEventImpl" do

      -- Test 28: Handler returning Integration.none produces zero actions
      it "returns zero actions when handler returns Integration.none" \_ -> do
        let result = handleEventImpl @TestHandlerA testEntity (ItemRemoved {removedItemId = 1})
        Integration.getActions result
          |> Array.length
          |> shouldBe 0

      -- Test 29: Handler returning Integration.batch [...] produces exactly one action
      it "returns exactly one action when handler returns single-item batch" \_ -> do
        let result = handleEventImpl @TestHandlerA testEntity (ItemAdded {addedItemId = 1, addedQuantity = 5})
        Integration.getActions result
          |> Array.length
          |> shouldBe 1

      -- Test 30: Handler returning Integration.none for all non-matching constructors
      it "returns zero actions for all non-matching event constructors" \_ -> do
        let resultB = handleEventImpl @TestHandlerA testEntity (ItemRemoved {removedItemId = 1})
        let resultC = handleEventImpl @TestHandlerA testEntity (EntityCreated {createdId = Uuid.nil, createdName = "test"})
        Integration.getActions resultB |> Array.length |> shouldBe 0
        Integration.getActions resultC |> Array.length |> shouldBe 0

    describe "type constraint verification" do

      -- Test 31: createTypedOutboundRunner compiles with all constraints satisfied
      it "createTypedOutboundRunner compiles with all constraints satisfied" \_ -> do
        let _ = createTypedOutboundRunner @TestHandlerA
        pass

      -- Test 32: HandledEvent ~ EventOf (EntityOf handler) constraint holds
      it "HandledEvent ~ EventOf (EntityOf handler) constraint holds" \_ -> do
        -- Verified by compilation of createTypedOutboundRunner @TestHandlerA.
        -- The constraint `event ~ HandledEvent handler` in createTypedOutboundRunner
        -- requires HandledEvent TestHandlerA ~ TestEvent ~ EventOf TestEntity.
        let _ = createTypedOutboundRunner @TestHandlerA
        pass

-- Test 33: INLINE pragma verified by code review of TH.hs
-- The TH macro includes:
--   TH.PragmaD (TH.InlineP (TH.mkName "handleEventImpl") TH.Inline TH.FunLike TH.AllPhases)
-- in the generated instance body. See Service.OutboundIntegration.TH.

-- ============================================================================
-- Compile-time error verification (uncomment to verify errors)
-- ============================================================================

-- TEST 12: Missing EntityOf
-- If uncommented, should produce:
--   ERROR: Missing EntityOf type instance for handler 'NoEntityHandler'.
--   Please add: `type instance EntityOf NoEntityHandler = YourEntityType`
--
-- data NoEntityHandler = NoEntityHandler deriving (Generic, Typeable, Show)
-- outboundIntegration ''NoEntityHandler

-- TEST 13: Missing EventOf for the entity
-- If uncommented, should produce:
--   ERROR: Could not resolve EventOf for entity 'OrphanEntity'.
--   Ensure your entity has: `type instance EventOf OrphanEntity = YourEventType`
--
-- data OrphanEntity = OrphanEntity deriving (Generic, Typeable, Show)
-- data OrphanHandler = OrphanHandler deriving (Generic, Typeable, Show)
-- type instance EntityOf OrphanHandler = OrphanEntity
-- -- (no handleEvent, no EventOf OrphanEntity)
-- outboundIntegration ''OrphanHandler

-- TEST 14: Missing handleEvent function
-- If uncommented, should produce:
--   ERROR: Missing 'handleEvent' function for handler 'NoHandleEventHandler'.
--   Please add: `handleEvent :: TestEntity -> TestEvent -> Integration.Outbound`
--
-- data NoHandleEventHandler = NoHandleEventHandler deriving (Generic, Typeable, Show)
-- type instance EntityOf NoHandleEventHandler = TestEntity
-- -- (no handleEvent function in scope)
-- outboundIntegration ''NoHandleEventHandler

-- TEST 15: Wrong handleEvent signature
-- If uncommented, should produce:
--   ERROR: 'handleEvent' has incorrect signature.
--   Expected: TestEntity -> TestEvent -> Outbound
--   Current:  Int -> Text -> Outbound
--
-- handleEvent :: Int -> Text -> Integration.Outbound
-- handleEvent _ _ = Integration.none
-- data WrongSigHandler = WrongSigHandler deriving (Generic, Typeable, Show)
-- type instance EntityOf WrongSigHandler = TestEntity
-- outboundIntegration ''WrongSigHandler
