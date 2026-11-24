module Service.Query.DefinitionSpec where

import Array qualified
import ConcurrentVar qualified
import Core hiding (event)
import Json qualified
import Service.AccessControl (AccessError, UserClaims)
import Service.AccessControl qualified as AccessControl
import Service.Application qualified as Application
import Service.Entity.Core (Event (..))
import Service.Event (EntityName (..), StreamId (..))
import Service.Event qualified as RawEvent
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamPosition (StreamPosition (..))
import Service.Event.TH (event)
import Service.EventStore.Core (EventStore (..), ReadStreamMessage (..))
import Service.EventStore.InMemory qualified as EventStoreInMemory
import Service.Query.Definition (QueryDefinition (..), WiredQueryEndpoint (..), createDefinitionWithStore)
import Service.Query.Registry qualified as Registry
import Service.QueryObjectStore.Core (QueryObjectStore (..))
import Service.QueryObjectStore.InMemory qualified as QOSInMemory
import Service.Query.Pagination (QueryPageRequest (..), QueryPageResponse (..))
import Service.Transport (EndpointSchema (..), QueryByIdEndpoint (..))
import Stream qualified
import Task qualified
import Test
import Uuid qualified


data CacheEntity = CacheEntity
  { cacheEntityId :: Uuid
  , cacheEntityValue :: Int
  }
  deriving (Generic)


instance Json.FromJSON CacheEntity


instance Json.ToJSON CacheEntity


type instance NameOf CacheEntity = "CacheEntity"


data CacheEvent = CacheEvent
  { cacheEventEntityId :: Uuid
  , cacheEventValue :: Int
  }


getCacheEventEntityId :: CacheEvent -> Uuid
getCacheEventEntityId cacheEvent = cacheEvent.cacheEventEntityId


type instance EventOf CacheEntity = CacheEvent


type instance EntityOf CacheEvent = CacheEntity


instance Entity CacheEntity where
  initialStateImpl = CacheEntity { cacheEntityId = Uuid.nil, cacheEntityValue = 0 }
  updateImpl cacheEvent _ =
    CacheEntity
      { cacheEntityId = cacheEvent.cacheEventEntityId
      , cacheEntityValue = cacheEvent.cacheEventValue
      }


instance Event CacheEvent where
  getEventEntityIdImpl = getCacheEventEntityId


event ''CacheEvent


-- | A one-entity query fixture, derived the canonical way.
--
-- It exercises both query-name threading and the automatic entity-fetcher
-- wiring. The marker owns the mechanical instances; the Entity and QueryOf
-- business behavior stay explicit.
data StoreWiringQuery = StoreWiringQuery
  { probeId :: Uuid
  , probe :: Int
  }


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = AccessControl.publicAccess


canView :: Maybe UserClaims -> StoreWiringQuery -> Maybe AccessError
canView = AccessControl.publicView


-- QueryOf has Query as a superclass, and Query is emitted by this splice.
-- GHC therefore requires the marker's declaration group before the business
-- instance; placing the marker last fails with "No instance for Query".
deriveQuery ''StoreWiringQuery [''CacheEntity]


instance QueryOf CacheEntity StoreWiringQuery where
  queryId cacheEntity = cacheEntity.cacheEntityId
  combine cacheEntity _ =
    Update
      StoreWiringQuery
        { probeId = cacheEntity.cacheEntityId
        , probe = cacheEntity.cacheEntityValue
        }


-- | Wire the fixture query through 'createDefinitionWithStore' with a spy store
-- factory that captures the name it is handed.
--
-- Since #734 / ADR-0071 'createDefinitionWithStore' hands the store factory the
-- query's real name (@NameOf query@), so the spy records @"StoreWiringQuery"@,
-- not the old @"__trait__"@ sentinel. The wiring test (C4) asserts exactly that;
-- only this helper flipped red->green — the test body did not.
wireWithSpy :: (Text -> Task Text (QueryObjectStore StoreWiringQuery)) -> QueryDefinition
wireWithSpy spy = createDefinitionWithStore @StoreWiringQuery spy


spec :: Spec Unit
spec = do
  describe "Application.withQueryOnly" do
    it "registers an internal projection with its entity updater and no published routes" \_ -> do
      let app = Application.new |> Application.withQueryOnly @StoreWiringQuery []
      case Array.toLinkedList app.queryDefinitions of
        [definition] -> do
          definition.queryRoutes |> shouldBe []
          store <- EventStoreInMemory.new |> Task.mapError toText
          (registry, _) <- definition.wireQuery store
          let updaters = Registry.getUpdatersForEntity (EntityName "CacheEntity") registry
          updaters |> Array.map (\updater -> updater.queryName) |> shouldBe ["StoreWiringQuery"]
        _ -> fail "Expected one registered projection"

  describe "createDefinitionWithStore" do
    it "passes NameOf query to the supplied store factory (#734)" \_ -> do
      -- The automatic wiring must hand the query's OWN name to the store factory
      -- (so each query's rows are keyed by it), not the shared "__trait__"
      -- sentinel. This pins the application wiring itself — the store-level
      -- PostgresSpec isolation tests only prove that manually named stores do
      -- not collide, which does not exercise how the name reaches the store.
      captured <- ConcurrentVar.containing ""
      let spyFactory queryName = do
            captured |> ConcurrentVar.modify (\_ -> queryName)
            QOSInMemory.new |> Task.mapError toText
      let definition = wireWithSpy spyFactory
      eventStore <- EventStoreInMemory.new |> Task.mapError toText
      definition.wireQuery eventStore
        |> Task.asResult
        |> discard
      capturedName <- captured |> ConcurrentVar.peek
      -- Pin to the query's own NameOf as an INDEPENDENT literal, not
      -- 'definition.queryName' (which the same wiring computes) — else a
      -- regression that corrupts the name identically in both places would slip
      -- past. The exposed field is asserted separately.
      let expectedName = "StoreWiringQuery"
      let exposedName = definition.queryName
      Task.unless (exposedName == expectedName) do
        fail [fmt|definition exposed queryName "#{exposedName}"; expected "#{expectedName}"|]
      Task.unless (capturedName == expectedName && capturedName != "__trait__" && capturedName != "") do
        fail [fmt|store factory received "#{capturedName}"; expected the query's own name "#{expectedName}" (not the "__trait__" sentinel)|]

    it "wires the collection and UUID handlers with one store and protected renderer" \_ -> do
      queryStore <- QOSInMemory.new @StoreWiringQuery |> Task.mapError toText
      queryId <- Uuid.generate
      let query = StoreWiringQuery {probeId = queryId, probe = 42}
      queryStore.atomicUpdate queryId (\_ -> Just query) |> Task.mapError toText
      storeFactoryCalls <- ConcurrentVar.containing (0 :: Int)
      rendererCalls <- ConcurrentVar.containing (0 :: Int)
      let storeFactory _ = do
            storeFactoryCalls |> ConcurrentVar.modify (\count -> count + 1)
            Task.yield queryStore
      baseEventStore <- EventStoreInMemory.new |> Task.mapError toText
      let eventStore = baseEventStore
            { renderProtectedJson = \value -> do
                rendererCalls |> ConcurrentVar.modify (\count -> count + 1)
                Task.yield value
            }
      let definition = createDefinitionWithStore @StoreWiringQuery storeFactory
      (_registry, wired) <- definition.wireQuery eventStore
      let pageRequest = QueryPageRequest {limit = 10, offset = 0}
      keyedText <- wired.wiredQueryById.byIdHandler Nothing queryId |> Task.mapError toText
      collectionText <- wired.wiredCollectionHandler Nothing Nothing pageRequest |> Task.mapError toText
      observedFactoryCalls <- storeFactoryCalls |> ConcurrentVar.peek
      observedRendererCalls <- rendererCalls |> ConcurrentVar.peek
      observedFactoryCalls |> shouldBe 1
      observedRendererCalls |> shouldBe 2
      wired.wiredQueryById.byIdSchema.responseSchema |> shouldBe definition.querySchema
      case Json.decodeText @StoreWiringQuery keyedText of
        Ok decoded -> do
          decoded.probeId |> shouldBe queryId
          decoded.probe |> shouldBe 42
        Err _ -> fail "Expected the UUID handler to return one query object"
      case Json.decodeText @(QueryPageResponse StoreWiringQuery) collectionText of
        Ok response -> do
          Array.length response.items |> shouldBe 1
          case Array.get 0 response.items of
            Just decoded -> decoded.probeId |> shouldBe queryId
            Nothing -> fail "Expected the collection handler to return the same stored query"
        Err _ -> fail "Expected the collection handler to return its page envelope"

    it "automatic query wiring reuses entity snapshots during replay" \_ -> do
      baseStore <- EventStoreInMemory.new |> Task.mapError toText
      metadata <- EventMetadata.new
      entityId <- Uuid.generate
      let entityName = EntityName "CacheEntity"
      let streamId = StreamId "cache-wiring-stream"
      let rawEvent localPosition value =
            RawEvent.Event
              { entityName
              , streamId
              , event = CacheEvent { cacheEventEntityId = entityId, cacheEventValue = value } |> Json.encode
              , metadata =
                  metadata
                    { localPosition = Just (StreamPosition localPosition)
                    , globalPosition = Just (StreamPosition localPosition)
                    }
              }
      let storedEvents = Array.fromLinkedList [rawEvent 0 10, rawEvent 1 20]
      readAllCalls <- ConcurrentVar.containing (0 :: Int)
      readStarts <- ConcurrentVar.containing (Array.empty :: Array StreamPosition)
      let spyStore = baseStore
            { readAllStreamEvents = \_ _ -> do
                readAllCalls |> ConcurrentVar.modify (\count -> count + 1)
                Stream.fromArray (storedEvents |> Array.map StreamEvent)
            , readStreamForwardFrom = \_ _ startPosition _ -> do
                readStarts |> ConcurrentVar.modify (Array.push startPosition)
                let page =
                      storedEvents
                        |> Array.takeIf (\stored -> stored.metadata.localPosition >= Just startPosition)
                        |> Array.map StreamEvent
                Stream.fromArray page
            }
      let definition = wireWithSpy (\_ -> QOSInMemory.new |> Task.mapError toText)
      (registry, _) <- definition.wireQuery spyStore
      updater <- case Registry.getUpdatersForEntity entityName registry |> Array.get 0 of
        Just found -> Task.yield found
        Nothing -> Task.throw "automatic cache fixture produced no updater"
      updater.updateQuery (rawEvent 0 10)
      updater.updateQuery (rawEvent 1 20)
      uncachedReads <- ConcurrentVar.peek readAllCalls
      starts <- ConcurrentVar.peek readStarts
      uncachedReads |> shouldBe 0
      starts |> shouldBe (Array.fromLinkedList [StreamPosition 0, StreamPosition 2])
