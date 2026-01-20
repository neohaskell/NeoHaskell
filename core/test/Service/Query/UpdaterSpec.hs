module Service.Query.UpdaterSpec where

import Array qualified
import Core
import Json qualified
import Service.EntityFetcher.Core (EntityFetcher (..))
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event (Event (..), Insertion (..), InsertionPayload (..))
import Service.Event.EntityName (EntityName (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore (..))
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Service.Query.Registry (QueryUpdater (..))
import Service.Query.Updater qualified as Updater
import Service.QueryObjectStore.Core (QueryObjectStore (..))
import Service.QueryObjectStore.InMemory qualified as QueryObjectStore.InMemory
import Task qualified
import Test
import Uuid qualified


-- Test Entity: Counter
data CounterEntity = CounterEntity
  { counterId :: Uuid,
    count :: Int
  }
  deriving (Eq, Show, Generic)


instance Entity CounterEntity where
  initialStateImpl = CounterEntity {counterId = Uuid.nil, count = 0}
  updateImpl event entity = case event of
    CounterCreated cid -> entity {counterId = cid, count = 0}
    CounterIncremented -> entity {count = entity.count + 1}
    CounterDecremented -> entity {count = entity.count - 1}


-- Test Events
data CounterEvent
  = CounterCreated Uuid
  | CounterIncremented
  | CounterDecremented
  deriving (Eq, Show, Generic)


instance Json.ToJSON CounterEvent
instance Json.FromJSON CounterEvent


type instance EventOf CounterEntity = CounterEvent


-- Test Query: CounterSummary
data CounterSummary = CounterSummary
  { summaryId :: Uuid,
    totalCount :: Int
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON CounterSummary
instance Json.FromJSON CounterSummary


instance Query CounterSummary where
  canAccessImpl _ = Nothing
  canViewImpl _ _ = Nothing


instance QueryOf CounterEntity CounterSummary where
  queryId entity = entity.counterId
  combine entity _maybeExisting =
    Update
      CounterSummary
        { summaryId = entity.counterId,
          totalCount = entity.count
        }


spec :: Spec Unit
spec = do
  describe "Query.Updater" do
    describe "createUpdater" do
      it "updates query when entity is found" \_ -> do
        -- Setup: Create event store, query store, and entity fetcher
        eventStore <- InMemory.new |> Task.mapError toText
        queryStore <- QueryObjectStore.InMemory.new |> Task.mapError toText

        -- Insert a counter creation event
        counterId <- Uuid.generate
        streamId <- StreamId.new
        let entityName = EntityName "Counter"

        insertCounterEvent eventStore entityName streamId (CounterCreated counterId)
        insertCounterEvent eventStore entityName streamId CounterIncremented
        insertCounterEvent eventStore entityName streamId CounterIncremented

        -- Create entity fetcher
        let castedStore = EventStore.castEventStore @CounterEvent eventStore
        entityFetcher <- createCounterFetcher castedStore

        -- Create updater
        let updater = Updater.createUpdater @CounterEntity @CounterSummary "CounterSummary" entityFetcher queryStore

        -- Build a fake raw event to trigger the updater
        rawEvent <- createRawEvent entityName streamId

        -- Call the updater
        updater.updateQuery rawEvent

        -- Verify the query was updated
        maybeQuery <- queryStore.get counterId |> Task.mapError toText
        case maybeQuery of
          Just query -> do
            query.summaryId |> shouldBe counterId
            query.totalCount |> shouldBe 2
          Nothing -> do
            fail "Expected query to be created"

      it "does nothing when entity is not found" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        queryStore <- QueryObjectStore.InMemory.new |> Task.mapError toText

        -- Create entity fetcher (but don't insert any events)
        let castedStore = EventStore.castEventStore @CounterEvent eventStore
        entityFetcher <- createCounterFetcher castedStore

        let updater = Updater.createUpdater @CounterEntity @CounterSummary "CounterSummary" entityFetcher queryStore

        -- Create raw event for a non-existent entity
        streamId <- StreamId.new
        let entityName = EntityName "Counter"
        rawEvent <- createRawEvent entityName streamId

        -- Call the updater
        updater.updateQuery rawEvent

        -- Verify no query was created
        allQueries <- queryStore.getAll |> Task.mapError toText
        Array.length allQueries |> shouldBe 0

      it "updates existing query with new entity state" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        queryStore <- QueryObjectStore.InMemory.new |> Task.mapError toText

        counterId <- Uuid.generate
        streamId <- StreamId.new
        let entityName = EntityName "Counter"

        -- Insert initial events
        insertCounterEvent eventStore entityName streamId (CounterCreated counterId)
        insertCounterEvent eventStore entityName streamId CounterIncremented

        let castedStore = EventStore.castEventStore @CounterEvent eventStore
        entityFetcher <- createCounterFetcher castedStore

        let updater = Updater.createUpdater @CounterEntity @CounterSummary "CounterSummary" entityFetcher queryStore

        -- First update
        rawEvent1 <- createRawEvent entityName streamId
        updater.updateQuery rawEvent1

        -- Insert more events
        insertCounterEvent eventStore entityName streamId CounterIncremented
        insertCounterEvent eventStore entityName streamId CounterIncremented

        -- Second update
        rawEvent2 <- createRawEvent entityName streamId
        updater.updateQuery rawEvent2

        -- Verify the query was updated with the new count
        maybeQuery <- queryStore.get counterId |> Task.mapError toText
        case maybeQuery of
          Just query -> query.totalCount |> shouldBe 3
          Nothing -> fail "Expected query to exist"


-- Helper to create an entity fetcher for Counter
createCounterFetcher :: EventStore CounterEvent -> Task Text (EntityFetcher CounterEntity CounterEvent)
createCounterFetcher eventStore = do
  EntityFetcher.new eventStore initialStateImpl updateImpl
    |> Task.mapError toText


-- Helper to insert a counter event
insertCounterEvent :: EventStore Json.Value -> EntityName -> StreamId -> CounterEvent -> Task Text Unit
insertCounterEvent eventStore entityName streamId event = do
  eventId <- Uuid.generate
  metadata <- EventMetadata.new

  let insertion =
        Insertion
          { id = eventId,
            event = Json.encode event,
            metadata = metadata
          }

  let payload =
        InsertionPayload
          { streamId = streamId,
            entityName = entityName,
            insertionType = AnyStreamState,
            insertions = [insertion]
          }

  eventStore.insert payload
    |> Task.mapError toText
    |> discard


-- Helper to create a raw event for triggering the updater
createRawEvent :: EntityName -> StreamId -> Task Text (Event Json.Value)
createRawEvent entityName streamId = do
  metadata <- EventMetadata.new
  Task.yield
    Event
      { entityName = entityName,
        streamId = streamId,
        event = Json.encode (),
        metadata = metadata
      }
