module Service.CommandExecutor.StorageMetadataSpec (spec) where

import Array (Array)
import Array qualified
import AsyncTask qualified
import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Directory qualified
import Maybe (Maybe (..))
import Maybe qualified
import Service.Auth qualified as Auth
import Service.CommandExecutor.CommandExecutionFailure qualified as CommandExecutionFailure
import Service.CommandExecutor.Core qualified as Executor
import Service.EntityFetcher.Core qualified as Fetcher
import Service.Event (Event)
import Service.Event qualified as Event
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core (EventStore)
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.Postgres qualified as Postgres
import Service.EventStore.Postgres.Internal (PostgresEventStore (..))
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.EventStore.Simple qualified as Simple
import Service.Infra.Postgres.ConnectionConfig qualified as ConnectionConfig
import Stream qualified
import Task (Task)
import Task qualified
import Test
import Test.Service.Command.Core (AddItemToCart (..), applyCartEvent, initialCartState)
import Test.Service.EventStore.Core (CartEvent (..))
import ToText (toText)
import Text (Text)
import Uuid qualified


spec :: Spec Unit
spec = do
  describe "metadata through production command execution and storage" do
    it "persists command metadata through a subscribed Simple store restart" \_ -> do
      runId <- Uuid.generate
      Directory.withTemporaryFolder toText \basePath -> do
        let config = SimpleEventStore {basePath, persistent = True}
        rawStore <- Simple.new config
        let store = EventStore.castEventStore @CartEvent rawStore
        let cleanup = store.close |> Task.ignoreError
        Task.finally cleanup do
          cartId <- Uuid.generate
          let entityName = EntityName [fmt|StorageMetadataSimple#{Uuid.toText runId}|]
          let streamId = StreamId.toStreamId cartId
          seedPayload <- Event.payloadFromEvents entityName streamId [CartCreated cartId]
          store.insert seedPayload |> Task.mapError toText |> discard
          eventsBeforeCommand <- readStreamEvents store entityName streamId
          Array.length eventsBeforeCommand |> shouldBe 1
          let seedEvent = Array.first eventsBeforeCommand |> Maybe.getOrDie

          sourceMetadata <- sourceMetadataForCommand
          observed <- ConcurrentVar.containing (Array.empty :: Array (Event CartEvent))
          subscription <-
            store.subscribeToStreamEvents entityName streamId (recordEvent observed)
              |> Task.mapError toText
          let unsubscribe = store.unsubscribe subscription |> Task.mapError toText |> Task.ignoreError
          Task.finally unsubscribe do
            result <- executeAddItem store entityName cartId sourceMetadata
            assertAccepted result
            storedEvents <- readStreamEvents store entityName streamId
            Array.length storedEvents |> shouldBe 2
            let savedEvent = Array.get 1 storedEvents |> Maybe.getOrDie
            assertCommandEvent seedEvent sourceMetadata cartId savedEvent
            globalEvents <- readGlobalEvents store entityName
            assertGlobalCommandEvent savedEvent globalEvents
            seedEvent.metadata.relatedUserSub |> shouldBe Nothing
            seedEvent.metadata.correlationId |> shouldBe Nothing
            seedEvent.metadata.causationId |> shouldBe Nothing
            savedEvent.metadata.globalPosition |> shouldBe (Just (Event.StreamPosition 1))

            notifications <- ConcurrentVar.peek observed
            Array.length notifications |> shouldBe 1
            let notifiedEvent = Array.first notifications |> Maybe.getOrDie
            notifiedEvent |> shouldBe savedEvent

            store.close |> Task.mapError toText
            restartedRawStore <- Simple.new config
            let restartedStore = EventStore.castEventStore @CartEvent restartedRawStore
            Task.finally (restartedStore.close |> Task.ignoreError) do
              eventsAfterRestart <- readStreamEvents restartedStore entityName streamId
              Array.length eventsAfterRestart |> shouldBe 2
              let restartedSeed = Array.first eventsAfterRestart |> Maybe.getOrDie
              let restartedCommand = Array.get 1 eventsAfterRestart |> Maybe.getOrDie
              restartedSeed.metadata |> shouldBe seedEvent.metadata
              restartedCommand.metadata |> shouldBe savedEvent.metadata
              globalEventsAfterRestart <- readGlobalEvents restartedStore entityName
              assertGlobalCommandEvent savedEvent globalEventsAfterRestart

    whenEnvVar "POSTGRES_AVAILABLE" do
      it "persists command metadata through typed PostgreSQL writes, subscription, and reopen" \_ -> do
        let config =
              PostgresEventStore
                { host = "localhost",
                  databaseName = "neohaskell",
                  user = "neohaskell",
                  password = "neohaskell",
                  port = 5432,
                  poolSize = 3,
                  sslMode = ConnectionConfig.SslModeUnset,
                  sslRootCert = Nothing
                }
        runId <- Uuid.generate
        cartId <- Uuid.generate
        let entityName = EntityName [fmt|StorageMetadataPostgres#{Uuid.toText runId}|]
        let streamId = StreamId.toStreamId cartId
        rawStore <- Postgres.new config |> Task.mapError toText
        let store = EventStore.castEventStore @CartEvent rawStore
        (seedEvent, sourceMetadata, savedEvent) <- Task.finally (store.close |> Task.ignoreError) do
          seedPayload <- Event.payloadFromEvents entityName streamId [CartCreated cartId]
          store.insert seedPayload |> Task.mapError toText |> discard
          eventsBeforeCommand <- readStreamEvents store entityName streamId
          Array.length eventsBeforeCommand |> shouldBe 1
          let seedEvent = Array.first eventsBeforeCommand |> Maybe.getOrDie

          sourceMetadata <- sourceMetadataForCommand
          observed <- ConcurrentVar.containing (Array.empty :: Array (Event CartEvent))
          subscription <-
            store.subscribeToStreamEvents entityName streamId (recordEvent observed)
              |> Task.mapError toText
          let unsubscribe = store.unsubscribe subscription |> Task.mapError toText |> Task.ignoreError
          Task.finally unsubscribe do
            result <- executeAddItem store entityName cartId sourceMetadata
            assertAccepted result
            waitForEventCount 1 60 observed
            storedEvents <- readStreamEvents store entityName streamId
            Array.length storedEvents |> shouldBe 2
            let savedEvent = Array.get 1 storedEvents |> Maybe.getOrDie
            assertCommandEvent seedEvent sourceMetadata cartId savedEvent
            globalEvents <- readGlobalEvents store entityName
            assertGlobalCommandEvent savedEvent globalEvents
            assertGlobalPositionAfter seedEvent savedEvent

            notifications <- ConcurrentVar.peek observed
            Array.length notifications |> shouldBe 1
            let notifiedEvent = Array.first notifications |> Maybe.getOrDie
            notifiedEvent |> shouldBe savedEvent
            Task.yield (seedEvent, sourceMetadata, savedEvent)

        reopenedRawStore <- Postgres.new config |> Task.mapError toText
        let reopenedStore = EventStore.castEventStore @CartEvent reopenedRawStore
        Task.finally (reopenedStore.close |> Task.ignoreError) do
          eventsAfterReopen <- readStreamEvents reopenedStore entityName streamId
          Array.length eventsAfterReopen |> shouldBe 2
          let reopenedSeed = Array.first eventsAfterReopen |> Maybe.getOrDie
          let reopenedCommand = Array.get 1 eventsAfterReopen |> Maybe.getOrDie
          reopenedSeed.metadata |> shouldBe seedEvent.metadata
          reopenedCommand.metadata |> shouldBe savedEvent.metadata
          assertCommandEvent seedEvent sourceMetadata cartId reopenedCommand
          globalEventsAfterReopen <- readGlobalEvents reopenedStore entityName
          assertGlobalCommandEvent savedEvent globalEventsAfterReopen


sourceMetadataForCommand :: Task Text EventMetadata
sourceMetadataForCommand = do
  metadata <- EventMetadata.new
  Task.yield
    metadata
      { relatedUserSub = Just "callback-user",
        correlationId = Just "root-correlation"
      }


executeAddItem :: EventStore CartEvent -> EntityName -> Uuid.Uuid -> EventMetadata -> Task Text Executor.ExecutionResult
executeAddItem eventStore entityName cartId sourceMetadata = do
  fetcher <- Fetcher.new eventStore initialCartState applyCartEvent |> Task.mapError toText
  Executor.execute
    (Executor.ExecutionDependencies
      { Executor.eventStore = eventStore,
        Executor.entityFetcher = fetcher,
        Executor.entityName = entityName
      })
    (Executor.FromEvent Auth.emptyContext sourceMetadata)
    AddItemToCart {cartId, itemId = Uuid.nil, amount = 1}
    |> Task.mapError CommandExecutionFailure.message


assertAccepted :: Executor.ExecutionResult -> Task Text Unit
assertAccepted result = case result of
  Executor.CommandAccepted {eventsAppended} -> eventsAppended |> shouldBe 1
  other -> fail [fmt|Expected one accepted command event, received #{toText other}|]


assertCommandEvent :: Event CartEvent -> EventMetadata -> Uuid.Uuid -> Event CartEvent -> Task Text Unit
assertCommandEvent seedEvent sourceMetadata cartId savedEvent = do
  savedEvent.event |> shouldBe (ItemAdded {entityId = cartId, itemId = Uuid.nil, amount = 1})
  savedEvent.metadata.eventId |> shouldSatisfy (\eventId -> eventId != seedEvent.metadata.eventId)
  savedEvent.metadata.eventId |> shouldSatisfy (\eventId -> eventId != sourceMetadata.eventId)
  savedEvent.metadata.relatedUserSub |> shouldBe (Just "callback-user")
  savedEvent.metadata.correlationId |> shouldBe (Just "root-correlation")
  savedEvent.metadata.causationId |> shouldBe (Just (Uuid.toText sourceMetadata.eventId))
  savedEvent.metadata.localPosition |> shouldBe (Just (Event.StreamPosition 1))
  savedEvent.metadata.globalPosition |> shouldSatisfy (\position -> position != Nothing)


assertGlobalPositionAfter :: Event CartEvent -> Event CartEvent -> Task Text Unit
assertGlobalPositionAfter earlierEvent laterEvent =
  case (earlierEvent.metadata.globalPosition, laterEvent.metadata.globalPosition) of
    (Just earlierPosition, Just laterPosition) -> laterPosition |> shouldSatisfy (\position -> position > earlierPosition)
    _ -> fail "Expected both PostgreSQL events to have global positions"


assertGlobalCommandEvent :: Event CartEvent -> Array (Event CartEvent) -> Task Text Unit
assertGlobalCommandEvent savedEvent globalEvents = do
  Array.length globalEvents |> shouldBe 2
  let globalCommand = Array.get 1 globalEvents |> Maybe.getOrDie
  globalCommand.metadata.eventId |> shouldBe savedEvent.metadata.eventId
  globalCommand.metadata.relatedUserSub |> shouldBe savedEvent.metadata.relatedUserSub
  globalCommand.metadata.correlationId |> shouldBe savedEvent.metadata.correlationId
  globalCommand.metadata.causationId |> shouldBe savedEvent.metadata.causationId
  globalCommand.metadata.globalPosition |> shouldBe savedEvent.metadata.globalPosition


readGlobalEvents :: EventStore CartEvent -> EntityName -> Task Text (Array (Event CartEvent))
readGlobalEvents eventStore entityName = do
  stream <-
    eventStore.readAllEventsForwardFromFiltered (Event.StreamPosition 0) (EventStore.Limit 10) [entityName]
      |> Task.mapError toText
  messages <- Stream.toArray stream
  Task.yield (EventStore.collectAllEvents messages)


readStreamEvents :: EventStore CartEvent -> EntityName -> StreamId.StreamId -> Task Text (Array (Event CartEvent))
readStreamEvents eventStore entityName streamId = do
  stream <- eventStore.readAllStreamEvents entityName streamId |> Task.mapError toText
  messages <- Stream.toArray stream
  Task.yield (EventStore.collectStreamEvents messages)


recordEvent :: ConcurrentVar (Array (Event CartEvent)) -> Event CartEvent -> Task Text Unit
recordEvent observed event =
  observed |> ConcurrentVar.modify (Array.push event)


waitForEventCount :: Int -> Int -> ConcurrentVar (Array (Event CartEvent)) -> Task Text Unit
waitForEventCount expected attemptsLeft observed = do
  events <- ConcurrentVar.peek observed
  if Array.length events >= expected
    then Task.yield unit
    else
      if attemptsLeft <= 0
        then Task.throw "Timed out waiting for PostgreSQL event subscription"
        else do
          AsyncTask.sleep 50 |> Task.mapError (\_ -> "PostgreSQL event subscription wait failed")
          waitForEventCount expected (attemptsLeft - 1) observed
