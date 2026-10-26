module Service.CommandExecutor.MetadataSpec (spec) where

import Array qualified
import AsyncTask qualified
import Auth.Claims (UserClaims (..))
import Basics
import ConcurrentVar qualified
import Control.Exception qualified as GhcException
import Decider qualified
import Log qualified
import Map qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Auth qualified as Auth
import Service.CommandExecutor.Core qualified as Executor
import Service.EntityFetcher.Core qualified as Fetcher
import Service.Event qualified as Event
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Stream qualified
import Task qualified
import Test
import Text (Text)
import Test.Service.Command.Core (AuthenticatedAddItem (..), initialCartState, applyCartEvent)
import Test.Service.EventStore.Core (CartEvent (..))
import ToText (toText)
import Uuid qualified


spec :: Spec Unit
spec = do
  it "does not log rejected decision reasons" \_ -> do
    let privateReason = "private-domain-reason-sentinel"
    let decisionContext =
          Decider.DecisionContext
            { Decider.genUuid = Uuid.generate
            }
    result <- Decider.runDecision decisionContext (Decider.reject privateReason)
    case result of
      Decider.RejectCommand reason -> reason |> shouldBe privateReason
      Decider.AcceptCommand _ _ -> fail "Expected the decision to be rejected"

  describe "persisted command metadata" do
    it "records the verified actor and server request correlation" \_ -> do
      store <- InMemory.new |> Task.map (EventStore.castEventStore @CartEvent) |> Task.mapError toText
      cartId <- Uuid.generate
      let entityName = EntityName "CartEntity"
      let streamId = StreamId.toStreamId cartId
      seed <- Event.payloadFromEvents entityName streamId [CartCreated cartId]
      store.insert seed |> Task.mapError toText |> Task.map (\_ -> unit)
      fetcher <- Fetcher.new store initialCartState applyCartEvent |> Task.mapError toText
      let claims = UserClaims
            { sub = "metadata-test-user"
            , email = Nothing
            , name = Nothing
            , permissions = []
            , tenantId = Nothing
            , rawClaims = Map.empty
            }
      context <- Auth.authenticatedContext claims
      let command = AuthenticatedAddItem {cartId, itemId = Uuid.nil, amount = 1}
      result <- Executor.execute store fetcher entityName Nothing context command
      case result of
        Executor.CommandAccepted {eventsAppended} -> eventsAppended |> shouldBe 1
        other -> fail [fmt|Expected accepted command, received #{toText other}|]
      messages <- store.readAllStreamEvents entityName streamId |> Task.mapError toText
      events <- messages |> Stream.toArray |> Task.map EventStore.collectStreamEvents
      Array.length events |> shouldBe 2
      case Array.last events of
        Nothing -> fail "Expected a saved command event"
        Just saved -> do
          saved.metadata.relatedUserSub |> shouldBe (Just "metadata-test-user")
          saved.metadata.correlationId |> shouldBe (Just (Uuid.toText context.requestId))
          saved.metadata.causationId |> shouldBe Nothing
      store.close |> Task.mapError toText

    it "does not treat source actor attribution as authenticated claims" \_ -> do
      store <- InMemory.new |> Task.map (EventStore.castEventStore @CartEvent) |> Task.mapError toText
      fetcher <- Fetcher.new store initialCartState applyCartEvent |> Task.mapError toText
      sourceMetadataBase <- EventMetadata.new
      let sourceMetadata = sourceMetadataBase {relatedUserSub = Just "historical-actor"}
      cartId <- Uuid.generate
      let command = AuthenticatedAddItem {cartId, itemId = Uuid.nil, amount = 1}
      result <- Executor.execute store fetcher (EntityName "CartEntity") (Just sourceMetadata) Auth.emptyContext command
      case result of
        Executor.CommandUnauthorized {} -> pass
        other -> fail [fmt|Expected an unauthenticated result, received #{toText other}|]
      store.close |> Task.mapError toText

    it "allocates a distinct correlation for each nil request ID" \_ -> do
      store <- InMemory.new |> Task.map (EventStore.castEventStore @CartEvent) |> Task.mapError toText
      cartId <- Uuid.generate
      let entityName = EntityName "CartEntity"
      let streamId = StreamId.toStreamId cartId
      seed <- Event.payloadFromEvents entityName streamId [CartCreated cartId]
      store.insert seed |> Task.mapError toText |> Task.map (\_ -> unit)
      fetcher <- Fetcher.new store initialCartState applyCartEvent |> Task.mapError toText
      let claims = UserClaims
            { sub = "nil-request-user"
            , email = Nothing
            , name = Nothing
            , permissions = []
            , tenantId = Nothing
            , rawClaims = Map.empty
            }
      let context = Auth.emptyContext {Auth.user = Just claims}
      let command = AuthenticatedAddItem {cartId, itemId = Uuid.nil, amount = 1}
      firstResult <- Executor.execute store fetcher entityName Nothing context command
      secondResult <- Executor.execute store fetcher entityName Nothing context command
      case (firstResult, secondResult) of
        (Executor.CommandAccepted {}, Executor.CommandAccepted {}) -> pass
        other -> fail [fmt|Expected two accepted commands, received #{toText other}|]
      messages <- store.readAllStreamEvents entityName streamId |> Task.mapError toText
      events <- messages |> Stream.toArray |> Task.map EventStore.collectStreamEvents
      Array.length events |> shouldBe 3
      case (Array.get 1 events, Array.get 2 events) of
        (Just first, Just second) -> do
          let firstCorrelation = first.metadata.correlationId
          let secondCorrelation = second.metadata.correlationId
          firstCorrelation |> shouldSatisfy (\value -> value != Nothing && value != Just (Uuid.toText Uuid.nil))
          secondCorrelation |> shouldSatisfy (\value -> value != Nothing && value != firstCorrelation)
          first.metadata.relatedUserSub |> shouldBe (Just "nil-request-user")
          second.metadata.relatedUserSub |> shouldBe (Just "nil-request-user")
        _ -> fail "Expected two saved command events"
      store.close |> Task.mapError toText

    it "does not report an event when append fails" \_ -> do
      store <- InMemory.new |> Task.map (EventStore.castEventStore @CartEvent) |> Task.mapError toText
      cartId <- Uuid.generate
      let entityName = EntityName "CartEntity"
      let streamId = StreamId.toStreamId cartId
      seed <- Event.payloadFromEvents entityName streamId [CartCreated cartId]
      store.insert seed |> Task.mapError toText |> Task.map (\_ -> unit)
      fetcher <- Fetcher.new store initialCartState applyCartEvent |> Task.mapError toText
      let claims = UserClaims
            { sub = "failure-user"
            , email = Nothing
            , name = Nothing
            , permissions = []
            , tenantId = Nothing
            , rawClaims = Map.empty
            }
      context <- Auth.authenticatedContext claims
      let command = AuthenticatedAddItem {cartId, itemId = Uuid.nil, amount = 1}
      let failure = EventStore.InsertionError (Event.InsertionFailed "private-store-error-sentinel")
      let failedStore = store {EventStore.insert = \_ -> Task.throw failure}
      result <- Executor.execute failedStore fetcher entityName Nothing context command
      case result of
        Executor.CommandFailed {} -> pass
        other -> fail [fmt|Expected append failure, received #{toText other}|]
      messages <- store.readAllStreamEvents entityName streamId |> Task.mapError toText
      events <- messages |> Stream.toArray |> Task.map EventStore.collectStreamEvents
      Array.length events |> shouldBe 1
      store.close |> Task.mapError toText

    it "stops after bounded append conflicts without saving a candidate" \_ -> do
      store <- InMemory.new |> Task.map (EventStore.castEventStore @CartEvent) |> Task.mapError toText
      cartId <- Uuid.generate
      let entityName = EntityName "CartEntity"
      let streamId = StreamId.toStreamId cartId
      seed <- Event.payloadFromEvents entityName streamId [CartCreated cartId]
      store.insert seed |> Task.mapError toText |> Task.map (\_ -> unit)
      fetcher <- Fetcher.new store initialCartState applyCartEvent |> Task.mapError toText
      let claims = UserClaims
            { sub = "conflict-user"
            , email = Nothing
            , name = Nothing
            , permissions = []
            , tenantId = Nothing
            , rawClaims = Map.empty
            }
      context <- Auth.authenticatedContext claims
      let command = AuthenticatedAddItem {cartId, itemId = Uuid.nil, amount = 1}
      let conflict = EventStore.InsertionError Event.ConsistencyCheckFailed
      let conflictingStore = store {EventStore.insert = \_ -> Task.throw conflict}
      result <- Executor.execute conflictingStore fetcher entityName Nothing context command
      case result of
        Executor.CommandFailed {retriesAttempted} -> retriesAttempted |> shouldBe 10
        other -> fail [fmt|Expected bounded retry failure, received #{toText other}|]
      messages <- store.readAllStreamEvents entityName streamId |> Task.mapError toText
      events <- messages |> Stream.toArray |> Task.map EventStore.collectStreamEvents
      Array.length events |> shouldBe 1
      store.close |> Task.mapError toText

    it "reports synchronous IO failures without scoped metadata or exception text" \_ -> do
      store <- InMemory.new |> Task.map (EventStore.castEventStore @CartEvent) |> Task.mapError toText
      fetcher <- Fetcher.new store initialCartState applyCartEvent |> Task.mapError toText
      context <- Auth.trustedContext
      cartId <- Uuid.generate
      let entityName = EntityName "CartEntity"
      let command = AuthenticatedAddItem {cartId, itemId = Uuid.nil, amount = 1}
      let throwingFetcher =
            fetcher
              { Fetcher.fetch = \_ _ ->
                  Task.fromIO (GhcException.throwIO (GhcException.ErrorCall "private-io-exception-sentinel"))
              }
      Log.withScope
        [ ("eventId", "ambient-event-id-sentinel")
        , ("streamId", "ambient-stream-id-sentinel")
        ] do
        result <- Executor.execute store throwingFetcher entityName Nothing context command |> Task.asResult
        case result of
          Err _ -> pass
          Ok executionResult -> fail [fmt|Expected the IO exception to fail the command, received #{toText executionResult}|]
      store.close |> Task.mapError toText

    it "reports an interrupted command attempt when cancelled" \_ -> do
      store <- InMemory.new |> Task.map (EventStore.castEventStore @CartEvent) |> Task.mapError toText
      fetcher <- Fetcher.new store initialCartState applyCartEvent |> Task.mapError toText
      context <- Auth.trustedContext
      cartId <- Uuid.generate
      let entityName = EntityName "CartEntity"
      let command = AuthenticatedAddItem {cartId, itemId = Uuid.nil, amount = 1}
      fetchStarted <- ConcurrentVar.containing False
      let blockingFetcher =
            fetcher
              { Fetcher.fetch = \_ _ -> do
                  fetchStarted |> ConcurrentVar.modify (\_ -> True)
                  AsyncTask.sleep 100000
                  Task.yield Fetcher.EntityNotFound
              }
      commandTask <- AsyncTask.run (Executor.execute store blockingFetcher entityName Nothing context command) |> Task.mapError toText
      AsyncTask.sleep 50 |> Task.mapError (\_ -> "sleep error" :: Text)
      fetchDidStart <- ConcurrentVar.peek fetchStarted
      fetchDidStart |> shouldBe True
      AsyncTask.cancel commandTask
      result <- AsyncTask.waitCatch commandTask
      case result of
        Err _ -> pass
        Ok executionResult -> fail [fmt|Expected cancellation, received #{toText executionResult}|]
      store.close |> Task.mapError toText
