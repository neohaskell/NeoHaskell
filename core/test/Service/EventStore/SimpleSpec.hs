module Service.EventStore.SimpleSpec where

import Core
import Array qualified
import Decider (Decision (..), DecisionContext (..))
import Decider qualified
import Erasable qualified
import Json qualified
import Maybe qualified
import "nhcore" Path qualified
import Service.EventStore.Core qualified as EventStoreCore
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.EventStore.Simple qualified as Simple
import Service.Event qualified as Event
import Result qualified
import Service.SnapshotCache.InMemory qualified as SnapshotCacheInMemory
import Task qualified
import Test
import Test.Service.EntityFetcher qualified as EntityFetcher
import Test.Service.EntityFetcher.Core qualified as EntityFetcherCore
import Test.Service.EventStore qualified as EventStore
import Uuid qualified


spec :: Spec Unit
spec = do
  describe "SimpleEventStore (non-persistent)" do
    let config =
          SimpleEventStore
            { basePath = Path.fromText ".neo/test-events" |> Maybe.getOrDie,
              persistent = False
            }
    let newStore = Simple.new config |> Task.map (EventStoreCore.castEventStore @EntityFetcherCore.CartEvent) |> Task.mapError toText
    EventStore.spec "simple" newStore

    it "rejects a protected append without storing a plaintext fallback" \_ -> do
      store <- Simple.new config |> Task.mapError toText
      let decision = do
            protected <- Erasable.new ("private" :: Text)
            Accept Event.StreamCreation [protected]
      (outcome, keys) <- Decider.runDecisionWithKeys (DecisionContext {genUuid = Uuid.generate}) (\_ -> Task.throw "No stored key") decision
      protected <- case outcome of
        RejectCommand _ -> Task.throw "Expected protected value"
        AcceptCommand _ values ->
          case Array.first values of
            Nothing -> Task.throw "Expected protected value"
            Just value -> Task.yield value
      streamId <- Uuid.generate
      payload <- Event.payloadFromEvents (Event.EntityName "protected-simple-fixture") (Event.StreamId (Uuid.toText streamId)) [Json.toJSON protected]
      result <- store.insertWithErasableKeys keys (payload {Event.insertionType = Event.StreamCreation}) |> Task.asResult
      result |> shouldSatisfy Result.isErr
      store.close

    let newStoreAndFetcher = do
          store <- newStore
          fetcher <- EntityFetcherCore.newFetcher store |> Task.mapError toText
          Task.yield (store, fetcher)

    EntityFetcher.spec newStoreAndFetcher

    let newStoreAndFetcherAndCache = do
          store <- newStore
          cache <- SnapshotCacheInMemory.new |> Task.mapError toText
          fetcher <- EntityFetcherCore.newFetcherWithCache store cache |> Task.mapError toText
          Task.yield (store, fetcher, cache)

    EntityFetcher.specWithCache newStoreAndFetcherAndCache
