module Test.Service.EntityFetcher.FetchWithCache.Spec where

import Array qualified
import Core
import Service.EntityFetcher.Core (EntityFetcher (..), EntityFetchResult (..), FetchedEntity (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore (..))
import Service.SnapshotCache.Core (SnapshotCache (..))
import Service.SnapshotCache.Snapshot (Snapshot (..), SnapshotKey (..))
import Task qualified
import Test
import Test.Service.EntityFetcher.Core (CartEvent (..), CartState (..))
import Test.Service.EntityFetcher.FetchWithCache.Context qualified as Context
import Uuid qualified


spec ::
  Task Text (EventStore CartEvent, EntityFetcher CartState CartEvent, SnapshotCache CartState) ->
  Spec Unit
spec newStoreAndFetcherAndCache = do
  describe "Entity Fetch With Cache" do
    describe "Cache Integration" do
      before (Context.initialize newStoreAndFetcherAndCache) do
        it "fetches correctly with empty cache" \context -> do
          eventId <- Uuid.generate
          metadata <- EventMetadata.new
          let metadata' =
                metadata
                  { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                    EventMetadata.eventId = eventId
                  }
          let insertion =
                Event.Insertion
                  { id = eventId,
                    event = CartCreated {entityId = def},
                    metadata = metadata'
                  }
          let payload =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.StreamCreation,
                    insertions = [insertion]
                  }

          payload
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          result <-
            context.fetcher.fetch context.entityName context.streamId
              |> Task.mapError toText

          case result of
            EntityNotFound -> fail "Expected EntityFound but got EntityNotFound"
            EntityFound fetched -> do
              fetched.state.version |> shouldBe 1

        it "updates cache after fetch" \context -> do
          eventId <- Uuid.generate
          metadata <- EventMetadata.new
          let metadata' =
                metadata
                  { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                    EventMetadata.eventId = eventId
                  }
          let insertion =
                Event.Insertion
                  { id = eventId,
                    event = CartCreated {entityId = def},
                    metadata = metadata'
                  }
          let payload =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.StreamCreation,
                    insertions = [insertion]
                  }

          payload
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          _ <-
            context.fetcher.fetch context.entityName context.streamId
              |> Task.mapError toText

          cachedSnapshot <-
            context.cache.get context.entityName context.streamId
              |> Task.mapError toText

          case cachedSnapshot of
            Nothing -> fail "Expected cache to be populated after fetch"
            Just snapshot -> do
              snapshot.state.version |> shouldBe 1
              snapshot.position |> shouldBe (Event.StreamPosition 0)

        it "uses cached snapshot for subsequent fetches" \context -> do
          let testState =
                CartState
                  { cartId = Nothing,
                    cartItems = Array.empty,
                    isCheckedOut = False,
                    version = 50
                  }
          let snapshot =
                Snapshot
                  { key = SnapshotKey context.entityName context.streamId,
                    state = testState,
                    position = Event.StreamPosition 49
                  }

          context.cache.set snapshot
            |> Task.mapError toText
            |> discard

          result <-
            context.fetcher.fetch context.entityName context.streamId
              |> Task.mapError toText

          case result of
            EntityNotFound -> fail "Expected EntityFound but got EntityNotFound with cached snapshot"
            EntityFound fetched -> do
              fetched.state.version |> shouldBe 50

        it "applies new events on top of cached snapshot" \context -> do
          let cachedState =
                CartState
                  { cartId = Just def,
                    cartItems = Array.empty,
                    isCheckedOut = False,
                    version = 1
                  }
          let snapshot =
                Snapshot
                  { key = SnapshotKey context.entityName context.streamId,
                    state = cachedState,
                    position = Event.StreamPosition 0
                  }

          context.cache.set snapshot
            |> Task.mapError toText
            |> discard

          eventId <- Uuid.generate
          metadata <- EventMetadata.new
          let metadata' =
                metadata
                  { EventMetadata.localPosition = Just (Event.StreamPosition 1),
                    EventMetadata.eventId = eventId
                  }
          let insertion =
                Event.Insertion
                  { id = eventId,
                    event = ItemAdded {entityId = def, itemId = def, amount = 100},
                    metadata = metadata'
                  }
          let payload =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.AnyStreamState,
                    insertions = [insertion]
                  }

          payload
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          result <-
            context.fetcher.fetch context.entityName context.streamId
              |> Task.mapError toText

          case result of
            EntityNotFound -> fail "Expected EntityFound"
            EntityFound fetched -> do
              fetched.state.version |> shouldBe 2
              Array.length fetched.state.cartItems |> shouldBe 1

        it "updates cache with new position after applying events" \context -> do
          let cachedState =
                CartState
                  { cartId = Just def,
                    cartItems = Array.empty,
                    isCheckedOut = False,
                    version = 1
                  }
          let snapshot =
                Snapshot
                  { key = SnapshotKey context.entityName context.streamId,
                    state = cachedState,
                    position = Event.StreamPosition 0
                  }

          context.cache.set snapshot
            |> Task.mapError toText
            |> discard

          insertions <-
            Array.initialize 5 identity
              |> Array.map
                ( \index -> do
                    eventId <- Uuid.generate
                    metadata <- EventMetadata.new
                    let metadata' =
                          metadata
                            { EventMetadata.localPosition = Just (Event.StreamPosition (fromIntegral (index + 1))),
                              EventMetadata.eventId = eventId
                            }
                    Task.yield
                      Event.Insertion
                        { id = eventId,
                          event = ItemAdded {entityId = def, itemId = def, amount = 10},
                          metadata = metadata'
                        }
                )
              |> Task.mapArray identity

          let payload =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.AnyStreamState,
                    insertions = insertions
                  }

          payload
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          _ <-
            context.fetcher.fetch context.entityName context.streamId
              |> Task.mapError toText

          newCachedSnapshot <-
            context.cache.get context.entityName context.streamId
              |> Task.mapError toText

          case newCachedSnapshot of
            Nothing -> fail "Expected cache to be updated"
            Just newSnapshot -> do
              newSnapshot.state.version |> shouldBe 6
              newSnapshot.position |> shouldBe (Event.StreamPosition 5)

    describe "Multiple Entities" do
      before (Context.initialize newStoreAndFetcherAndCache) do
        it "caches different entities independently" \context -> do
          streamId2 <- StreamId.new

          eventId1 <- Uuid.generate
          metadata1 <- EventMetadata.new
          let metadata1' =
                metadata1
                  { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                    EventMetadata.eventId = eventId1
                  }
          let insertion1 =
                Event.Insertion
                  { id = eventId1,
                    event = CartCreated {entityId = def},
                    metadata = metadata1'
                  }
          let payload1 =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.StreamCreation,
                    insertions = [insertion1]
                  }

          payload1
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          eventId2 <- Uuid.generate
          metadata2 <- EventMetadata.new
          let metadata2' =
                metadata2
                  { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                    EventMetadata.eventId = eventId2
                  }
          let insertion2 =
                Event.Insertion
                  { id = eventId2,
                    event = CartCreated {entityId = def},
                    metadata = metadata2'
                  }
          let payload2 =
                Event.InsertionPayload
                  { streamId = streamId2,
                    entityName = context.entityName,
                    insertionType = Event.StreamCreation,
                    insertions = [insertion2]
                  }

          payload2
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          _ <-
            context.fetcher.fetch context.entityName context.streamId
              |> Task.mapError toText

          _ <-
            context.fetcher.fetch context.entityName streamId2
              |> Task.mapError toText

          cache1 <-
            context.cache.get context.entityName context.streamId
              |> Task.mapError toText

          cache2 <-
            context.cache.get context.entityName streamId2
              |> Task.mapError toText

          case (cache1, cache2) of
            (Just s1, Just s2) -> do
              s1.state.version |> shouldBe 1
              s2.state.version |> shouldBe 1
            _ -> fail "Expected both caches to be populated"

    describe "Cache Performance" do
      before (Context.initialize newStoreAndFetcherAndCache) do
        it "efficiently handles entity with many events using cache" \context -> do
          openEventId <- Uuid.generate
          openMetadata <- EventMetadata.new
          let openMetadata' =
                openMetadata
                  { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                    EventMetadata.eventId = openEventId
                  }
          let openInsertion =
                Event.Insertion
                  { id = openEventId,
                    event = CartCreated {entityId = def},
                    metadata = openMetadata'
                  }
          let openPayload =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.StreamCreation,
                    insertions = [openInsertion]
                  }

          openPayload
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          insertions1 <-
            Array.initialize 50 identity
              |> Array.map
                ( \index -> do
                    eventId <- Uuid.generate
                    metadata <- EventMetadata.new
                    let metadata' =
                          metadata
                            { EventMetadata.localPosition = Just (Event.StreamPosition (fromIntegral (index + 1))),
                              EventMetadata.eventId = eventId
                            }
                    Task.yield
                      Event.Insertion
                        { id = eventId,
                          event = ItemAdded {entityId = def, itemId = def, amount = 1},
                          metadata = metadata'
                        }
                )
              |> Task.mapArray identity

          let payload1 =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.AnyStreamState,
                    insertions = insertions1
                  }

          payload1
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          state1 <-
            context.fetcher.fetch context.entityName context.streamId
              |> Task.mapError toText
              |> Task.andThen
                ( \result -> case result of
                    EntityFound fetched -> Task.yield fetched.state
                    EntityNotFound -> Task.throw "Expected entity to exist"
                )

          state1.version |> shouldBe 51

          insertions2 <-
            Array.initialize 10 identity
              |> Array.map
                ( \index -> do
                    eventId <- Uuid.generate
                    metadata <- EventMetadata.new
                    let metadata' =
                          metadata
                            { EventMetadata.localPosition = Just (Event.StreamPosition (fromIntegral (index + 51))),
                              EventMetadata.eventId = eventId
                            }
                    Task.yield
                      Event.Insertion
                        { id = eventId,
                          event = ItemAdded {entityId = def, itemId = def, amount = 1},
                          metadata = metadata'
                        }
                )
              |> Task.mapArray identity

          let payload2 =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.AnyStreamState,
                    insertions = insertions2
                  }

          payload2
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          state2 <-
            context.fetcher.fetch context.entityName context.streamId
              |> Task.mapError toText
              |> Task.andThen
                ( \result -> case result of
                    EntityFound fetched -> Task.yield fetched.state
                    EntityNotFound -> Task.throw "Expected entity to exist"
                )

          state2.version |> shouldBe 61
          Array.length state2.cartItems |> shouldBe 1
