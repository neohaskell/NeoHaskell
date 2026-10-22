module Service.CommandExecutor.MetadataSpec (spec) where

import Array qualified
import Auth.Claims (UserClaims (..))
import Basics
import Map qualified
import Maybe (Maybe (..))
import Service.Auth qualified as Auth
import Service.CommandExecutor.Core qualified as Executor
import Service.EntityFetcher.Core qualified as Fetcher
import Service.Event qualified as Event
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Stream qualified
import Task qualified
import Test
import Test.Service.Command.Core (AuthenticatedAddItem (..), initialCartState, applyCartEvent)
import Test.Service.EventStore.Core (CartEvent (..))
import ToText (toText)
import Uuid qualified


spec :: Spec Unit
spec = do
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
      result <- Executor.execute store fetcher entityName context command
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
