module Main (main) where

import Prelude (IO)
import Array qualified
import Auth.Claims (UserClaims (..))
import Basics
import Console qualified
import Json qualified
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
import Task (Task)
import Task qualified
import Test
import Test.Hspec qualified as Hspec
import Test.Service.Command.Core (AuthenticatedAddItem (..), initialCartState, applyCartEvent)
import Test.Service.EventStore.Core (CartEvent (..))
import Text (Text)
import ToText (toText)
import Uuid qualified

acceptedCommandMetadata :: Task Text (EventMetadata, Text)
acceptedCommandMetadata = do
  store <- InMemory.new |> Task.map (EventStore.castEventStore @CartEvent) |> Task.mapError toText
  cartId <- Uuid.generate
  let entityName = EntityName "CartEntity"
  let streamId = StreamId.toStreamId cartId
  seed <- Event.payloadFromEvents entityName streamId [CartCreated cartId]
  store.insert seed |> Task.mapError toText |> Task.map (\_ -> unit)
  fetcher <- Fetcher.new store initialCartState applyCartEvent |> Task.mapError toText
  let claims = UserClaims {sub = "lineage-prototype-user", email = Nothing, name = Nothing, permissions = [], tenantId = Nothing, rawClaims = Map.empty}
  context <- Auth.authenticatedContext claims
  let command = AuthenticatedAddItem {cartId, itemId = Uuid.nil, amount = 1}
  result <- Executor.execute store fetcher entityName context command
  case result of
    Executor.CommandAccepted {eventsAppended} -> eventsAppended |> shouldBe 1
    other -> fail [fmt|Expected accepted command, received #{toText other}|]
  messages <- store.readAllStreamEvents entityName streamId |> Task.mapError toText
  events <- messages |> Stream.toArray |> Task.map EventStore.collectStreamEvents
  Array.length events |> shouldBe 2
  store.close |> Task.mapError toText
  case Array.last events of
    Nothing -> Task.throw "No accepted event read back"
    Just event -> do
      Console.print (Json.encodeText event.metadata)
      Task.yield (event.metadata, Uuid.toText context.requestId)

main :: IO Unit
main = Hspec.hspec do
  describe "Baseline characterization" do
    it "accepted authenticated command persists empty lineage" \_ -> do
      (metadata, _) <- acceptedCommandMetadata
      metadata.relatedUserSub |> shouldBe Nothing
      metadata.correlationId |> shouldBe Nothing
      metadata.causationId |> shouldBe Nothing
  describe "Required behavior" do
    it "accepted authenticated command retains actor and root correlation" \_ -> do
      (metadata, requestId) <- acceptedCommandMetadata
      metadata.relatedUserSub |> shouldBe (Just "lineage-prototype-user")
      metadata.correlationId |> shouldBe (Just requestId)
