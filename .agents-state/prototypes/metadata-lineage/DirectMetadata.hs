module Main (main) where

import Prelude (IO)
import Array qualified
import Auth.Claims (UserClaims (..))
import Basics
import DateTime qualified
import Map qualified
import Maybe (Maybe (..))
import Service.AccessControl (AccessError (..))
import Service.Auth (RequestContext (..))
import Service.Auth qualified as Auth
import Service.Command.Core (Command (..))
import Service.Event qualified as Event
import Service.Event.EntityName (EntityName (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as Metadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Stream qualified
import Task (Task)
import Task qualified
import Test
import Test.Hspec qualified as Hspec
import Test.Hspec.QuickCheck qualified as HspecQuickCheck
import Test.QuickCheck qualified as QuickCheck
import Test.Service.Command.Core (AuthenticatedAddItem)
import Text (Text)
import ToText (toText)
import Uuid qualified

prepare :: RequestContext -> Task Text RequestContext
prepare request =
  if request.requestId == Uuid.nil
    then do
      requestId <- Uuid.generate
      Task.yield request {Auth.requestId = requestId}
    else Task.yield request

populate :: RequestContext -> Maybe EventMetadata -> EventMetadata -> EventMetadata
populate request source fresh =
  case source of
    Nothing -> do
      let subject = case request.user of
            Nothing -> Nothing
            Just claims -> Just claims.sub
      fresh
        { Metadata.relatedUserSub = subject,
          Metadata.correlationId = Just (Uuid.toText request.requestId),
          Metadata.causationId = Nothing
        }
    Just parent -> do
      let sourceId = Uuid.toText parent.eventId
      let correlation = case parent.correlationId of
            Nothing -> sourceId
            Just value -> value
      fresh
        { Metadata.relatedUserSub = parent.relatedUserSub,
          Metadata.correlationId = Just correlation,
          Metadata.causationId = Just sourceId
        }

payloadForCommand :: RequestContext -> Maybe EventMetadata -> Task Text (Event.InsertionPayload Text)
payloadForCommand request source = do
  payload <- Event.payloadFromEvents (EntityName "DirectMetadataPrototype") (StreamId.toStreamId Uuid.nil) ["First", "Second"]
  let populateInsertion :: Event.Insertion Text -> Event.Insertion Text
      populateInsertion insertion = Event.Insertion
        { id = insertion.id,
          event = insertion.event,
          metadata = populate request source insertion.metadata
        }
  Task.yield payload {Event.insertions = payload.insertions |> Array.map populateInsertion}

testClaims :: UserClaims
testClaims = UserClaims {sub = "user-42", email = Nothing, name = Nothing, permissions = [], tenantId = Nothing, rawClaims = Map.empty}

main :: IO Unit
main = Hspec.hspec do
  describe "Populate existing EventMetadata directly" do
    it "root records verified subject and normalized request correlation" \_ -> do
      request <- prepare (Auth.emptyContext {Auth.user = Just testClaims})
      fresh <- Metadata.new
      let metadata = populate request Nothing fresh
      metadata.relatedUserSub |> shouldBe (Just "user-42")
      metadata.correlationId |> shouldBe (Just (Uuid.toText request.requestId))
      metadata.causationId |> shouldBe Nothing
      request.requestId |> shouldNotBe Uuid.nil
      requestAgain <- prepare request
      requestAgain.requestId |> shouldBe request.requestId

    it "event callbacks preserve tracing while retaining fresh event identity and timestamps" \_ -> do
      parent <- Metadata.new
      fresh <- Metadata.new
      request <- Auth.trustedContext
      let source = parent
            { Metadata.relatedUserSub = Just "user-42",
              Metadata.correlationId = Just "root-correlation",
              Metadata.causationId = Just "earlier-event",
              Metadata.createdAt = DateTime.fromEpochSeconds 0,
              Metadata.localPosition = Just (Event.StreamPosition 99),
              Metadata.globalPosition = Just (Event.StreamPosition 501)
            }
      let metadata = populate request (Just source) fresh
      metadata.relatedUserSub |> shouldBe (Just "user-42")
      metadata.correlationId |> shouldBe (Just "root-correlation")
      metadata.causationId |> shouldBe (Just (Uuid.toText source.eventId))
      metadata.eventId |> shouldBe fresh.eventId
      metadata.eventId |> shouldNotBe source.eventId
      metadata.createdAt |> shouldBe fresh.createdAt
      metadata.createdAt |> shouldNotBe source.createdAt
      metadata.localPosition |> shouldBe fresh.localPosition
      metadata.globalPosition |> shouldBe fresh.globalPosition
      canExecuteImpl @AuthenticatedAddItem request.user |> shouldBe (Just Unauthenticated)

    it "missing historical correlation falls back to the source event identity" \_ -> do
      source <- Metadata.new
      first <- Metadata.new
      second <- Metadata.new
      request <- Auth.trustedContext
      let childA = populate request (Just source) first
      let childB = populate request (Just source) second
      childA.correlationId |> shouldBe (Just (Uuid.toText source.eventId))
      childB.correlationId |> shouldBe childA.correlationId
      childA.causationId |> shouldBe (Just (Uuid.toText source.eventId))
      source.correlationId |> shouldBe Nothing

    it "present historical correlation remains opaque" \_ -> do
      source <- Metadata.new
      fresh <- Metadata.new
      request <- Auth.trustedContext
      ["", "  ", "legacy-correlation", Uuid.toText Uuid.nil]
        |> Task.forEach (\correlation -> do
          let child = populate request (Just source {Metadata.correlationId = Just correlation}) fresh
          child.correlationId |> shouldBe (Just correlation))

    it "autonomous commands get fresh correlations without actor or event cause" \_ -> do
      requestA <- Auth.trustedContext
      requestB <- Auth.trustedContext
      fresh <- Metadata.new
      let first = populate requestA Nothing fresh
      let second = populate requestB Nothing fresh
      first.relatedUserSub |> shouldBe Nothing
      first.causationId |> shouldBe Nothing
      first.correlationId |> shouldNotBe second.correlationId

    it "sibling events and rebuilt payloads share tracing but have distinct event IDs" \_ -> do
      request <- Auth.trustedContext
      source <- Metadata.new
      first <- payloadForCommand request (Just source)
      second <- payloadForCommand request (Just source)
      let tracing payload = payload.insertions |> Array.map (\item -> (item.metadata.relatedUserSub, item.metadata.correlationId, item.metadata.causationId))
      let expected = (Nothing, Just (Uuid.toText source.eventId), Just (Uuid.toText source.eventId))
      tracing first |> shouldBe [expected, expected]
      tracing second |> shouldBe [expected, expected]
      let identities payload = payload.insertions |> Array.map (\item -> item.metadata.eventId)
      identities first |> shouldNotBe (identities second)
      identities first |> shouldSatisfy (\ids -> Array.first ids != Array.last ids)

    it "directly populated metadata survives real in-memory persistence" \_ -> do
      request <- Auth.authenticatedContext testClaims
      payload <- payloadForCommand request Nothing
      store <- InMemory.new |> Task.map (EventStore.castEventStore @Text) |> Task.mapError toText
      store.insert payload |> Task.mapError toText |> Task.map (\_ -> unit)
      stream <- store.readAllStreamEvents payload.entityName payload.streamId |> Task.mapError toText
      messages <- Stream.toArray stream
      let events = EventStore.collectStreamEvents messages
      Array.length events |> shouldBe 2
      events |> Task.forEach (\event -> do
        event.metadata.relatedUserSub |> shouldBe (Just "user-42")
        event.metadata.correlationId |> shouldBe (Just (Uuid.toText request.requestId)))
      store.close |> Task.mapError toText

    HspecQuickCheck.prop "direct metadata propagation retains root correlation and immediate parent across callback depth" do
      \(QuickCheck.Positive rawDepth) -> do
        let depth = 1 + modBy 40 (rawDepth :: Int)
        let initial = EventMetadata
              { eventId = Uuid.nil,
                relatedUserSub = Just "user-42",
                correlationId = Just "root-correlation",
                causationId = Nothing,
                createdAt = DateTime.fromEpochSeconds 0,
                localPosition = Nothing,
                globalPosition = Nothing
              }
        let step number source = do
              let fresh = initial {Metadata.eventId = Uuid.generateV5 Uuid.nil (toText number)}
              populate Auth.emptyContext (Just source) fresh
        let final = Array.initialize depth identity |> Array.foldl step initial
        let expectedCause = if depth == 1 then Uuid.nil else Uuid.generateV5 Uuid.nil (toText (depth - 2))
        (final.relatedUserSub, final.correlationId, final.causationId) == (Just "user-42", Just "root-correlation", Just (Uuid.toText expectedCause))
