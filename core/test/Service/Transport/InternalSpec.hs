module Service.Transport.InternalSpec where

import Core
import Decider qualified
import Integration qualified
import Json qualified
import Map qualified
import Service qualified
import Service.Auth (RequestContext)
import Service.Command.Core (Event (..), TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Service.Integration.Dispatcher qualified as Dispatcher
import Service.Transport.Internal (InternalTransport)
import Task qualified
import Test
import Uuid qualified


data InternalEvent
  = InternalCreated {entityId :: Uuid}
  deriving (Eq, Show, Generic, Typeable)


instance Json.ToJSON InternalEvent


instance Json.FromJSON InternalEvent


data InternalEntity = InternalEntity
  { entityId :: Uuid
  , wasCreated :: Bool
  }
  deriving (Eq, Show, Generic, Typeable)


instance Json.ToJSON InternalEntity


instance Json.FromJSON InternalEntity


type instance EventOf InternalEntity = InternalEvent


type instance EntityOf InternalEvent = InternalEntity


type instance NameOf InternalEntity = "InternalEntity"


instance Entity InternalEntity where
  initialStateImpl = InternalEntity {entityId = Uuid.nil, wasCreated = False}
  updateImpl event _state =
    case event of
      InternalCreated {entityId} ->
        InternalEntity
          { entityId = entityId
          , wasCreated = True
          }


instance Event InternalEvent where
  getEventEntityIdImpl event =
    case event of
      InternalCreated {entityId} -> entityId


data CreateInternalRecord = CreateInternalRecord
  { recordId :: Uuid
  }
  deriving (Eq, Show, Generic, Typeable)


instance Json.ToJSON CreateInternalRecord


instance Json.FromJSON CreateInternalRecord


type instance EntityOf CreateInternalRecord = InternalEntity


type instance TransportsOf CreateInternalRecord = '[InternalTransport]


getEntityId :: CreateInternalRecord -> Maybe Uuid
getEntityId cmd = Just cmd.recordId


decide :: CreateInternalRecord -> Maybe InternalEntity -> RequestContext -> Decision InternalEvent
decide cmd maybeEntity _ctx =
  case maybeEntity of
    Just _ ->
      Decider.reject "Record already exists"
    Nothing ->
      [InternalCreated {entityId = cmd.recordId}]
        |> Decider.acceptNew


command ''CreateInternalRecord


spec :: Spec Unit
spec = do
  describe "Service.Transport.Internal" do
    it "builds dispatch handlers without requiring a registered runnable transport" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let runner =
            Service.new
              |> Service.command @CreateInternalRecord
              |> Service.toServiceRunner

      (endpointsByTransport, schemasByTransport, dispatchMap) <- runner.getEndpointsByTransport eventStore Map.empty

      Map.length endpointsByTransport |> shouldBe 0
      Map.length schemasByTransport |> shouldBe 0
      dispatchMap |> Map.contains "CreateInternalRecord" |> shouldBe True

    it "dispatches internal-only commands through the transport-independent dispatch map" \_ -> do
      rawEventStore <- InMemory.new |> Task.mapError toText
      let runner =
            Service.new
              |> Service.command @CreateInternalRecord
              |> Service.toServiceRunner

      (_, _, dispatchMap) <- runner.getEndpointsByTransport rawEventStore Map.empty

      recordId <- Uuid.generate
      let payload = Integration.makeCommandPayload CreateInternalRecord {recordId = recordId}

      Dispatcher.dispatchCommand dispatchMap payload

      let typedEventStore = rawEventStore |> EventStore.castEventStore @InternalEvent
      fetcher <-
        EntityFetcher.new
          typedEventStore
          (initialStateImpl @InternalEntity)
          (updateImpl @InternalEntity)
          |> Task.mapError toText
      result <- fetcher.fetch (EntityName "InternalEntity") (StreamId.toStreamId recordId) |> Task.mapError toText

      case result of
        EntityFetcher.EntityNotFound ->
          fail "Expected internal command dispatch to create the record"
        EntityFetcher.EntityFound fetched -> do
          fetched.state.entityId |> shouldBe recordId
          fetched.state.wasCreated |> shouldBe True
