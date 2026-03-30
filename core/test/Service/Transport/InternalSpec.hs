module Service.Transport.InternalSpec where

import Array qualified
import ConcurrentVar qualified
import Core
import Decider qualified
import Integration qualified
import Json qualified
import Map qualified
import Service qualified
import Service.Auth (RequestContext)
import Service.Auth qualified as Auth
import Service.Command.Core (Event (..), TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Service.Integration.Dispatcher qualified as Dispatcher
import Service.Response (CommandResponse (..))
import Service.Transport.Internal (InternalTransport (..))
import Task qualified
import Test
import Text qualified
import Uuid qualified


-- ============================================================================
-- Test fixtures
-- ============================================================================


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


-- ============================================================================
-- Tests
-- ============================================================================


spec :: Spec Unit
spec = do
  describe "Service.Transport.Internal" do
    -- ==================================================================
    -- Dispatch map construction
    -- ==================================================================
    describe "dispatch map construction" do
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

      it "internal command does not appear in public endpoint maps" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let runner =
              Service.new
                |> Service.command @CreateInternalRecord
                |> Service.toServiceRunner

        (endpointsByTransport, schemasByTransport, _dispatchMap) <- runner.getEndpointsByTransport eventStore Map.empty

        -- No transport bucket should contain the internal command
        endpointsByTransport
          |> Map.values
          |> Array.reduce (\cmdMap acc -> acc + Map.length cmdMap) 0
          |> shouldBe 0
        schemasByTransport
          |> Map.values
          |> Array.reduce (\cmdMap acc -> acc + Map.length cmdMap) 0
          |> shouldBe 0

    -- ==================================================================
    -- Dispatch handler behavior
    -- ==================================================================
    describe "dispatch handler" do
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

      it "returns Failed response for invalid JSON input" \_ -> do
        rawEventStore <- InMemory.new |> Task.mapError toText
        let runner =
              Service.new
                |> Service.command @CreateInternalRecord
                |> Service.toServiceRunner

        (_, _, dispatchMap) <- runner.getEndpointsByTransport rawEventStore Map.empty

        case dispatchMap |> Map.get "CreateInternalRecord" of
          Nothing -> fail "Expected dispatch handler to be present"
          Just handler -> do
            requestContext <- Auth.anonymousContext
            let invalidBody = "not-valid-json" |> Text.toBytes
            resultVar <- ConcurrentVar.new

            handler requestContext invalidBody (\(response, _responseBytes) ->
              ConcurrentVar.set response resultVar
              )

            result <- ConcurrentVar.get resultVar
            case result of
              Failed {} -> pass
              other -> fail [fmt|Expected Failed response, got: #{toText other}|]

      it "returns Failed response for empty request body" \_ -> do
        rawEventStore <- InMemory.new |> Task.mapError toText
        let runner =
              Service.new
                |> Service.command @CreateInternalRecord
                |> Service.toServiceRunner

        (_, _, dispatchMap) <- runner.getEndpointsByTransport rawEventStore Map.empty

        case dispatchMap |> Map.get "CreateInternalRecord" of
          Nothing -> fail "Expected dispatch handler to be present"
          Just handler -> do
            requestContext <- Auth.anonymousContext
            let emptyBody = "" |> Text.toBytes
            resultVar <- ConcurrentVar.new

            handler requestContext emptyBody (\(response, _responseBytes) ->
              ConcurrentVar.set response resultVar
              )

            result <- ConcurrentVar.get resultVar
            case result of
              Failed {} -> pass
              other -> fail [fmt|Expected Failed response, got: #{toText other}|]

      it "error message contains the command name on invalid input" \_ -> do
        rawEventStore <- InMemory.new |> Task.mapError toText
        let runner =
              Service.new
                |> Service.command @CreateInternalRecord
                |> Service.toServiceRunner

        (_, _, dispatchMap) <- runner.getEndpointsByTransport rawEventStore Map.empty

        case dispatchMap |> Map.get "CreateInternalRecord" of
          Nothing -> fail "Expected dispatch handler to be present"
          Just handler -> do
            requestContext <- Auth.anonymousContext
            let invalidBody = "{}" |> Text.toBytes
            resultVar <- ConcurrentVar.new

            handler requestContext invalidBody (\(response, _responseBytes) ->
              ConcurrentVar.set response resultVar
              )

            result <- ConcurrentVar.get resultVar
            case result of
              Failed {error} ->
                error |> shouldSatisfy (Text.contains "CreateInternalRecord")
              other -> fail [fmt|Expected Failed response, got: #{toText other}|]

      it "dispatches with anonymous context without rejection" \_ -> do
        rawEventStore <- InMemory.new |> Task.mapError toText
        let runner =
              Service.new
                |> Service.command @CreateInternalRecord
                |> Service.toServiceRunner

        (_, _, dispatchMap) <- runner.getEndpointsByTransport rawEventStore Map.empty

        case dispatchMap |> Map.get "CreateInternalRecord" of
          Nothing -> fail "Expected dispatch handler to be present"
          Just handler -> do
            -- Use anonymous context (no user claims) — same as integration dispatch
            requestContext <- Auth.anonymousContext
            recordId <- Uuid.generate
            let cmd = CreateInternalRecord {recordId = recordId}
            let body = Json.encodeText cmd |> Text.toBytes
            resultVar <- ConcurrentVar.new

            handler requestContext body (\(response, _responseBytes) ->
              ConcurrentVar.set response resultVar
              )

            result <- ConcurrentVar.get resultVar
            case result of
              Accepted {} -> pass
              other -> fail [fmt|Expected Accepted response with anonymous context, got: #{toText other}|]

    -- ==================================================================
    -- Duplicate detection
    -- ==================================================================
    describe "duplicate detection" do
      it "detects duplicate command names in dispatch map across services" \_ -> do
        rawEventStore <- InMemory.new |> Task.mapError toText
        -- Two runners that both provide the same command in their dispatch maps
        let runner =
              Service.new
                |> Service.command @CreateInternalRecord
                |> Service.toServiceRunner

        (_, _, dispatch1) <- runner.getEndpointsByTransport rawEventStore Map.empty
        (_, _, dispatch2) <- runner.getEndpointsByTransport rawEventStore Map.empty

        -- Simulate the Application.hs merge: merging dispatch2 into dispatch1
        -- should detect the duplicate "CreateInternalRecord" key
        let mergeResult =
              dispatch2
                |> Map.entries
                |> Array.reduce
                    (\(cmdName, handler) acc ->
                      case acc of
                        Err err -> Err err
                        Ok accMap ->
                          case accMap |> Map.get cmdName of
                            Just _ -> Err [fmt|Duplicate command: #{cmdName}|]
                            Nothing -> Ok (accMap |> Map.set cmdName handler)
                    )
                    (Ok dispatch1)

        case mergeResult of
          Err err ->
            err |> shouldSatisfy (Text.contains "CreateInternalRecord")
          Ok _ ->
            fail "Expected duplicate detection to produce an error"
