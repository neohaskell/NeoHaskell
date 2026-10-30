-- | Exercise typed inbound workers against two real entity runtimes.
-- The tests start workers through Application's worker supervisor.
module Service.InboundIntegration.TypedSpec where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Decider qualified
import Integration qualified
import Integration.Timer qualified as Timer
import Json qualified
import Map qualified
import Service qualified
import Service.AccessControl qualified as AccessControl
import Service.Application.Integrations qualified as Integrations
import Service.Application qualified as Application
import Service.Auth (RequestContext)
import Service.Command.Core (AccessError, Event (..), TransportsOf, UserClaims)
import Service.CommandExecutor.TH (command)
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.InMemory qualified as InMemory
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.InboundIntegration.ConfiguredFixture (ConfiguredWorker, InboundWorkerSettings)
import Service.Integration.Dispatcher qualified as Dispatcher
import Service.Transport.Internal (InternalTransport)
import Service.Transport.InternalSpec qualified as First
import "nhcore" Path qualified
import Maybe qualified
import Task qualified
import Test
import Text qualified
import Uuid qualified


data SecondEvent = SecondCreated {entityId :: Uuid}
  deriving (Eq, Show, Generic, Typeable)


instance Json.ToJSON SecondEvent


instance Json.FromJSON SecondEvent


data SecondEntity = SecondEntity {entityId :: Uuid, wasCreated :: Bool}
  deriving (Eq, Show, Generic, Typeable)


instance Json.ToJSON SecondEntity


instance Json.FromJSON SecondEntity


type instance EventOf SecondEntity = SecondEvent


type instance EntityOf SecondEvent = SecondEntity


type instance NameOf SecondEntity = "SecondEntity"


instance Entity SecondEntity where
  initialStateImpl = SecondEntity {entityId = Uuid.nil, wasCreated = False}
  updateImpl event _state = case event of
    SecondCreated {entityId} -> SecondEntity {entityId = entityId, wasCreated = True}


instance Event SecondEvent where
  getEventEntityIdImpl event = case event of
    SecondCreated {entityId} -> entityId


data CreateSecond = CreateSecond {recordId :: Uuid}
  deriving (Eq, Show, Generic, Typeable)


instance Json.ToJSON CreateSecond


instance Json.FromJSON CreateSecond


type instance EntityOf CreateSecond = SecondEntity


type instance TransportsOf CreateSecond = '[InternalTransport]


getEntityId :: CreateSecond -> Maybe Uuid
getEntityId commandValue = Just commandValue.recordId


decide :: CreateSecond -> Maybe SecondEntity -> RequestContext -> Decision SecondEvent
decide commandValue existing _context = case existing of
  Just _ -> Decider.reject "Second record already exists"
  Nothing -> [SecondCreated {entityId = commandValue.recordId}] |> Decider.acceptNew


canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess claims = AccessControl.publicAccess claims


command ''CreateSecond


data WorkerConfig = WorkerConfig {firstId :: Uuid, secondId :: Uuid}


data TwoEntityWorker = TwoEntityWorker


workerImpl :: WorkerConfig -> Integration.TypedInbound '[ '(First.CreateInternalRecord, First.InternalEntity), '(CreateSecond, SecondEntity)]
workerImpl config =
  Integration.typedInbound Integration.TypedInboundConfig
    { Integration.run = \emit -> do
        emit First.CreateInternalRecord {First.recordId = config.firstId}
        emit CreateSecond {recordId = config.secondId}
    }


inboundIntegration ''TwoEntityWorker


spec :: Spec Unit
spec = do
  describe "Service.InboundIntegration.Typed" do
    it "keeps the single-command timer dispatching to its entity runtime" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let runner = Service.new |> Service.command @First.CreateInternalRecord |> Service.toServiceRunner
      (_, _, endpoints) <- runner.getEndpointsByTransport eventStore Map.empty
      recordId <- Uuid.generate
      emitted <- ConcurrentVar.new
      let timer = Timer.every Timer.Every
            { interval = Timer.seconds 60
            , toCommand = \_ -> First.CreateInternalRecord {First.recordId = recordId}
            }
      worker <- AsyncTask.run
        (Integration.runInbound timer (\payload -> do
          Dispatcher.dispatchCommandWithSource endpoints Nothing payload
            |> Task.mapError Integration.PermanentFailure
          ConcurrentVar.set unit emitted
        ))
        |> Task.mapError toText
      outcome <- AsyncTask.race (ConcurrentVar.get emitted) (AsyncTask.sleep 1000)
      AsyncTask.cancel worker
      case outcome of
        AsyncTask.LeftWon _ -> Task.yield unit
        AsyncTask.RightWon _ -> fail "Timer did not emit its first command"
      fetcher <- EntityFetcher.new (eventStore |> EventStore.castEventStore @First.InternalEvent) (initialStateImpl @First.InternalEntity) (updateImpl @First.InternalEntity)
        |> Task.mapError toText
      result <- fetcher.fetch (EntityName "InternalEntity") (StreamId.toStreamId recordId) |> Task.mapError toText
      case result of
        EntityFetcher.EntityNotFound -> fail "Timer command did not reach its entity runtime"
        EntityFetcher.EntityFound fetched -> fetched.state.wasCreated |> shouldBe True

    it "dispatches one worker's two commands to their respective entity runtimes" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let firstRunner = Service.new |> Service.command @First.CreateInternalRecord |> Service.toServiceRunner
      let secondRunner = Service.new |> Service.command @CreateSecond |> Service.toServiceRunner
      (_, _, firstEndpoints) <- firstRunner.getEndpointsByTransport eventStore Map.empty
      (_, _, secondEndpoints) <- secondRunner.getEndpointsByTransport eventStore Map.empty
      let endpoints = Map.merge firstEndpoints secondEndpoints
      firstId <- Uuid.generate
      secondId <- Uuid.generate
      let inboundWorker = workerImpl WorkerConfig {firstId = firstId, secondId = secondId} |> Integration.asInbound
      workers <- Integrations.startInboundWorkers (Array.wrap inboundWorker) endpoints
      case Array.first workers of
        Nothing -> fail "Expected one inbound worker"
        Just worker -> AsyncTask.waitFor worker

      firstFetcher <- EntityFetcher.new (eventStore |> EventStore.castEventStore @First.InternalEvent) (initialStateImpl @First.InternalEntity) (updateImpl @First.InternalEntity)
        |> Task.mapError toText
      firstResult <- firstFetcher.fetch (EntityName "InternalEntity") (StreamId.toStreamId firstId) |> Task.mapError toText
      case firstResult of
        EntityFetcher.EntityNotFound -> fail "First entity did not receive its command"
        EntityFetcher.EntityFound fetched -> fetched.state.wasCreated |> shouldBe True

      secondFetcher <- EntityFetcher.new (eventStore |> EventStore.castEventStore @SecondEvent) (initialStateImpl @SecondEntity) (updateImpl @SecondEntity)
        |> Task.mapError toText
      secondResult <- secondFetcher.fetch (EntityName "SecondEntity") (StreamId.toStreamId secondId) |> Task.mapError toText
      case secondResult of
        EntityFetcher.EntityNotFound -> fail "Second entity did not receive its command"
        EntityFetcher.EntityFound fetched -> fetched.state.wasCreated |> shouldBe True

    it "rejects a missing receiving runtime before starting its worker" \_ -> do
      started <- ConcurrentVar.containing False
      let inboundWorker =
            Integration.typedInbound @'[ '(First.CreateInternalRecord, First.InternalEntity)]
              Integration.TypedInboundConfig
                { Integration.run = \_ -> started |> ConcurrentVar.modify (\_ -> True)
                }
              |> Integration.asInbound
      result <- Integrations.startInboundWorkers (Array.wrap inboundWorker) Map.empty |> Task.asResult
      case result of
        Ok _ -> fail "Expected missing command runtime to reject startup"
        Err message -> message |> shouldSatisfy (Text.contains "no receiving runtime is registered")
      wasStarted <- ConcurrentVar.get started
      wasStarted |> shouldBe False

    it "restarts a failed typed worker and stops after a successful attempt" \_ -> do
      attempts <- ConcurrentVar.containing (0 :: Int)
      let inboundWorker =
            Integration.typedInbound @'[ '(First.CreateInternalRecord, First.InternalEntity)]
              Integration.TypedInboundConfig
                { Integration.run = \_ -> do
                    attempts |> ConcurrentVar.modify (\count -> count + 1)
                    count <- ConcurrentVar.peek attempts
                    Task.when (count == 1) do
                      Task.throw (Integration.PermanentFailure "first attempt failed")
                }
              |> Integration.asInbound
      eventStore <- InMemory.new |> Task.mapError toText
      let runner = Service.new |> Service.command @First.CreateInternalRecord |> Service.toServiceRunner
      (_, _, endpoints) <- runner.getEndpointsByTransport eventStore Map.empty
      workers <- Integrations.startInboundWorkers (Array.wrap inboundWorker) endpoints
      case Array.first workers of
        Nothing -> fail "Expected one inbound worker"
        Just worker -> AsyncTask.waitFor worker
      count <- ConcurrentVar.peek attempts
      count |> shouldBe 2

    it "constructs a config-dependent handler after Application loads config" \_ -> do
      let storeConfig = SimpleEventStore
            { basePath = Path.fromText ".neo/inbound-config-order" |> Maybe.getOrDie
            , persistent = False
            }
      let app = Application.new
            |> Application.withConfig @InboundWorkerSettings
            |> Application.withEventStore @() (\_ -> storeConfig)
            |> Application.withInboundHandler @ConfiguredWorker
      result <- Application.run app |> Task.asResult
      case result of
        Ok _ -> fail "Expected the declared command to require a receiving runtime"
        Err message -> message |> shouldSatisfy (Text.contains "no receiving runtime is registered")
