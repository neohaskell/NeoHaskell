module Integration.ContextSpec where

import Array qualified
import AsyncTask qualified

import Auth.SecretStore (SecretStore (..))
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import ConcurrentMap qualified
import ConcurrentVar qualified
import Core
import Integration qualified
import Json qualified
import Map qualified
import Service.Event (EntityName (..), Event (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.InMemory qualified as InMemory
import Service.Integration.Dispatcher qualified as Dispatcher
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import Task qualified
import Test
import Test.Auth.TestUtils qualified as TestUtils


data ContextCommand = ContextCommand
  { hadTokens :: Bool
  }
  deriving (Eq, Show, Generic, Typeable)


instance Json.ToJSON ContextCommand


instance Json.FromJSON ContextCommand


type instance NameOf ContextCommand = "ContextCommand"


data ContextEvent = ContextEvent
  { value :: Int
  }
  deriving (Eq, Show, Generic, Typeable)


instance Json.ToJSON ContextEvent


makeContext :: Task Text Integration.ActionContext
makeContext = do
  store <- InMemorySecretStore.new
  locks <- ConcurrentMap.new
  Task.yield
    Integration.ActionContext
      { Integration.secretStore = store
      , Integration.providerRegistry = Integration.fromMap Map.empty
      , Integration.refreshLocks = locks
      , Integration.fileAccess = Nothing
      , Integration.outboundDispatch = DispatchRegistry.empty
      }


spec :: Spec Unit
spec = do
  describe "Integration.Context" do
    it "creates context-aware actions" \_ -> do
      context <- makeContext

      let contextAction _ctx = Integration.noCommand
      let testAction = Integration.action contextAction

      result <- Integration.runAction context testAction |> Task.mapError toText
      result |> shouldBe Nothing

    it "executes action with context" \_ -> do
      context <- makeContext
      let tokenKey = TestUtils.makeTestTokenKey "user-1"
      case context.secretStore of
        SecretStore {put} ->
          put tokenKey TestUtils.makeTestTokenSet

      let contextAction ctx = do
            case ctx.secretStore of
              SecretStore {get} -> do
                maybeTokens <- get tokenKey |> Task.mapError Integration.UnexpectedError
                let hasTokens = case maybeTokens of
                      Just _ -> True
                      Nothing -> False
                Integration.emitCommand ContextCommand {hadTokens = hasTokens}

      let testAction = Integration.action contextAction
      result <- Integration.runAction context testAction |> Task.mapError toText
      case result of
        Just payload -> do
          case Json.decode @ContextCommand payload.commandData of
            Ok decoded -> decoded.hadTokens |> shouldBe True
            Err _ -> fail "Failed to decode command payload"
        Nothing -> fail "Expected command payload"

    it "runs existing actions without context" \_ -> do
      context <- makeContext
      let testAction = Integration.action \_ctx -> Integration.noCommand
      result <- Integration.runAction context testAction |> Task.mapError toText
      result |> shouldBe Nothing

    it "runs dispatcher with empty context" \_ -> do
      contextSeen <- ConcurrentVar.containing (Nothing :: Maybe Bool)

      let runner =
            Dispatcher.OutboundRunner
              { entityTypeName = "TestEntity",
                processEvent = \ctx _eventStore _event -> do
                  let isEmpty = (ctx.providerRegistry |> Integration.entries |> Array.length) == 0
                  contextSeen |> ConcurrentVar.modify (\_ -> Just isEmpty)
                  Task.yield Array.empty
              }

      eventStore <- InMemory.new |> Task.mapError toText
      emptyStore <- InMemorySecretStore.new
      emptyLocks <- ConcurrentMap.new
      let emptyContext = Integration.ActionContext
            { Integration.secretStore = emptyStore
            , Integration.providerRegistry = Integration.fromMap Map.empty
            , Integration.refreshLocks = emptyLocks
            , Integration.fileAccess = Nothing
            , Integration.outboundDispatch = DispatchRegistry.empty
            }
      dispatcher <-
        Dispatcher.newWithLifecycleConfig
          Dispatcher.defaultConfig
          eventStore
          [runner]
          []
          Map.empty
          emptyContext

      metadata <- EventMetadata.new
      let streamId = StreamId.fromTextUnsafe "entity-A"
      let event =
            Event
              { entityName = EntityName "TestEntity"
              , streamId = streamId
              , event = Json.encode (ContextEvent {value = 1})
              , metadata = metadata
              }

      Dispatcher.dispatch dispatcher event
      AsyncTask.sleep 50 |> Task.mapError (\_ -> "sleep error" :: Text)

      seen <- ConcurrentVar.peek contextSeen
      seen |> shouldBe (Just True)

      Dispatcher.shutdown dispatcher
