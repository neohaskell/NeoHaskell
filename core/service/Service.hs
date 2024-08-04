{-# LANGUAGE AllowAmbiguousTypes #-}

module Service (
  init,
  Service,
  registerActionHandler,
) where

import Action (Action)
import Action qualified
import Appendable ((++))
import Array (Array)
import Array qualified
import AsyncIO qualified
import Basics
import Brick qualified
import Brick.BChan (BChan)
import Brick.BChan qualified
import Channel (Channel)
import Channel qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Console (print)
import File qualified
import Graphics.Vty qualified as Vty
import Map qualified
import Maybe (Maybe (..))
import Text (Text)
import ToText (Show (..), ToText, toText)
import Trigger (Trigger (..))
import Unknown qualified
import Var (Var)
import Var qualified


type View = Text


type Service (event :: Type) =
  Record
    '[ "actionHandlers" := Action.HandlerRegistry,
       "actionsQueue" := Channel (Action event)
     ]


type UserApp (model :: Type) (event :: Type) =
  Record
    '[ "init" := (model, Action event),
       "view" := (model -> View),
       "triggers" := (Array (Trigger event)),
       "update" := (event -> model -> (model, Action event))
     ]


type RuntimeState (event :: Type) = Var (Maybe (Service event))


registerActionHandler ::
  forall payload event.
  ( Unknown.Convertible payload,
    Unknown.Convertible event,
    ToText payload,
    ToText event
  ) =>
  Text ->
  (payload -> IO event) ->
  RuntimeState event ->
  IO ()
registerActionHandler actionHandlerName handler runtimeState = do
  print "Getting state"
  service <- getState runtimeState
  print ("Got state: " ++ toText service)
  let actionHandler payload =
        case (Unknown.toValue payload) of
          Nothing -> do
            print "Payload is Nothing"
            pure Nothing
          Just pl -> do
            print ("Payload was Just " ++ toText pl)
            event <- handler pl
            pure (Unknown.fromValue (event :: event) |> Just)
  let newRegistry =
        service.actionHandlers
          |> Map.set actionHandlerName actionHandler
  let newService = service {actionHandlers = newRegistry}
  print "Setting state"
  runtimeState
    |> setState newService


init ::
  forall (model :: Type) (event :: Type).
  (Unknown.Convertible event, ToText event, ToText model) =>
  UserApp model event ->
  IO ()
init userApp = do
  print "Creating queue"
  runtimeState <- Var.new Nothing
  actionsQueue <- Channel.new
  eventsQueue <- Channel.new
  let initialService =
        ANON
          { actionHandlers = Action.emptyRegistry,
            actionsQueue = actionsQueue
          }
  print "Setting state"
  runtimeState
    |> setState initialService
  print "Registering default action handlers"
  registerDefaultActionHandlers @event runtimeState
  let (initModel, initCmd) = userApp.init
  print "Creating model ref"
  modelRef <- ConcurrentVar.new
  modelRef |> ConcurrentVar.set initModel
  print "Writing init action"
  actionsQueue |> Channel.write initCmd
  print "Starting loop"
  (actionWorker @event actionsQueue eventsQueue runtimeState)
    |> AsyncIO.run
    |> discard

  mainWorker @event userApp eventsQueue modelRef actionsQueue
    |> AsyncIO.run
    |> discard

  print "Starting triggers"
  runTriggers userApp.triggers eventsQueue

  (renderWorker @model @event userApp modelRef)


-- PRIVATE

runTriggers ::
  forall (event :: Type).
  Array (Trigger event) ->
  Channel event ->
  IO ()
runTriggers triggers eventsQueue = do
  let dispatchEvent event = do
        eventsQueue |> Channel.write event

  let triggerDispatch (Trigger process) =
        process dispatchEvent
          |> AsyncIO.run
          |> discard
  triggers
    |> Array.forEach triggerDispatch


getState ::
  forall (event :: Type).
  RuntimeState event ->
  IO (Service event)
getState runtimeState = do
  maybeService <- Var.get runtimeState
  case maybeService of
    Nothing -> dieWith "Service is not initialized"
    Just service -> pure service


setState ::
  forall (event :: Type).
  Service event ->
  RuntimeState event ->
  IO ()
setState value runtimeState = do
  runtimeState |> Var.set (Just value)


-- TODO: Action Handlers should come in the user app record as a map of
-- action names to handlers. This way, the user app can register its own
-- action handlers. Ideally also the user could omit the action handlers
-- and the service would still work with the default action handlers.
registerDefaultActionHandlers ::
  forall (event :: Type).
  ( Unknown.Convertible event,
    ToText event
  ) =>
  RuntimeState event ->
  IO ()
registerDefaultActionHandlers runtimeState = do
  runtimeState
    |> registerActionHandler @(File.ReadOptions event) @event "File.readText" (File.readTextHandler @event)

  runtimeState
    |> registerActionHandler "continueWith" (Action.continueWithHandler @event)


actionWorker ::
  forall (event :: Type).
  (Unknown.Convertible event) =>
  Channel (Action event) ->
  Channel event ->
  RuntimeState event ->
  IO ()
actionWorker actionsQueue eventsQueue runtimeState =
  forever do
    print "Reading next action batch"
    nextActionBatch <- Channel.read actionsQueue
    print "Getting state"
    state <- getState runtimeState
    print "Processing next action batch"
    processed <- Action.processBatch state.actionHandlers nextActionBatch

    case processed of
      Nothing -> do
        print "No actions to process"
      Just event -> do
        eventsQueue |> Channel.write event


data ServiceN = ServiceN
  deriving (Show, Ord, Eq)


data ServiceEvent (model :: Type)
  = ModelUpdated model
  deriving (Show, Ord, Eq)


renderWorker ::
  forall (model :: Type) (event :: Type).
  UserApp model event ->
  ConcurrentVar model ->
  IO ()
renderWorker userApp modelRef = do
  print "Getting model"
  model <- ConcurrentVar.peek modelRef

  print "Setting up event channel"
  eventChannel <- Brick.BChan.newBChan 1

  let handleEvent event = case event of
        Brick.AppEvent (ModelUpdated newModel) -> do
          Brick.put newModel
        _ -> Brick.halt

  let brickApp =
        Brick.App
          { Brick.appDraw = \state -> [userApp.view state |> Brick.txt @ServiceN],
            Brick.appChooseCursor = \_ _ -> Nothing,
            Brick.appHandleEvent = handleEvent,
            Brick.appStartEvent = pure (),
            Brick.appAttrMap = \_ -> Brick.attrMap Vty.defAttr []
          }

  print "Running render model worker"
  renderModelWorker @model modelRef eventChannel
    |> AsyncIO.run
    |> discard

  print "Rendering view"
  _ <-
    Brick.customMainWithDefaultVty
      (Just eventChannel)
      brickApp
      model
  print "Done rendering"


renderModelWorker ::
  forall (model :: Type).
  ConcurrentVar model ->
  BChan (ServiceEvent model) ->
  IO ()
renderModelWorker modelRef eventChannel =
  forever do
    print "Peeking model"
    model <- ConcurrentVar.peek modelRef
    print "Sending model update event"
    Brick.BChan.writeBChan eventChannel (ModelUpdated model)


mainWorker ::
  forall (event :: Type) (model :: Type).
  (ToText model, ToText event) =>
  UserApp
    model
    event ->
  Channel event ->
  ConcurrentVar model ->
  Channel (Action event) ->
  IO ()
mainWorker userApp eventsQueue modelRef actionsQueue =
  forever do
    print "Reading next event"
    event <- Channel.read eventsQueue

    print "Getting model"
    model <- ConcurrentVar.get modelRef
    print ("Got model: " ++ toText model)

    print "Updating model"
    let (newModel, newCmd) = userApp.update event model
    print
      ( "New model: "
          ++ toText newModel
          ++ "New action: "
          ++ toText newCmd
      )

    print "Setting new model"
    modelRef |> ConcurrentVar.set newModel

    print "Writing new action"
    actionsQueue |> Channel.write newCmd