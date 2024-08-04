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


type Service (msg :: Type) =
  Record
    '[ "actionHandlers" := Action.HandlerRegistry,
       "actionsQueue" := Channel (Action msg)
     ]


type UserApp (model :: Type) (msg :: Type) =
  Record
    '[ "init" := (model, Action msg),
       "view" := (model -> View),
       "triggers" := (Array (Trigger msg)),
       "update" := (msg -> model -> (model, Action msg))
     ]


type RuntimeState (msg :: Type) = Var (Maybe (Service msg))


registerActionHandler ::
  forall payload msg.
  ( Unknown.Convertible payload,
    Unknown.Convertible msg,
    ToText payload,
    ToText msg
  ) =>
  Text ->
  (payload -> IO msg) ->
  RuntimeState msg ->
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
            msg <- handler pl
            pure (Unknown.fromValue (msg :: msg) |> Just)
  let newRegistry =
        service.actionHandlers
          |> Map.set actionHandlerName actionHandler
  let newService = service {actionHandlers = newRegistry}
  print "Setting state"
  runtimeState
    |> setState newService


init ::
  forall (model :: Type) (msg :: Type).
  (Unknown.Convertible msg, ToText msg, ToText model) =>
  UserApp model msg ->
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
  registerDefaultActionHandlers @msg runtimeState
  let (initModel, initCmd) = userApp.init
  print "Creating model ref"
  modelRef <- ConcurrentVar.new
  modelRef |> ConcurrentVar.set initModel
  print "Writing init action"
  actionsQueue |> Channel.write initCmd
  print "Starting loop"
  (actionWorker @msg actionsQueue eventsQueue runtimeState)
    |> AsyncIO.run
    |> discard

  mainWorker @msg userApp eventsQueue modelRef actionsQueue
    |> AsyncIO.run
    |> discard

  print "Starting triggers"
  runTriggers userApp.triggers eventsQueue

  (renderWorker @model @msg userApp modelRef)


-- PRIVATE

runTriggers ::
  forall (msg :: Type).
  Array (Trigger msg) ->
  Channel msg ->
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
  forall (msg :: Type).
  RuntimeState msg ->
  IO (Service msg)
getState runtimeState = do
  maybeService <- Var.get runtimeState
  case maybeService of
    Nothing -> dieWith "Service is not initialized"
    Just service -> pure service


setState ::
  forall (msg :: Type).
  Service msg ->
  RuntimeState msg ->
  IO ()
setState value runtimeState = do
  runtimeState |> Var.set (Just value)


-- TODO: Action Handlers should come in the user app record as a map of
-- action names to handlers. This way, the user app can register its own
-- action handlers. Ideally also the user could omit the action handlers
-- and the service would still work with the default action handlers.
registerDefaultActionHandlers ::
  forall (msg :: Type).
  ( Unknown.Convertible msg,
    ToText msg
  ) =>
  RuntimeState msg ->
  IO ()
registerDefaultActionHandlers runtimeState = do
  runtimeState
    |> registerActionHandler @(File.ReadOptions msg) @msg "File.readText" (File.readTextHandler @msg)

  runtimeState
    |> registerActionHandler "continueWith" (Action.continueWithHandler @msg)


actionWorker ::
  forall (msg :: Type).
  (Unknown.Convertible msg) =>
  Channel (Action msg) ->
  Channel msg ->
  RuntimeState msg ->
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
      Just msg -> do
        eventsQueue |> Channel.write msg


data ServiceN = ServiceN
  deriving (Show, Ord, Eq)


data ServiceEvent (model :: Type)
  = ModelUpdated model
  deriving (Show, Ord, Eq)


renderWorker ::
  forall (model :: Type) (msg :: Type).
  UserApp model msg ->
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
  forall (msg :: Type) (model :: Type).
  (ToText model, ToText msg) =>
  UserApp
    model
    msg ->
  Channel msg ->
  ConcurrentVar model ->
  Channel (Action msg) ->
  IO ()
mainWorker userApp eventsQueue modelRef actionsQueue =
  forever do
    print "Reading next event"
    msg <- Channel.read eventsQueue

    print "Getting model"
    model <- ConcurrentVar.get modelRef
    print ("Got model: " ++ toText model)

    print "Updating model"
    let (newModel, newCmd) = userApp.update msg model
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