{-# LANGUAGE AllowAmbiguousTypes #-}

module Service (
  init,
  Service,
  UserApp,
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
import Channel (Channel, read)
import Channel qualified
import Command qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Console (print)
import Control.Exception qualified
import File qualified
import Graphics.Vty qualified as Vty
import IO qualified
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
       "actionsQueue" := Channel (Action event),
       "shouldExit" := Bool
     ]


type UserApp (model :: Type) (event :: Type) =
  Record
    '[ "init" := ((model, Action event)),
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
  print "[init] Creating queue"
  runtimeState <- Var.new Nothing
  actionsQueue <- Channel.new
  eventsQueue <- Channel.new
  let initialService =
        ANON
          { actionHandlers = Action.emptyRegistry,
            actionsQueue = actionsQueue,
            shouldExit = False
          }
  print "[init] Setting state"
  runtimeState
    |> setState initialService
  print "[init] Registering default action handlers"
  registerDefaultActionHandlers @event runtimeState
  let (initModel, initCmd) = userApp.init
  print "[init] Creating model ref"
  modelRef <- ConcurrentVar.new
  modelRef |> ConcurrentVar.set initModel
  print "[init] Writing init action"
  actionsQueue |> Channel.write initCmd
  print "[init] Starting loop"
  let cleanup threadName exception = do
        print ("[init] EXCEPTION: " ++ toText exception)
        print ("[init] " ++ threadName ++ " cleanup")
        print "[init] Cleaning up"
        runtimeState |> modifyState (\s -> s {shouldExit = True})
        case Control.Exception.fromException exception of
          Just Control.Exception.UserInterrupt -> do
            print "[init] Exiting"
            IO.exitSuccess
          _ -> pure ()
  let actionW =
        (actionWorker @event actionsQueue eventsQueue runtimeState)
          |> IO.catchAny (cleanup "actionWorker")

  let renderW =
        (renderWorker @model @event userApp modelRef runtimeState)
          |> IO.catchAny (cleanup "renderWorker")

  let mainW =
        (mainWorker @event @model userApp eventsQueue modelRef actionsQueue runtimeState)
          |> IO.catchAny (cleanup "mainWorker")

  AsyncIO.process actionW \_ -> do
    AsyncIO.process renderW \_ -> do
      print "[init] Starting triggers"
      runTriggers userApp.triggers eventsQueue
      -- The action worker must be the main loop
      -- or else it won't be able to exit the program
      mainW


-- \|> IO.finally do
--   print "[init] Cancelling workers"
--   AsyncIO.cancel actionPromise
--   AsyncIO.cancel renderPromise

-- AsyncIO.process actionW \_ -> do
--   AsyncIO.process renderW \_ -> do
--     mainW

-- PRIVATE

runTriggers ::
  forall (event :: Type).
  Array (Trigger event) ->
  Channel event ->
  IO ()
runTriggers triggers eventsQueue = do
  print "[runTriggers] Running triggers"

  print ("[runTriggers] Got " ++ (Array.length triggers |> toText) ++ " triggers")

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


modifyState ::
  forall (event :: Type).
  (Service event -> Service event) ->
  RuntimeState event ->
  IO ()
modifyState f runtimeState = do
  currentState <- getState runtimeState
  let newState = f currentState
  runtimeState |> setState newState


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
    |> registerActionHandler @(Command.CommandOptions event) @event "Command.parse" (Command.parseHandler @event)

  runtimeState
    |> registerActionHandler "continueWith" (Action.continueWithHandler @event)


actionWorker ::
  forall (event :: Type).
  (Unknown.Convertible event) =>
  Channel (Action event) ->
  Channel event ->
  RuntimeState event ->
  IO ()
actionWorker actionsQueue eventsQueue runtimeState = loop
 where
  loop = do
    print "[actionWorker] Checking exit condition"
    state <- getState runtimeState
    if state.shouldExit
      then do
        print "[actionWorker] Exiting due to shouldExit flag"
        IO.exitSuccess
      else do
        print "[actionWorker] Reading next action batch"
        nextActionBatch <- Channel.read actionsQueue
        print "[actionWorker] Getting state"
        state' <- getState runtimeState
        print "[actionWorker] Processing next action batch"
        result <- Action.processBatch state'.actionHandlers nextActionBatch

        case result of
          Action.Continue Nothing -> do
            print "[actionWorker] No actions to process"
            loop
          Action.Continue (Just event) -> do
            eventsQueue |> Channel.write event
            loop
          Action.Error msg -> do
            "[actionWorker] Error: "
              ++ msg
              |> print
            -- Decide whether to continue or exit based on the error
            -- For now, we'll continue, but you might want to exit for certain errors
            loop


data ServiceN = ServiceN
  deriving (Show, Ord, Eq)


data ServiceEvent (model :: Type)
  = ModelUpdated model
  | ExitRender
  deriving (Show, Ord, Eq)


renderWorker ::
  forall (model :: Type) (event :: Type).
  UserApp model event ->
  ConcurrentVar model ->
  RuntimeState event ->
  IO ()
renderWorker userApp modelRef runtimeState = do
  -- We wait a little bit to give the other workers a chance to start
  -- before we start rendering the view. This is also useful for when
  -- the action worker might exit the program on first execution.
  -- E.g. when the settings parser fails to parse the command line
  -- arguments.
  AsyncIO.sleep 500
  print "[renderWorker] Getting state"
  state <- getState runtimeState
  if state.shouldExit
    then do
      print "[renderWorker] Exiting"
      IO.exitSuccess
    else do
      print "[renderWorker] Getting model"
      model <- ConcurrentVar.peek modelRef

      print "[renderWorker] Setting up event channel"
      eventChannel <- Brick.BChan.newBChan 1

      let handleEvent event = case event of
            Brick.AppEvent (ModelUpdated newModel) -> do
              Brick.put newModel
            Brick.AppEvent ExitRender -> do
              Brick.halt
            _ -> Brick.halt

      let brickApp =
            Brick.App
              { Brick.appDraw = \s -> [userApp.view s |> Brick.txt @ServiceN],
                Brick.appChooseCursor = \_ _ -> Nothing,
                Brick.appHandleEvent = handleEvent,
                Brick.appStartEvent = pure (),
                Brick.appAttrMap = \_ -> Brick.attrMap Vty.defAttr []
              }

      print "[renderWorker] Running render model worker"
      renderModelWorker @model modelRef eventChannel runtimeState
        |> AsyncIO.run
        |> discard

      print "[renderWorker] Rendering view"
      _ <-
        Brick.customMainWithDefaultVty
          (Just eventChannel)
          brickApp
          model
      print "[renderWorker] Done rendering"


renderModelWorker ::
  forall (model :: Type) (event :: Type).
  ConcurrentVar model ->
  BChan (ServiceEvent model) ->
  RuntimeState event ->
  IO ()
renderModelWorker modelRef eventChannel runtimeState =
  forever do
    state <- getState runtimeState
    if state.shouldExit
      then do
        print "[renderModelWorker] Exiting"
        Brick.BChan.writeBChan eventChannel ExitRender
      else do
        -- print "[renderModelWorker] Peeking model"
        model <- ConcurrentVar.peek modelRef
        -- print "[renderModelWorker] Sending model update event"
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
  RuntimeState event ->
  IO ()
mainWorker userApp eventsQueue modelRef actionsQueue runtimeState = do
  -- let handleNoMoreEvents = do
  --       AsyncIO.sleep 20
  --       print "[mainWorker] No more events to process after 30 seconds"

  let readEvent = do
        print "[mainWorker] Reading next event"
        Channel.read eventsQueue

  forever do
    currentState <- getState runtimeState
    when currentState.shouldExit do
      print "[mainWorker] Exiting"
      IO.exitSuccess
    event <-
      readEvent
    -- \|> AsyncIO.withRecovery handleNoMoreEvents
    -- case eventRes of
    --   Result.Err _ ->
    --     -- handleNoMoreEvents
    --     pure ()
    --   Result.Ok event -> do
    print "[mainWorker] Getting model"
    model <- ConcurrentVar.get modelRef
    print ("[mainWorker] Got model: " ++ toText model)

    print "[mainWorker] Updating model"
    let (newModel, newCmd) = userApp.update event model
    print
      ( "[mainWorker] New model: "
          ++ toText newModel
          ++ "New action: "
          ++ toText newCmd
      )

    print "[mainWorker] Setting new model"
    modelRef |> ConcurrentVar.set newModel

    print "[mainWorker] Writing new action"
    actionsQueue |> Channel.write newCmd