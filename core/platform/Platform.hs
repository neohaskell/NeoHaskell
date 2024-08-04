{-# LANGUAGE AllowAmbiguousTypes #-}

module Platform (
  init,
  Platform,
  registerCommandHandler,
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


type Platform (msg :: Type) =
  Record
    '[ "commandHandlers" := Action.HandlerRegistry,
       "commandsQueue" := Channel (Action msg)
     ]


type UserApp (model :: Type) (msg :: Type) =
  Record
    '[ "init" := (model, Action msg),
       "view" := (model -> View),
       "triggers" := (Array (Trigger msg)),
       "update" := (msg -> model -> (model, Action msg))
     ]


type RuntimeState (msg :: Type) = Var (Maybe (Platform msg))


registerCommandHandler ::
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
registerCommandHandler commandHandlerName handler runtimeState = do
  print "Getting state"
  platform <- getState runtimeState
  print ("Got state: " ++ toText platform)
  let commandHandler payload =
        case (Unknown.toValue payload) of
          Nothing -> do
            print "Payload is Nothing"
            pure Nothing
          Just pl -> do
            print ("Payload was Just " ++ toText pl)
            msg <- handler pl
            pure (Unknown.fromValue (msg :: msg) |> Just)
  let newRegistry =
        platform.commandHandlers
          |> Map.set commandHandlerName commandHandler
  let newPlatform = platform {commandHandlers = newRegistry}
  print "Setting state"
  runtimeState
    |> setState newPlatform


init ::
  forall (model :: Type) (msg :: Type).
  (Unknown.Convertible msg, ToText msg, ToText model) =>
  UserApp model msg ->
  IO ()
init userApp = do
  print "Creating queue"
  runtimeState <- Var.new Nothing
  commandsQueue <- Channel.new
  eventsQueue <- Channel.new
  let initialPlatform =
        ANON
          { commandHandlers = Action.emptyRegistry,
            commandsQueue = commandsQueue
          }
  print "Setting state"
  runtimeState
    |> setState initialPlatform
  print "Registering default action handlers"
  registerDefaultCommandHandlers @msg runtimeState
  let (initModel, initCmd) = userApp.init
  print "Creating model ref"
  modelRef <- ConcurrentVar.new
  modelRef |> ConcurrentVar.set initModel
  print "Writing init action"
  commandsQueue |> Channel.write initCmd
  print "Starting loop"
  (commandWorker @msg commandsQueue eventsQueue runtimeState)
    |> AsyncIO.run
    |> discard

  mainWorker @msg userApp eventsQueue modelRef commandsQueue
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
  IO (Platform msg)
getState runtimeState = do
  maybePlatform <- Var.get runtimeState
  case maybePlatform of
    Nothing -> dieWith "Platform is not initialized"
    Just platform -> pure platform


setState ::
  forall (msg :: Type).
  Platform msg ->
  RuntimeState msg ->
  IO ()
setState value runtimeState = do
  runtimeState |> Var.set (Just value)


-- TODO: Action Handlers should come in the user app record as a map of
-- action names to handlers. This way, the user app can register its own
-- action handlers. Ideally also the user could omit the action handlers
-- and the platform would still work with the default action handlers.
registerDefaultCommandHandlers ::
  forall (msg :: Type).
  ( Unknown.Convertible msg,
    ToText msg
  ) =>
  RuntimeState msg ->
  IO ()
registerDefaultCommandHandlers runtimeState = do
  runtimeState
    |> registerCommandHandler @(File.ReadOptions msg) @msg "File.readText" (File.readTextHandler @msg)

  runtimeState
    |> registerCommandHandler "continueWith" (Action.continueWithHandler @msg)


commandWorker ::
  forall (msg :: Type).
  (Unknown.Convertible msg) =>
  Channel (Action msg) ->
  Channel msg ->
  RuntimeState msg ->
  IO ()
commandWorker commandsQueue eventsQueue runtimeState =
  forever do
    print "Reading next action batch"
    nextCommandBatch <- Channel.read commandsQueue
    print "Getting state"
    state <- getState runtimeState
    print "Processing next action batch"
    processed <- Action.processBatch state.commandHandlers nextCommandBatch

    case processed of
      Nothing -> do
        print "No commands to process"
      Just msg -> do
        eventsQueue |> Channel.write msg


data PlatformN = PlatformN
  deriving (Show, Ord, Eq)


data PlatformEvent (model :: Type)
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
          { Brick.appDraw = \state -> [userApp.view state |> Brick.txt @PlatformN],
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
  BChan (PlatformEvent model) ->
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
mainWorker userApp eventsQueue modelRef commandsQueue =
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
    commandsQueue |> Channel.write newCmd