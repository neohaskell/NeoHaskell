{-# LANGUAGE AllowAmbiguousTypes #-}

module Service (
  run,
  UserApp (..),
  RuntimeState.registerActionHandler,
) where

import Action qualified
import Array (Array)
import Array qualified
import AsyncIO qualified
import Basics
import Channel (Channel)
import Channel qualified
import ConcurrentVar qualified
import Console (log)
import Control.Exception qualified
import IO (IO)
import IO qualified
import Maybe (Maybe (..))
import Service.ActionWorker qualified as ActionWorker
import Service.Core (UserApp)
import Service.Core qualified as Core
import Service.EventWorker qualified as EventWorker
import Service.RenderWorker qualified as RenderWorker
import Service.RuntimeState qualified as RuntimeState
import Text (Text)
import ToText (ToText, toPrettyText)
import Trigger (Trigger (..))
import Unknown qualified
import Var qualified


run ::
  forall (model :: Type) (event :: Type).
  (Unknown.Convertible event, ToText event, ToText model) =>
  UserApp model event ->
  IO ()
run userApp = do
  log "[init] Creating queue"
  runtimeState <- Var.new Nothing
  actionsQueue <- Channel.new
  eventsQueue <- Channel.new
  let initialService =
        RuntimeState.RuntimeState
          { actionHandlers = Action.emptyRegistry,
            actionsQueue = actionsQueue,
            shouldExit = False
          }
  log "[init] Setting state"
  runtimeState
    |> RuntimeState.set initialService
  log "[init] Registering default action handlers"
  runtimeState
    |> RuntimeState.registerDefaultActionHandlers @event
  let (initModel, initCmd) = userApp.init
  log "[init] Creating model ref"
  modelRef <- ConcurrentVar.new
  modelRef |> ConcurrentVar.set initModel
  log "[init] Writing init action"
  actionsQueue |> Channel.write initCmd
  log "[init] Starting loop"
  let cleanup :: Text -> Control.Exception.SomeException -> IO ()
      cleanup threadName exception = do
        log [fmt|[init] EXCEPTION in {threadName}: {toPrettyText exception}|]
        log "[init] Cleaning up"
        runtimeState |> RuntimeState.modify (\s -> s {RuntimeState.shouldExit = True})
        case Control.Exception.fromException exception of
          Just Control.Exception.UserInterrupt -> do
            log "[init] Exiting"
            IO.exitSuccess
          _ -> pure ()
  let actionWorker =
        (ActionWorker.run @event actionsQueue eventsQueue runtimeState)
          |> IO.catchAny (cleanup "actionWorker")

  let renderWorker =
        (RenderWorker.run @model @event userApp modelRef runtimeState)
          |> IO.catchAny (cleanup "renderWorker")

  let eventWorker =
        (EventWorker.run @event @model userApp eventsQueue modelRef actionsQueue runtimeState)
          |> IO.catchAny (cleanup "mainWorker")

  AsyncIO.process actionWorker \_ -> do
    AsyncIO.process renderWorker \_ -> do
      log "[init] Starting triggers"
      runTriggers userApp.triggers eventsQueue
      -- The action worker must be the main loop
      -- or else it won't be able to exit the program
      eventWorker


-- PRIVATE

runTriggers ::
  forall (event :: Type).
  Array (Trigger event) ->
  Channel event ->
  IO ()
runTriggers triggers eventsQueue = do
  log "[runTriggers] Running triggers"

  log [fmt|"[runTriggers] Got {Array.length triggers |> toPrettyText} triggers|]

  let dispatchEvent event = do
        eventsQueue |> Channel.write event

  let triggerDispatch (Trigger process) =
        process dispatchEvent
          |> AsyncIO.run
          |> discard

  triggers
    |> Array.forEach triggerDispatch
