{-# LANGUAGE AllowAmbiguousTypes #-}

module Service (
  init,
  UserApp,
  RuntimeState.registerActionHandler,
) where

import Action qualified
import Appendable ((++))
import Array (Array)
import Array qualified
import AsyncIO qualified
import Basics
import Channel (Channel)
import Channel qualified
import ConcurrentVar qualified
import Console (print)
import Control.Exception qualified
import IO qualified
import Maybe (Maybe (..))
import Service.ActionWorker qualified as ActionWorker
import Service.Core (UserApp)
import Service.Core qualified as Core
import Service.EventWorker qualified as EventWorker
import Service.RenderWorker qualified as RenderWorker
import Service.RuntimeState qualified as RuntimeState
import ToText (ToText, toText)
import Trigger (Trigger (..))
import Unknown qualified
import Var qualified


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
        RuntimeState.RuntimeState
          { actionHandlers = Action.emptyRegistry,
            actionsQueue = actionsQueue,
            shouldExit = False
          }
  print "[init] Setting state"
  runtimeState
    |> RuntimeState.set initialService
  print "[init] Registering default action handlers"
  runtimeState
    |> RuntimeState.registerDefaultActionHandlers @event
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
        runtimeState |> RuntimeState.modify (\s -> s {RuntimeState.shouldExit = True})
        case Control.Exception.fromException exception of
          Just Control.Exception.UserInterrupt -> do
            print "[init] Exiting"
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
      print "[init] Starting triggers"
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
  print "[runTriggers] Running triggers"

  print [fmt|"[runTriggers] Got {Array.length triggers |> toText} triggers|]

  let dispatchEvent event = do
        eventsQueue |> Channel.write event

  let triggerDispatch (Trigger process) =
        process dispatchEvent
          |> AsyncIO.run
          |> discard

  triggers
    |> Array.forEach triggerDispatch
