module Service.ActionWorker (run) where

import Action (Action)
import Action qualified
import Basics
import Channel (Channel)
import Channel qualified
import Console (log)
import IO (IO)
import IO qualified
import Maybe (Maybe (..))
import Service.RuntimeState qualified as RuntimeState
import Unknown qualified


run ::
  forall (event :: Type).
  (Unknown.Convertible event) =>
  Channel (Action event) ->
  Channel event ->
  RuntimeState.Reference event ->
  IO ()
run actionsQueue eventsQueue runtimeState = loop
 where
  loop = do
    log "Checking exit condition"
    state <- RuntimeState.get runtimeState
    if state.shouldExit
      then do
        log "Exiting due to shouldExit flag"
        IO.exitSuccess
      else do
        log "Reading next action batch"
        nextActionBatch <- Channel.read actionsQueue
        log "Getting state"
        state' <- RuntimeState.get runtimeState
        log "Processing next action batch"
        result <- Action.processBatch state'.actionHandlers nextActionBatch
        case result of
          Action.Continue Nothing -> do
            log "No actions to process"
            loop
          Action.Continue (Just unknownEvent) -> do
            -- Attempt to convert to the top-level event type
            case Unknown.toValue unknownEvent of
              Just event -> do
                eventsQueue |> Channel.write event
                loop
              Nothing -> do
                log "Error: Could not convert event to the expected event type"
                loop
          Action.Error msg -> do
            log [fmt|Error: {msg}|]
            loop