module Service.EventWorker (run) where

import Action (Action)
import Basics
import Channel (Channel)
import qualified Channel
import ConcurrentVar (ConcurrentVar)
import qualified ConcurrentVar
import Console (log)
import IO (IO)
import qualified IO
import Service.Core
import qualified Service.RuntimeState as RuntimeState
import ToText (ToText, toPrettyText)


run ::
  forall (event :: Type) (model :: Type).
  (ToText model, ToText event) =>
  UserApp
    model
    event ->
  Channel event ->
  ConcurrentVar model ->
  Channel (Action event) ->
  RuntimeState.Reference event ->
  IO ()
run userApp eventsQueue modelRef actionsQueue runtimeState =
  forever do
    currentState <- RuntimeState.get runtimeState
    when currentState . shouldExit do
      log "Exiting"
      IO.exitSuccess
    log "Reading next event"
    event <- Channel.read eventsQueue
    log "Getting model"
    model <- ConcurrentVar.get modelRef
    log [fmt|Got model: {toPrettyText model}|]

    log "Updating model"
    let (newModel, newCmd) = userApp . update event model
    log [fmt|New model: {toPrettyText newModel} - New action: {toPrettyText newCmd}|]

    log "Setting new model"
    modelRef |> ConcurrentVar.set newModel

    log "Writing new action"
    actionsQueue |> Channel.write newCmd
