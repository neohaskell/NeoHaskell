{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Service.EventWorker (run) where

import Action (Action)
import Basics
import Channel (Channel)
import Channel qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Console (print)
import IO qualified
import Service.Core (UserApp)
import Service.RuntimeState qualified as RuntimeState
import ToText (ToText, toText)


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
    when currentState.shouldExit do
      print "Exiting"
      IO.exitSuccess
    print "Reading next event"
    event <- Channel.read eventsQueue
    print "Getting model"
    model <- ConcurrentVar.get modelRef
    print [fmt|Got model: {toText model}|]

    print "Updating model"
    let (newModel, newCmd) = userApp.update event model
    print [fmt|New model: {toText newModel} - New action: {toText newCmd}|]

    print "Setting new model"
    modelRef |> ConcurrentVar.set newModel

    print "Writing new action"
    actionsQueue |> Channel.write newCmd