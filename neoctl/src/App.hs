{-| Assemble neoctl's Event Model with its placeholder query and CLI transport.
The launcher runs this model for every supported argument vector.
-}
module App (appEventModel) where

import Core
import Maybe qualified
import Neoctl.Feature qualified as Feature
import Neoctl.Invocation qualified as Invocation
import Neoctl.Transport qualified as NeoctlTransport
import Path qualified
import Service.EventModel qualified as EventModel
import Service.EventStore.Simple (SimpleEventStore (..))


appEventModel :: [Text] -> Result Text EventModel.EventModel
appEventModel arguments = EventModel.eventModel do
  EventModel.appName "neoctl"
  EventModel.withEventStore @() (\_ -> SimpleEventStore {basePath = Path.fromText ".neo/events" |> Maybe.getOrDie, persistent = False})
  let invocation = arguments |> Invocation.classifyArguments
  EventModel.withTransport (NeoctlTransport.transport invocation)
  EventModel.features Feature.flow
