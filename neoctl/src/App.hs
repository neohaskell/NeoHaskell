{-| Assemble the neoctl HTTP service and its placeholder query.
Run this model with 'EventModel.run'.
-}
module App (appEventModel) where

import Core
import Maybe qualified
import Neoctl.Feature qualified as Feature
import Path qualified
import Service.EventModel qualified as EventModel
import Service.Transport.Web qualified as WebTransport
import Service.EventStore.Simple (SimpleEventStore (..))


appEventModel :: Result Text EventModel.EventModel
appEventModel = EventModel.eventModel do
  EventModel.appName "neoctl"
  EventModel.withEventStore @() (\_ -> SimpleEventStore {basePath = Path.fromText ".neo/events" |> Maybe.getOrDie, persistent = False})
  EventModel.withTransport WebTransport.server
  EventModel.withHost "127.0.0.1"
  EventModel.features Feature.flow
