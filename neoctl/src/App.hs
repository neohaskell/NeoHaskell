{-| Assemble the neoctl HTTP service and its placeholder query.
The launcher builds the model from 'appSettings'.
-}
module App (appSettings) where

import Core
import Maybe qualified
import Neoctl.Feature qualified as Feature
import Path qualified
import Service.EventModel qualified as EventModel
import Service.Transport.Web qualified as WebTransport
import Service.EventStore.Simple (SimpleEventStore (..))


appSettings :: EventModel.EventModelAction
appSettings = do
  EventModel.appName "neoctl"
  EventModel.withEventStore @() (\_ -> SimpleEventStore {basePath = Path.fromText ".neo/events" |> Maybe.getOrDie, persistent = False})
  EventModel.withTransport WebTransport.server
  EventModel.withHost "127.0.0.1"
  EventModel.features Feature.flow
