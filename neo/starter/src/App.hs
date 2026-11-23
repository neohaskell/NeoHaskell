module App (appEventModel) where

import Core
import Service.Transport.Web qualified as WebTransport
import Starter.Config (StarterConfig (..))
import Starter.Counter.Feature (counterFlow)


appEventModel :: EventModel
appEventModel = eventModel do
  appName "Counter"
  withConfig @StarterConfig
  withPortFromConfig (\(config :: StarterConfig) -> config.httpPort)
  withEventStore inMemory
  withTransport WebTransport.server
  features do
    counterFlow
