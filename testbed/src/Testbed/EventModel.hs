-- | Compose testbed Features and configure their shared application runtime.
-- Run 'appEventModel' through 'Service.EventModel.run'.
module Testbed.EventModel (appEventModel) where

import Core
import Service.Application (ApiInfo (..))
import Service.EventModel qualified as EventModel
import Service.Transport.Web qualified as WebTransport
import Testbed.Cart.Feature (cartFlow)
import Testbed.Config (TestbedConfig)
import Testbed.Document.Feature (documentFlow)
import Testbed.Erasable.Feature (protectedFlow)
import Testbed.Erasable.Owned.Feature (ownedErasureFlow)
import Testbed.ExternalEffect.Feature (externalEffectFlow)
import Testbed.Infrastructure (makeFileUploadConfig, makePostgresConfig)
import Testbed.Stock.Feature (stockFlow)


appEventModel :: Text -> Int -> Result Text EventModel.EventModel
appEventModel httpHost httpPort = EventModel.eventModel do
  EventModel.appName "Testbed"
  EventModel.withConfig @TestbedConfig
  EventModel.withEventStore makePostgresConfig
  EventModel.withTransport WebTransport.server
  EventModel.withHost httpHost
  EventModel.withPort httpPort
  EventModel.withApiInfo @() (\_ -> testbedApiInfo)
  EventModel.withFileUpload makeFileUploadConfig
  EventModel.features do
    cartFlow
    stockFlow
    documentFlow
    externalEffectFlow
    protectedFlow
    ownedErasureFlow


testbedApiInfo :: ApiInfo
testbedApiInfo =
  ApiInfo
    { apiTitle = "Testbed API",
      apiVersion = "1.0.0",
      apiDescription = "Example NeoHaskell application demonstrating event sourcing, CQRS, and integrations"
    }
