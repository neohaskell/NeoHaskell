-- | Compose testbed Features and configure their shared application runtime.
-- Run 'appEventModel' through 'Service.EventModel.Launcher.run'.
module Testbed.EventModel (appEventModel) where

import Core
import Result qualified
import Service.Application (ApiInfo (..))
import Service.EventModel qualified as EventModel
import Service.Transport.Web qualified as WebTransport
import Set qualified as Set
import Testbed.Cart.Feature (cartFlow)
import Testbed.Config (TestbedConfig)
import Testbed.Document.Feature (documentFlow)
import Testbed.Erasable.Feature (protectedFlow)
import Testbed.Erasable.Owned.Feature (ownedErasureFlow)
import Testbed.ExternalEffect.Feature (externalEffectFlow)
import Testbed.Infrastructure (makeFileUploadConfig, makePostgresConfig)
import Testbed.Meter (meterFlow)
import Testbed.RequestOrigin (requestOriginFlow)
import Testbed.Stock.Feature (stockFlow)


appEventModel :: Text -> Int -> EventModel
appEventModel httpHost httpPort = EventModel.eventModel do
  EventModel.appName "Testbed"
  EventModel.withConfig @TestbedConfig
  EventModel.withEventStore makePostgresConfig
  EventModel.withTransport
    ( WebTransport.server
        { WebTransport.trustedProxyAddresses = Set.singleton testbedTrustedProxyAddress
        }
    )
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
    requestOriginFlow
    meterFlow


testbedApiInfo :: ApiInfo
testbedApiInfo =
  ApiInfo
    { apiTitle = "Testbed API",
      apiVersion = "1.0.0",
      apiDescription = "Example NeoHaskell application demonstrating event sourcing, CQRS, and integrations"
    }


testbedTrustedProxyAddress :: WebTransport.TrustedProxyAddress
testbedTrustedProxyAddress =
  case WebTransport.trustedProxyAddressFromText "127.0.0.1" of
    Result.Ok address -> address
    Result.Err _ -> panic "Invalid testbed trusted proxy address"
