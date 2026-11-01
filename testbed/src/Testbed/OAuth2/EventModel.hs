-- | Compose OAuth testbed Features and configure its shared runtime.
-- Run 'oauthEventModel' through 'Service.EventModel.run'.
module Testbed.OAuth2.EventModel (oauthEventModel) where

import Core
import Service.Application qualified as Application
import Service.EventModel qualified as EventModel
import Service.Transport.Web qualified as WebTransport
import Testbed.AccountConnection.Feature (accountConnectionFlow)
import Testbed.Config (TestbedConfig)
import Testbed.Erasable.Owned.Feature (ownedErasureFlow)
import Testbed.Erasable.Owned.ReplayGateStore (ReplayGateStoreConfig (..))
import Testbed.OAuth2.Configuration (makeOAuthPostgresConfig, oauthApiInfo, providerConfig)


oauthEventModel :: Int -> Int -> Bool -> Result Text EventModel.EventModel
oauthEventModel oauthPort fixturePort queueTimeoutMode = EventModel.eventModel do
  EventModel.appName "OAuth2 testbed"
  EventModel.withConfig @TestbedConfig
  EventModel.withEventStore makeOAuthPostgresConfig
  EventModel.withTransport WebTransport.server
  EventModel.withHost "127.0.0.1"
  EventModel.withPort oauthPort
  EventModel.withApiInfo @() (\_ -> oauthApiInfo)
  EventModel.withQueryObjectStore ReplayGateStoreConfig
  EventModel.withAuth @() (\_ -> [fmt|http://127.0.0.1:#{fixturePort}/oidc|])
  EventModel.withOAuth2StateKey "OAUTH2_TESTBED_STATE_KEY"
  EventModel.withOAuth2Provider @() (\_ -> providerConfig oauthPort fixturePort)
  configureDispatcher queueTimeoutMode
  EventModel.features do
    accountConnectionFlow
    ownedErasureFlow


configureDispatcher :: Bool -> EventModel.EventModelAction
configureDispatcher True =
  EventModel.withDispatcherConfig @()
    (\_ -> Application.defaultConfig {Application.workerChannelCapacity = 1, Application.channelWriteTimeoutMs = 250})
configureDispatcher False = pass
