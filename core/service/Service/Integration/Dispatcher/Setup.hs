{-# LANGUAGE Strict #-}

-- | Inputs required to construct one integration dispatcher.
--
-- Pass a 'Setup' to dispatcher construction with settings, stores, and runners.
module Service.Integration.Dispatcher.Setup (
  Setup (..),
) where

import Array (Array)
import Integration qualified
import Json qualified
import Map (Map)
import Service.EventStore.Core (EventStore)
import Service.Integration.Dispatcher.Config (DispatcherConfig)
import Service.Integration.Types (OutboundRunner, OutboundLifecycleRunner)
import Text (Text)


data Setup endpoint = Setup
  { settings :: DispatcherConfig,
    eventStore :: EventStore Json.Value,
    outboundRunners :: Array OutboundRunner,
    lifecycleRunners :: Array OutboundLifecycleRunner,
    commandEndpoints :: Map Text endpoint,
    actionContext :: Integration.ActionContext
  }
