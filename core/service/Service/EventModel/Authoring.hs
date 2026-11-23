{-# LANGUAGE UndecidableInstances #-}

-- | Compose an application from configuration, stores, transports and Features.
module Service.EventModel.Authoring (
  module Model,
  withEventStore,
  inMemory,
  jsonLines,
) where

import Service.EventModel as Model (
  EventModel,
  EventModelAction,
  FeatureAction,
  eventModel,
  appName,
  withConfig,
  withIntegration,
  withSecretStore,
  withAuthOverrides,
  withCors,
  withHealthCheck,
  withoutHealthCheck,
  useReadinessEndpoint,
  withQueryObjectStore,
  withTransport,
  withHost,
  withPort,
  withPortFromConfig,
  withApiInfo,
  withFileUpload,
  withAuth,
  withOAuth2StateKey,
  withOAuth2Provider,
  withDispatcherConfig,
  outboundLifecycle,
  features,
 )
import Service.EventModel qualified as EventModel
import Service.EventStore (EventStoreConfig)
import Service.EventStore.Simple (inMemory, jsonLines)
import Type.Reflection (Typeable)


-- | Select a ready-to-use store or a factory that uses application configuration.
class EventStoreSelection selection where
  withEventStore :: selection -> EventModelAction


instance {-# OVERLAPPABLE #-} (EventStoreConfig store) => EventStoreSelection store where
  withEventStore store = EventModel.withEventStore @() (\_ -> store)


instance (Typeable config, EventStoreConfig store) => EventStoreSelection (config -> store) where
  withEventStore = EventModel.withEventStore
