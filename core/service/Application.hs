{- HLINT ignore "Use camelCase" -}

module Application (
  Application,
  new,
  add,
  __internal_runApplicationMain,
) where

import Basics
import GHC.IO qualified as GHC
import Json qualified
import Service.EventStore.Core (EventStoreConfig)
import Service.ServiceDefinition.Core (Service, ServiceEntityType, ServiceEventType)
import Service.ServiceDefinition.Core qualified as Service
import Service.SnapshotCache.Core (SnapshotCacheConfig)


-- | An Application is a container for services that can be run together.
-- For now, this is a simple passthrough to a single service.
data Application
  = EmptyApplication
  | forall cmds commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig.
    ( EventStoreConfig eventStoreConfig,
      SnapshotCacheConfig snapshotCacheConfig,
      ServiceEventType cmds ~ ServiceEventType cmds,
      ServiceEntityType cmds ~ ServiceEntityType cmds,
      Json.FromJSON (ServiceEventType cmds),
      Json.ToJSON (ServiceEventType cmds),
      Json.FromJSON (ServiceEntityType cmds),
      Json.ToJSON (ServiceEntityType cmds)
    ) =>
    ApplicationWithService
      (Service cmds commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig)


-- | Create a new empty application.
new :: Application
new = EmptyApplication


-- | Add a service to the application.
-- For now, this replaces any existing service (single service only).
add ::
  forall cmds commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig.
  ( EventStoreConfig eventStoreConfig,
    SnapshotCacheConfig snapshotCacheConfig,
    Json.FromJSON (ServiceEventType cmds),
    Json.ToJSON (ServiceEventType cmds),
    Json.FromJSON (ServiceEntityType cmds),
    Json.ToJSON (ServiceEntityType cmds)
  ) =>
  Service cmds commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig ->
  Application ->
  Application
add service _ = ApplicationWithService service


-- | Run the application. This is a passthrough to the underlying service's run function.
__internal_runApplicationMain :: Application -> GHC.IO Unit
__internal_runApplicationMain EmptyApplication =
  panic "Cannot run an empty application. Add at least one service using Application.add"
__internal_runApplicationMain (ApplicationWithService service) =
  Service.__internal_runServiceMain service
