module Testbed.Service (
  service,
) where

import Core
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.ServiceDefinition.Core qualified as Service
import Service.SnapshotCache.InMemory (InMemorySnapshotCacheConfig (..))
import Service.Transport.Web qualified as WebTransport
import Testbed.Cart.Service qualified as CartService


service :: Service _ _ _ _ _
service =
  CartService.service
    |> Service.useServer WebTransport.server
    |> Service.useEventStore
      PostgresEventStore
        { user = "neohaskell",
          password = "neohaskell",
          host = "localhost",
          databaseName = "neohaskell",
          port = 5432
        }
    |> Service.useSnapshotCache InMemorySnapshotCacheConfig