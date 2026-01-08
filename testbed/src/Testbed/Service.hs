module Testbed.Service (
  service,
) where

import Core
import Service.ServiceDefinition.Core qualified as Service
import Service.SnapshotCache.InMemory (InMemorySnapshotCacheConfig (..))
import Service.Transport.Web qualified as WebTransport
import Testbed.Cart.Service qualified as CartService


service :: Service.Service _ _ _ _ _
service =
  CartService.service
    |> Service.useServer WebTransport.server
    |> Service.useSnapshotCache InMemorySnapshotCacheConfig