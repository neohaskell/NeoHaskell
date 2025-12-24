module Testbed.Service (
  service,
) where

import Core
import Service.Api.WebApi qualified as WebApi
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.ServiceDefinition.Core qualified as Service
import Testbed.Cart.Service qualified as CartService


service :: Service _ _ _ _
service =
  CartService.service
    |> Service.useServer WebApi.server
    |> Service.useEventStore
      PostgresEventStore
        { user = "neohaskell",
          password = "neohaskell",
          host = "localhost",
          databaseName = "neohaskell",
          port = 5432
        }