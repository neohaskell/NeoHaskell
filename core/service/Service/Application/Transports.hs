-- | # Service.Application.Transports
--
-- Transport runtime for the Application.
--
-- This module handles running transports with their combined endpoints.
module Service.Application.Transports (
  -- * Runtime
  runTransports,
) where

import Basics
import Console qualified
import Map (Map)
import Map qualified
import Maybe qualified
import Service.ServiceDefinition.Core (TransportValue (..))
import Service.Transport (Transport (..), QueryEndpointHandler, EndpointHandler, Endpoints (..))
import Task (Task)
import Task qualified
import Text (Text)


-- | Run all transports with their combined endpoints.
--
-- Each transport is started exactly once with all command and query endpoints
-- from all services that use that transport.
runTransports ::
  Map Text TransportValue ->
  Map Text (Map Text EndpointHandler) ->
  Map Text QueryEndpointHandler ->
  Task Text Unit
runTransports transportsMap endpointsByTransport queryEndpoints = do
  transportsMap
    |> Map.entries
    |> Task.forEach \(transportName, transportVal) -> do
        let commandEndpointsForTransport =
              endpointsByTransport
                |> Map.get transportName
                |> Maybe.withDefault Map.empty

        let commandCount = Map.length commandEndpointsForTransport
        let queryCount = Map.length queryEndpoints
        Console.print [fmt|Starting transport: #{transportName} with #{commandCount} commands and #{queryCount} queries|]

        case transportVal of
          TransportValue transport -> do
            let endpoints =
                  Endpoints
                    { transport = transport
                    , commandEndpoints = commandEndpointsForTransport
                    , queryEndpoints = queryEndpoints
                    }
            let runnableTransport = assembleTransport endpoints
            runTransport transport runnableTransport
