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
import Maybe (Maybe (..))
import Maybe qualified
import Service.ServiceDefinition.Core (TransportValue (..))
import Service.Transport (Transport (..), QueryEndpointHandler, EndpointHandler, Endpoints (..))
import Service.Transport.Web (WebTransport (..), AuthEnabled, OAuth2Config)
import Task (Task)
import Task qualified
import Text (Text)
import Unsafe.Coerce qualified as GhcUnsafe


-- | Run all transports with their combined endpoints.
--
-- Each transport is started exactly once with all command and query endpoints
-- from all services that use that transport.
--
-- If 'maybeWebAuth' is provided, WebTransport will enforce JWT authentication.
-- If 'maybeOAuth2' is provided, WebTransport will enable OAuth2 provider routes.
-- Other transports ignore the auth and OAuth2 parameters.
runTransports ::
  Map Text TransportValue ->
  Map Text (Map Text EndpointHandler) ->
  Map Text QueryEndpointHandler ->
  Maybe AuthEnabled ->
  -- ^ Optional authentication configuration for WebTransport
  Maybe OAuth2Config ->
  -- ^ Optional OAuth2 provider configuration for WebTransport
  Task Text Unit
runTransports transportsMap endpointsByTransport queryEndpoints maybeWebAuth maybeOAuth2 = do
  transportsMap
    |> Map.entries
    |> Task.forEach \(transportName, transportVal) -> do
        let commandEndpointsForTransport =
              endpointsByTransport
                |> Map.get transportName
                |> Maybe.withDefault Map.empty

        let commandCount = Map.length commandEndpointsForTransport
        let sharedQueryCount = Map.length queryEndpoints
        let transportQueryCount = sharedQueryCount
        Console.print [fmt|Starting transport: #{transportName} with #{commandCount} commands and #{transportQueryCount} queries (shared query endpoints: #{sharedQueryCount} total)|]

        -- Handle WebTransport specially to configure auth and OAuth2 on the transport itself
        case transportName of
          "WebTransport" -> runWebTransport transportVal commandEndpointsForTransport queryEndpoints maybeWebAuth maybeOAuth2
          _ -> runGenericTransport transportVal commandEndpointsForTransport queryEndpoints


-- | Run WebTransport with JWT authentication and OAuth2 provider routes.
--
-- SAFETY: We use unsafeCoerce to cast the existentially-typed transport to WebTransport.
-- This is safe because:
-- 1. We guard on transportName == "WebTransport" before calling this function
-- 2. Transport names are derived from type names via TypeName, ensuring consistency
-- 3. WebTransport is the only transport that registers with name "WebTransport"
-- If the type system allowed Typeable on the existential, we'd use cast instead.
runWebTransport ::
  TransportValue ->
  Map Text EndpointHandler ->
  Map Text QueryEndpointHandler ->
  Maybe AuthEnabled ->
  Maybe OAuth2Config ->
  Task Text Unit
runWebTransport transportVal commandEndpoints queryEndpoints maybeAuth maybeOAuth2 = do
  case transportVal of
    TransportValue transport -> do
      -- Cast the existentially-typed transport to WebTransport
      let baseWebTransport :: WebTransport = GhcUnsafe.unsafeCoerce transport
      -- Configure auth and OAuth2 on the WebTransport itself
      let webTransportWithConfig = baseWebTransport {authEnabled = maybeAuth, oauth2Config = maybeOAuth2}
      -- Build endpoints with the configured transport
      let endpoints :: Endpoints WebTransport =
            Endpoints
              { transport = webTransportWithConfig,
                commandEndpoints = commandEndpoints,
                queryEndpoints = queryEndpoints
              }
      let runnableTransport = assembleTransport endpoints
      runTransport webTransportWithConfig runnableTransport


-- | Run a generic transport without auth.
runGenericTransport ::
  TransportValue ->
  Map Text EndpointHandler ->
  Map Text QueryEndpointHandler ->
  Task Text Unit
runGenericTransport transportVal commandEndpoints queryEndpoints = do
  case transportVal of
    TransportValue transport -> do
      let endpoints =
            Endpoints
              { transport = transport,
                commandEndpoints = commandEndpoints,
                queryEndpoints = queryEndpoints
              }
      let runnableTransport = assembleTransport endpoints
      runTransport transport runnableTransport
