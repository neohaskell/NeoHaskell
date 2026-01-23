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
import Service.Transport.Web (WebTransport (..), AuthEnabled, OAuth2Config, FileUploadEnabled)
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
-- If 'maybeFileUpload' is provided, WebTransport will enable file upload/download routes.
-- Other transports ignore these parameters.
runTransports ::
  Map Text TransportValue ->
  Map Text (Map Text EndpointHandler) ->
  Map Text QueryEndpointHandler ->
  Maybe AuthEnabled ->
  -- ^ Optional authentication configuration for WebTransport
  Maybe OAuth2Config ->
  -- ^ Optional OAuth2 provider configuration for WebTransport
  Maybe FileUploadEnabled ->
  -- ^ Optional file upload configuration for WebTransport
  Task Text Unit
runTransports transportsMap endpointsByTransport queryEndpoints maybeWebAuth maybeOAuth2 maybeFileUpload = do
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

        -- Handle WebTransport specially to configure auth, OAuth2, and file uploads
        case transportName of
          "WebTransport" -> runWebTransport transportVal commandEndpointsForTransport queryEndpoints maybeWebAuth maybeOAuth2 maybeFileUpload
          _ -> runGenericTransport transportVal commandEndpointsForTransport queryEndpoints


-- | Run WebTransport with JWT authentication, OAuth2 provider routes, and file uploads.
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
  Maybe FileUploadEnabled ->
  Task Text Unit
runWebTransport transportVal commandEndpoints queryEndpoints maybeAuth maybeOAuth2 maybeFileUpload = do
  case transportVal of
    TransportValue transport -> do
      -- Cast the existentially-typed transport to WebTransport
      let baseWebTransport :: WebTransport = GhcUnsafe.unsafeCoerce transport
      -- Configure auth, OAuth2, and file uploads on the WebTransport itself
      let webTransportWithConfig = baseWebTransport
            { authEnabled = maybeAuth
            , oauth2Config = maybeOAuth2
            , fileUploadEnabled = maybeFileUpload
            }
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
