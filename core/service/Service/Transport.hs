module Service.Transport (
  Transport (..),
  EndpointHandler,
  QueryEndpointHandler,
  Endpoints (..),
) where

import Basics
import Bytes (Bytes)
import Json qualified
import Map (Map)
import Maybe (Maybe)
import Record qualified
import Service.Auth (RequestContext, UserClaims)
import Service.Command.Core (Command, NameOf)
import Service.Query.Auth (QueryEndpointError)
import Service.Response (CommandResponse)
import Task (Task)
import Text (Text)


-- | Handler for a single command endpoint.
--
-- Takes RequestContext, request bytes, and a callback to send the response.
-- The RequestContext provides authentication context for authorization decisions.
type EndpointHandler = RequestContext -> Bytes -> ((CommandResponse, Bytes) -> Task Text Unit) -> Task Text Unit


-- | Handler for a single query endpoint.
--
-- Takes Maybe UserClaims for authorization and returns query data as JSON Text.
-- The handler is responsible for:
-- 1. Calling canAccessImpl (pre-fetch auth check)
-- 2. Fetching the data
-- 3. Calling canViewImpl on each result (post-fetch auth filter)
--
-- Returns typed QueryEndpointError for proper HTTP status mapping:
-- - AuthorizationError Unauthenticated -> 401
-- - AuthorizationError Forbidden -> 403
-- - AuthorizationError InsufficientPermissions -> 403
-- - StorageError -> 500
--
-- Used for GET /queries/{query-name} endpoints.
type QueryEndpointHandler = Maybe UserClaims -> Task QueryEndpointError Text


-- | Collection of command and query endpoints for a transport.
-- Groups command handlers and query handlers that share the same transport configuration.
data Endpoints transport = Endpoints
  { transport :: transport,
    commandEndpoints :: Map Text EndpointHandler,
    queryEndpoints :: Map Text QueryEndpointHandler
  }


-- | Transport abstraction for different API protocols.
--
-- A Transport defines how to:
-- * Build command handlers that parse requests and serialize responses
-- * Assemble handlers into a runnable server
-- * Run the assembled server
class Transport transport where
  -- | The request type for this transport.
  type Request transport

  -- | The response type for this transport.
  type Response transport

  -- | The assembled, runnable representation of this transport.
  type RunnableTransport transport

  -- | Assemble endpoint handlers into a runnable transport.
  assembleTransport ::
    Endpoints transport ->
    RunnableTransport transport

  -- | Run the assembled transport.
  runTransport :: transport -> RunnableTransport transport -> Task Text Unit

  -- | Build an endpoint handler for a specific command type.
  --
  -- This handles:
  -- * Deserializing the request into the command type
  -- * Passing RequestContext to the domain handler
  -- * Calling the domain handler
  -- * Serializing the response
  buildHandler ::
    forall command name.
    ( Command command,
      Json.FromJSON command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    transport ->
    Record.Proxy command ->
    (RequestContext -> command -> Task Text CommandResponse) ->
    EndpointHandler
