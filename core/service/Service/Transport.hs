module Service.Transport (
  Transport (..),
  EndpointHandler,
  Endpoints (..),
) where

import Basics
import Bytes (Bytes)
import Json qualified
import Map (Map)
import Record qualified
import Service.Command.Core (Command, NameOf)
import Service.Response (CommandResponse)
import Task (Task)
import Text (Text)


-- | Handler for a single command endpoint.
--
-- Takes request bytes and a callback to send the response.
type EndpointHandler = Bytes -> ((CommandResponse, Bytes) -> Task Text Unit) -> Task Text Unit


-- | Collection of command endpoints for a transport.
data Endpoints transport = Endpoints
  { transport :: transport,
    commandEndpoints :: Map Text EndpointHandler
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
    (command -> Task Text CommandResponse) ->
    EndpointHandler
