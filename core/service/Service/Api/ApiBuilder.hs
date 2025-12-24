-- | DEPRECATED: Use Service.Transport instead.
--
-- This module is kept for backward compatibility during the transition.
module Service.Api.ApiBuilder (
  ApiBuilder (..),
  ApiEndpointHandler,
  ApiEndpoints (..),
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
type ApiEndpointHandler = Bytes -> ((CommandResponse, Bytes) -> Task Text Unit) -> Task Text Unit


-- | Collection of command endpoints for an API.
data ApiEndpoints api = ApiEndpoints
  { api :: api,
    commandEndpoints :: Map Text ApiEndpointHandler
  }


-- | API abstraction for different transport protocols.
--
-- DEPRECATED: Use 'Transport' from Service.Transport instead.
class ApiBuilder api where
  -- | The request type for this API.
  type Request api

  -- | The response type for this API.
  type Response api

  -- | The assembled, runnable representation of this API.
  type RunnableApi api

  -- | Assemble endpoint handlers into a runnable API.
  assembleApi ::
    ApiEndpoints api ->
    RunnableApi api

  -- | Run the assembled API.
  runApi :: api -> RunnableApi api -> Task Text Unit

  -- | Build an endpoint handler for a specific command type.
  buildCommandHandler ::
    forall command name.
    ( Command command,
      Json.FromJSON command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    api ->
    Record.Proxy command ->
    (command -> Task Text CommandResponse) ->
    ApiEndpointHandler
