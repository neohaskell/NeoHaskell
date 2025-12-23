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
import Service.CommandResponse (CommandResponse)
import Task (Task)
import Text (Text)


type ApiEndpointHandler = Bytes -> ((CommandResponse, Bytes) -> Task Text Unit) -> Task Text Unit


data ApiEndpoints api = ApiEndpoints
  { api :: api,
    commandEndpoints :: Map Text ApiEndpointHandler
  }


class ApiBuilder api where
  type Request api
  type Response api
  type RunnableApi api


  assembleApi ::
    ApiEndpoints api ->
    RunnableApi api


  runApi :: api -> RunnableApi api -> Task Text Unit


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
