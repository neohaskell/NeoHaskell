module Service.Api.ApiBuilder (
  ApiBuilder (..),
  ApiEndpointHandler,
  ApiEndpoints (..),
) where

import Basics
import Bytes (Bytes)
import Map (Map)
import Record qualified
import Service.Command.Core (Command, NameOf)
import Task (Task)
import Text (Text)


type ApiEndpointHandler = Bytes -> (Bytes -> Task Text Unit) -> Task Text Unit


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
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    api ->
    Record.Proxy command ->
    ApiEndpointHandler
