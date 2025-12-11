module Service.Api.ApiBuilder (
  ApiBuilder (..),
  ApiEndpointHandler,
) where

import Basics
import Bytes (Bytes)
import Record qualified
import Service.Command.Core (Command, NameOf)
import Task (Task)
import Text (Text)


type ApiEndpointHandler = Bytes -> (Bytes -> Task Text Unit) -> Task Text Unit


class ApiBuilder api where
  buildCommandHandler ::
    forall command name.
    ( Command command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    api ->
    Record.Proxy command ->
    ApiEndpointHandler
