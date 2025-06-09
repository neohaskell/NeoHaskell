module Environment (
  getVariable,
) where

import Basics
import Result qualified
import System.Environment qualified
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


getVariable :: Text -> Task Text Text
getVariable key = Task.fromIOResult do
  let errorMsg = [fmt|Environment variable #{key} not found|]
  maybeRes <- System.Environment.lookupEnv (Text.toLinkedList key)
  maybeRes
    |> Result.fromMaybe errorMsg
    |> Result.map (\value -> value |> Text.fromLinkedList |> Text.trim)
    |> pure
