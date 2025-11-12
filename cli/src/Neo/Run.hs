module Neo.Run (
  handle,
  Error (..),
) where

import Array qualified
import Neo.Core
import Subprocess qualified
import Task qualified


data Error
  = NixFileError
  | CabalFileError
  | CustomError Text
  deriving (Show)


handle :: ProjectConfiguration -> Task Error Unit
handle config = do
  let projectName = config.name
  let rootFolder = [path|.|]
  completion <-
    Subprocess.openInherit [fmt|./result/bin/#{projectName}|] (Array.fromLinkedList []) rootFolder Subprocess.InheritBOTH
      |> Task.mapError (\_ -> CustomError "Failed to run the built application")
  if completion.exitCode != 0
    then errorOut completion.stderr
    else print completion.stdout


errorOut :: Text -> Task Error _
errorOut err =
  [fmt|Oops running failed:
    #{err}|]
    |> CustomError
    |> Task.throw
