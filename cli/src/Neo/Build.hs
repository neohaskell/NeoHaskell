module Neo.Build
  ( handle,
    Error (..),
  )
where

import Array qualified
import File qualified
import Neo.Build.Templates.Cabal qualified as Cabal
import Neo.Build.Templates.Nix qualified as Nix
import Neo.Core
import Subprocess qualified
import Task qualified

data Error
  = NixFileError
  | CabalFileError
  | Error Text
  deriving (Show)

handle :: ProjectConfiguration -> Task Error ()
handle config = do
  let rootFolder = [path|.|]
  let nixFileName = [path|default.nix|]
  -- FIXME: Read the project name from config instead
  let cabalFileName = [path|example.cabal|]

  let nixFile = Nix.template config
  let cabalFile = Cabal.template config

  File.writeText nixFileName nixFile
    |> Task.mapError (\_ -> NixFileError)

  File.writeText cabalFileName cabalFile
    |> Task.mapError (\_ -> CabalFileError)

  -- FIXME: Create another thread that renders the output of the build via streaming.
  -- As right now there's no output at all
  completion <- Subprocess.open "nix-build" (Array.fromLinkedList []) rootFolder
  -- completion <- Subprocess.open "nix-build" (Array.fromLinkedList ["-E", nixFile]) rootFolder
  if completion.exitCode != 0
    then errorOut completion.stderr
    else print completion.stdout

errorOut :: Text -> Task Error _
errorOut err =
  [fmt|Oops the build failed:
    {err}|]
    |> Error
    |> Task.throw
