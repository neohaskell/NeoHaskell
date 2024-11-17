module Neo.Build (
    handle,
    Error (..),
) where

import Array qualified
import File qualified
import Neo.Build.Templates.Cabal qualified as Cabal
import Neo.Build.Templates.Nix qualified as Nix
import Neo.Core
import Path qualified
import Subprocess qualified
import Task qualified


data Error
    = NixFileError
    | CabalFileError
    | Error Text
    deriving (Show)


handle :: ProjectConfiguration -> Task Error ()
handle config = do
    let rootFolder = [path|sandbox|]
    let nixPath = [path|devenv.nix|]
    let cabalPath = [path|example.cabal|]

    let nixFile = Nix.template config
    let cabalFile = Cabal.template config

    let nixFileName =
            Array.fromLinkedList [rootFolder, nixPath]
                |> Path.joinPaths
    let cabalFileName =
            Array.fromLinkedList [rootFolder, cabalPath]
                |> Path.joinPaths

    File.writeText nixFileName nixFile
        |> Task.mapError (\_ -> NixFileError)

    File.writeText cabalFileName cabalFile
        |> Task.mapError (\_ -> CabalFileError)

    completion <- Subprocess.open "cabal" (Array.fromLinkedList ["build"]) rootFolder
    if completion.exitCode != 0
        then errorOut completion.stderr
        else print completion.stdout


errorOut :: Text -> Task Error _
errorOut err =
    [fmt|Oops the build failed:
    {err}|]
        |> Error
        |> Task.throw