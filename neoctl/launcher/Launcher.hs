{-| Run neoctl's Event Model and translate typed failures to exit code 2.
The launcher applies the default log level before application startup.
-}
module Main where

import App (appEventModel)
import Core
import IO qualified
import LinkedList qualified
import Service.EventModel qualified as EventModel
import System.Environment qualified as GhcEnvironment
import System.Exit qualified as GhcExit
import System.IO qualified as GhcIO
import Task qualified
import Text qualified


main :: IO Unit
main = do
  GhcIO.hSetBuffering GhcIO.stdout GhcIO.LineBuffering
  GhcIO.hSetBuffering GhcIO.stderr GhcIO.LineBuffering
  configuredLogLevel <- GhcEnvironment.lookupEnv "LOG_LEVEL"
  case configuredLogLevel of
    Nothing -> GhcEnvironment.setEnv "LOG_LEVEL" "Error"
    Just _ -> IO.yield ()
  rawArguments <- GhcEnvironment.getArgs
  let arguments = rawArguments |> LinkedList.map Text.fromLinkedList
  let runApp = appEventModel arguments |> EventModel.run
  outcome <- runApp |> Task.runResult
  case outcome of
    Ok _ -> IO.yield ()
    Err message -> failWith message


failWith :: Text -> IO Unit
failWith message = do
  GhcIO.hPutStrLn GhcIO.stderr (Text.toLinkedList message)
  GhcExit.exitWith (GhcExit.ExitFailure 2)
