{-| Start the local HTTP service or print neoctl usage and version information.
Service startup failures and invalid arguments exit with code 2.
-}
module Main where

import App (appEventModel)
import Core
import IO qualified
import Paths_neoctl qualified
import Service.EventModel qualified as EventModel
import System.Environment qualified as GhcEnvironment
import System.Exit qualified as GhcExit
import System.IO qualified as GhcIO
import Task qualified
import Text qualified
import Version qualified


main :: IO Unit
main = do
  GhcIO.hSetBuffering GhcIO.stdout GhcIO.LineBuffering
  GhcIO.hSetBuffering GhcIO.stderr GhcIO.LineBuffering
  arguments <- GhcEnvironment.getArgs
  case arguments of
    [] -> runService
    ["--help"] -> showUsage
    ["-h"] -> showUsage
    ["--version"] ->
      GhcIO.putStrLn (Text.toLinkedList [fmt|neoctl #{Version.toText Paths_neoctl.version}|])
    _ -> failWith "Invalid arguments. Run neoctl --help for usage."


showUsage :: IO Unit
showUsage = do
  GhcIO.putStrLn "Usage: neoctl [--help|-h|--version]"
  GhcIO.putStrLn "Run the NeoHaskell control service at http://127.0.0.1:8080."


runService :: IO Unit
runService = do
  configuredLogLevel <- GhcEnvironment.lookupEnv "LOG_LEVEL"
  case configuredLogLevel of
    Nothing -> GhcEnvironment.setEnv "LOG_LEVEL" "Error"
    Just _ -> IO.yield ()
  let runApp = appEventModel |> EventModel.run
  outcome <- runApp |> Task.runResult
  case outcome of
    Ok _ -> IO.yield ()
    Err message -> failWith message


failWith :: Text -> IO Unit
failWith message = do
  GhcIO.hPutStrLn GhcIO.stderr (Text.toLinkedList message)
  GhcExit.exitWith (GhcExit.ExitFailure 2)
