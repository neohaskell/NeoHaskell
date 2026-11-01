module Main where

import App (appEventModel)
import Core
import Service.EventModel qualified as EventModel
import System.IO qualified as GhcIO
import Task qualified


main :: IO ()
main = do
  GhcIO.hSetBuffering GhcIO.stdout GhcIO.LineBuffering
  GhcIO.hSetBuffering GhcIO.stderr GhcIO.LineBuffering
  let runApp = appEventModel |> EventModel.run
  runApp |> Task.runOrPanic
