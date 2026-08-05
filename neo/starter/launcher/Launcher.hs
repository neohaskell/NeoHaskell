module Main where

import App (app)
import Core
import Service.Application qualified as Application
import System.IO qualified as GhcIO
import Task qualified


main :: IO ()
main = do
  GhcIO.hSetBuffering GhcIO.stdout GhcIO.LineBuffering
  GhcIO.hSetBuffering GhcIO.stderr GhcIO.LineBuffering
  let runApp = app |> Application.run
  runApp |> Task.runOrPanic
