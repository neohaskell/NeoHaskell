module Main where

import App (appTask)
import Core
import Service.Application qualified as Application
import Task qualified


main :: IO ()
main = do
  let runApp = do
        application <- appTask
        Application.run application
  runApp |> Task.runOrPanic
