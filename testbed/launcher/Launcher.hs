module Main where

import App (app)
import Core
import Service.Application qualified as Application
import Task qualified


main :: IO ()
main = do
  Application.run app
    |> Task.runOrPanic
