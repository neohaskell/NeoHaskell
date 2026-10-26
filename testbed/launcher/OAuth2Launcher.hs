module Main where

import Core
import OAuth2App (app)
import Service.Application qualified as Application
import Task qualified


main :: IO ()
main =
  Application.run app
    |> Task.runOrPanic
