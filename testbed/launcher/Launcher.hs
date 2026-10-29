module Main where

import App (app)
import Core
import Testbed.Config (readPort)
import Service.Application qualified as Application
import Task qualified


main :: IO ()
main = run |> Task.runOrPanic


run :: Task Text Unit
run = do
  httpPort <- readPort "TESTBED_HTTP_PORT" 8080
  Application.run (app httpPort)
