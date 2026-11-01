module Main where

import Core
import Service.EventModel qualified as EventModel
import Task qualified
import Testbed.Config (readPort)
import Testbed.EventModel (appEventModel)


main :: IO ()
main = run |> Task.runOrPanic


run :: Task Text Unit
run = do
  httpPort <- readPort "TESTBED_HTTP_PORT" 8080
  EventModel.run (appEventModel httpPort)
