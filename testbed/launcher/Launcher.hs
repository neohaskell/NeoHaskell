module Main where

import Core
import Service.EventModel qualified as EventModel
import Task qualified
import Testbed.Config (readHost, readPort)
import Testbed.EventModel (appEventModel)


main :: IO ()
main = run |> Task.runOrPanic


run :: Task Text Unit
run = do
  httpHost <- readHost "TESTBED_HTTP_HOST" "127.0.0.1"
  httpPort <- readPort "TESTBED_HTTP_PORT" 8080
  EventModel.run (appEventModel httpHost httpPort)
