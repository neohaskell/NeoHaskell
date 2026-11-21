module Main where

import Core
import Environment qualified
import Service.EventModel qualified as EventModel
import Task qualified
import Testbed.Config (readHost, readPort)
import Testbed.EventModel (appEventModel, s3AppEventModel)
import Text qualified


main :: IO ()
main = run |> Task.runOrPanic


run :: Task Text Unit
run = do
  Environment.loadEnvFileIfPresent ".env" |> Task.ignoreError
  httpHost <- readHost "TESTBED_HTTP_HOST" "127.0.0.1"
  httpPort <- readPort "TESTBED_HTTP_PORT" 8080
  s3Endpoint <- Environment.getVariable "S3_ENDPOINT" |> Task.asResult
  let model = case s3Endpoint of
        Ok endpoint ->
          if Text.isEmpty endpoint
            then appEventModel httpHost httpPort
            else s3AppEventModel httpHost httpPort
        Err _ -> appEventModel httpHost httpPort
  EventModel.run model
