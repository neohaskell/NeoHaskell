module Main where

import Core
import Environment qualified
import Service.EventModel qualified as EventModel
import Task qualified
import Testbed.Config (readPort)
import Testbed.EventModel (appEventModel, s3AppEventModel)
import Text qualified


main :: IO ()
main = run |> Task.runOrPanic


run :: Task Text Unit
run = do
  Environment.loadEnvFileIfPresent ".env" |> Task.ignoreError
  httpPort <- readPort "TESTBED_HTTP_PORT" 8080
  s3Endpoint <- Environment.getVariable "S3_ENDPOINT" |> Task.asResult
  let model = case s3Endpoint of
        Ok endpoint ->
          if Text.isEmpty endpoint
            then appEventModel httpPort
            else s3AppEventModel httpPort
        Err _ -> appEventModel httpPort
  EventModel.run model
