module Main where

import Core
import Environment qualified
import OAuth2App (app)
import Service.Application qualified as Application
import Task qualified
import Testbed.Config (readPort)


main :: IO ()
main = run |> Task.runOrPanic


run :: Task Text Unit
run = do
  oauthPort <- readPort "TESTBED_OAUTH_PORT" 8081
  fixturePort <- readPort "TESTBED_FIXTURE_PORT" 8181
  timeoutMode <- Environment.getVariable "TESTBED_ERASURE_QUEUE_TIMEOUT" |> Task.asResult
  let queueTimeoutMode = case timeoutMode of
        Ok "1" -> True
        _ -> False
  Application.run (app oauthPort fixturePort queueTimeoutMode)
