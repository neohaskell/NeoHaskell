module Main where

import Core
import Environment qualified
import Service.EventModel qualified as EventModel
import Task qualified
import Testbed.Config (readPort)
import Testbed.OAuth2.EventModel (oauthEventModel)


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
  EventModel.run (oauthEventModel oauthPort fixturePort queueTimeoutMode)
