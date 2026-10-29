module Main where

import Core
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
  Application.run (app oauthPort fixturePort)
