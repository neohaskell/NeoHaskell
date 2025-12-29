module Main where

import Application qualified
import Core
import Testbed.Service qualified


main :: IO ()
main = do
  Application.new
    |> Application.add Testbed.Service.service
    |> Application.__internal_runApplicationMain
