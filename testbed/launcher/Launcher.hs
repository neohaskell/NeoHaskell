module Main where

import Testbed.Service qualified
import Core
import Service qualified


main :: IO ()
main = do
  Service.__internal_runServiceMain Testbed.Service.service
