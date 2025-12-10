module Main where

import Core
import Service qualified
import Testbed.Service qualified


main :: IO ()
main = do
  Service.__internal_runServiceMain Testbed.Service.service
