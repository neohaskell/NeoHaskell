module Main where

import Testbed.Cart qualified as Cart
import Core
import Service qualified


main :: IO ()
main = do
  Service.run Cart.service
