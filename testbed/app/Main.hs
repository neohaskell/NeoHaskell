module Main where

import Cart qualified
import Core
import Service qualified


main :: IO ()
main = do
  Service.run Cart.service
