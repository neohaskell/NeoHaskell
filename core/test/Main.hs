module Main (main) where

import Core
import Test.DocTest


main :: IO ()
main =
  doctest
    [ "-isrc",
      "core/Array.hs"
    ]
