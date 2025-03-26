module Main where

import Core
import Neo qualified
import Task qualified


main :: IO ()
main = Task.runMain Neo.run
