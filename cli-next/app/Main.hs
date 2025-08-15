module Main where

import Core
import NeoSandbox qualified
import Task qualified


main :: IO ()
main = Task.runOrPanic NeoSandbox.run
