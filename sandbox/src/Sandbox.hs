module Sandbox where

import qualified Console
import Core


run :: Task Text ()
run = do
  Console.print "Hello, NeoHaskell!"
