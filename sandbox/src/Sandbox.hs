module Sandbox where

import Core
import Console qualified


run :: Task Text ()
run = do
    Console.print "Hello, NeoHaskell!"