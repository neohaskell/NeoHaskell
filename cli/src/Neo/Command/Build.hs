module Neo.Command.Build (Command, decoder, handler) where

import Core
import Neo.Services (Services)
import OptionsParser (OptionsParser)


type Command = Unit


decoder :: OptionsParser Command
decoder = pure unit


handler :: Services -> Command -> IO ()
handler _ _ = do
  print "Running build command"