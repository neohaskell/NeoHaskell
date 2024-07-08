module Neo.Command.Build (Command, decoder, handler) where

import Core
import Neo.Services (Services)
import OptionsParser (OptionsParser)


type Command = Unit


decoder :: OptionsParser Command
decoder = pure unit


handler :: Services -> Command -> IO ()
handler services _ = do
  print "Running build command"
  services . events . register Event.BuildTriggered