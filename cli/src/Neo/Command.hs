module Neo.Command (Command, decoder, handler) where

import Core
import Neo.Command.Build qualified as Build
import Neo.Services (Services)
import OptionsParser (OptionsParser)
import OptionsParser qualified


data Command
  = Build Build.Command


decoder :: OptionsParser Command
decoder =
  OptionsParser.commands
    [ ANON
        { name = "build",
          description = "Build the project",
          handler = do
            command <- Build.decoder
            pure (Build command)
        }
    ]


handler :: Services -> Command -> IO ()
handler services (Build command) = Build.handler services command