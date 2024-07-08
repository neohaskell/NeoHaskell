module Neo.Command (Command, decoder, handler) where

import Core
import Neo.Command.Build qualified as Build
import Neo.Services (Services)
import OptionsParser (OptionsParser)
import OptionsParser qualified


data Command
  = Build Build.Command


buildDecoder :: OptionsParser Command
buildDecoder = do
  command <- Build.decoder
  pure (Build command)


decoder :: OptionsParser Command
decoder =
  OptionsParser.commands
    [ ANON
        { name = "build",
          description = "Build the project",
          decoder = buildDecoder
        }
    ]


handler :: Services -> Command -> IO ()
handler services (Build command) = Build.handler services command