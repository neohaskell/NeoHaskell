module Neo (main) where

import Core
import Neo.Build qualified as Build
import OptionsParser (OptionsParser)
import OptionsParser qualified


data Command
  = Build Build.Command


command :: OptionsParser Command
command =
  OptionsParser.commands
    [ ANON
        { name = "build",
          description = "Build the project",
          handler = do
            cmd <- Build.command
            OptionsParser.yield (Build cmd)
        }
    ]


main :: IO ()
main = do
  cmd <- OptionsParser.run command
  case cmd of
    Build cmd -> Build.commandHandler cmd
