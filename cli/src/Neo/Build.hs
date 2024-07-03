module Neo.Build (Command, command, commandHandler) where

import Core
import OptionsParser (OptionsParser)
import OptionsParser qualified


type Command =
  Record
    [ -- Whether to be verbose
      "verbose" := Bool,
      -- Only build the specified target
      "only" := Text
    ]


command :: OptionsParser Command
command = do
  verbose <-
    OptionsParser.flag
      ANON
        { long = "verbose",
          short = 'v',
          help = "Whether to be verbose"
        }

  only <-
    OptionsParser.text
      ANON
        { long = "only",
          metavar = "TARGET",
          help = "Only build the specified target",
          short = 'o'
        }

  pure (ANON {verbose = verbose, only = only})


commandHandler :: Command -> IO ()
commandHandler = dieWith "Not implemented yet!"