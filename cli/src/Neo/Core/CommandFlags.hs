module Neo.Core.CommandFlags (
  CommandFlags (..),
  parser,
) where

import Command qualified
import Core


-- | CommandFlags represents the flags that can be passed to the command line
-- interface.
data CommandFlags = CommandFlags
  { projectFile :: Path
  }
  deriving (Show, Eq, Ord)


-- | `CommandFlags.parser` defines the command line flags that can be passed to
-- the command line interface.
--
-- In order to use it, return an action `Command.parse` with this value passed as
-- the argument.
parser :: Command.OptionsParser CommandFlags
parser = do
  projectFilePath <-
    Command.path
      Command.PathConfig
        { metavar = "PATH",
          short = 'c',
          help = "Path to the project configuration file",
          long = "projectConfig",
          value = Just [path|neo.json|]
        }
  pure (CommandFlags {projectFile = projectFilePath})
