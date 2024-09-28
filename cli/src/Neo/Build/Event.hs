module Neo.Build.Event (
  Event (..),
  commandParser,
) where

import Command qualified
import Core
import Neo.Core.CommandFlags (CommandFlags)
import Neo.Core.CommandFlags qualified as CommandFlags
import Neo.Core.ProjectConfiguration (ProjectConfiguration)


-- | Represents one step of the build process
data Event
  = BuildStarted CommandFlags
  | ProjectFileNotFound
  | ProjectFileRead Text
  | FailedToParseProjectFile Text
  | ProjectFileParsed ProjectConfiguration
  | BuildDirectoryCreated Path
  | FailedToCreateBuildDirectory Path
  | NixBuildStarted
  deriving (Show, Eq, Ord)


-- Allows parsing a `BuildStarted` event from the command line.
-- This is useful for triggering the build process from the command line.
commandParser :: Command.OptionsParser Event
commandParser = do
  config <- CommandFlags.parser
  pure (BuildStarted config)