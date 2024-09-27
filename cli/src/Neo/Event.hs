module Neo.Event (
  Event (..),
  commandParser,
) where

import Array qualified
import Command qualified
import Core
import Neo.Build.Event qualified as Build


data Event
  = Build Build.Event
  | NoOp
  deriving (Show, Eq, Ord)


commandParser :: Command.OptionsParser Event
commandParser = do
  let build =
        Command.CommandOptions
          { name = "build",
            description = "build a file or directory",
            version = Nothing,
            decoder = buildParser
          }
  Command.commands
    (Array.fromLinkedList [build])


buildParser :: Command.OptionsParser Event
buildParser = do
  event <- Build.commandParser
  pure (Build event)
