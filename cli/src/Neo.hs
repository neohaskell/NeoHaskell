module Neo (
  Command (..),
  Event (..),
  State (..),
  update,
  handleCommand,
) where

import Array qualified
import Core


-- | ADT for commands that can be issued to the CLI.
data Command
  = ReadLine Str


-- | ADT for events that can be triggered by commands.
data Event
  = CodeSent Str
  | ReplCommandTriggered Str
  | NaturalLanguageSent Str


-- | Record for the application state.
data State = State
  {
  }


-- Add fields here as necessary

-- | Function to update the state based on an event.
update :: Event -> State -> State
update event state =
  -- Implement state transitions based on events here
  state


-- | Function to handle commands using the configured services.
handleCommand :: services -> Command -> Promise Void
handleCommand services command =
  -- Implement command handling using the services here
  todo
