module Neo (
  Command(..),
  Event(..),
  State(..),
  update,
  handleCommand
) where

import Core

-- | ADT for commands that can be issued to the CLI.
data Command
  = ReadLine String
  deriving (Show, Eq)

-- | ADT for events that can be triggered by commands.
data Event
  = CodeSent String
  | ReplCommandTriggered String
  | NaturalLanguageSent String
  deriving (Show, Eq)

-- | Record for the application state.
data State = State
  { -- Add fields here as necessary
  }

-- | Function to update the state based on an event.
update :: Event -> State -> State
update event state =
  -- Implement state transitions based on events here
  state

-- | Function to handle commands and register events.
handleCommand :: Command -> State -> (State, [Event])
handleCommand command state =
  -- Implement command handling and event registration here
  (state, [])
