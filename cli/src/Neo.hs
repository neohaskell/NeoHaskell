module Neo (
  Command (..),
  Event (..),
  State (..),
  update,
  handleCommand,
) where

import Core
import Neo.Repl qualified as Repl


-- | ADT for commands that can be issued to the CLI.
data Command
  = Repl Repl.Command


-- | ADT for events that can be triggered by commands.
data Event
  = ReplEvent Repl.Event


-- | Record for the application state.
data State = State
  { repl :: Repl.State
  }


-- Add fields here as necessary

-- | Function to update the state based on an event.
update :: Event -> State -> State
update event state =
  case event of
    ReplEvent replEvent -> do
      let
        repl =
          state.repl
            |> Repl.update replEvent
      state{repl}


-- | Function to handle commands using the configured services.
handleCommand :: services -> Command -> Promise Void
handleCommand _ _ =
  -- Implement command handling using the services here
  todo
