module Neo.State (
  State (..),
  init,
  update,
) where

import Action qualified
import Command qualified
import Core
import Neo.Build qualified as Build
import Neo.Event (Event (..))
import Neo.Event qualified as Event


-- | The 'State' data type represents the core state of the Neo application.
-- It contains a single field 'build' which holds the state of the build process.
--
-- The 'State' type derives 'Show', 'Eq', and 'Ord' to allow for easy
-- debugging, comparison, and ordering of state instances.
--
-- @build :: Build.State@ - The state of the build process.
data State = State
  { build :: Build.State
  }
  deriving (Show, Eq, Ord)


-- | Initializes the state and action for the NeoHaskell console helper.
--
-- This function sets up the initial state and parses the command options
-- for the NeoHaskell CLI.
--
-- @return A tuple containing the initial state and the parsed action.
init :: (State, Action Event)
init = do
  let initialState = State {build = Build.initialState}
  let action =
        Command.parse
          Command.CommandOptions
            { name = "neo",
              description = "NeoHaskell's console helper",
              version = Just [version|0.5.0|],
              decoder = Event.commandParser
            }
  (initialState, action)


-- | The 'update' function takes an 'Event' and a 'State', and returns a tuple
-- containing the updated 'State' and an 'Action' to be performed based on the
-- given 'Event'.
--
-- The function handles different types of events:
--
-- \* 'Build' event: Delegates handling to 'buildHandler' with the provided
--   'buildEvent' and current 'State'.
-- \* 'NoOp' event: Delegates handling to 'noOpHandler' with the current 'State'.
--
-- Arguments:
--
-- \* 'event' - The event to be processed.
-- \* 'state' - The current state before processing the event.
--
-- Returns:
--
-- \* A tuple containing the updated state and the action to be performed.
update :: Event -> State -> (State, Action Event)
update event state =
  case event of
    Build buildEvent ->
      buildHandler buildEvent state
    NoOp ->
      noOpHandler state


-- | Handles build events and updates the state accordingly.
--
-- This function takes a 'Build.Event' and the current 'State', and returns
-- a tuple containing the updated 'State' and an 'Action' to be performed.
--
-- The function works by updating the build state using the 'Build.update'
-- function, then creating a new state with the updated build state. It also
-- maps the resulting build action to an 'Action' using 'Action.map'.
--
-- @param buildEvent The build event to handle.
-- @param state The current state.
-- @return A tuple containing the updated state and the action to be performed.
buildHandler :: Build.Event -> State -> (State, Action Event)
buildHandler buildEvent state = do
  let (newBuildState, buildAction) = Build.update buildEvent state.build
  let newState = state {build = newBuildState}
  let action = buildAction |> Action.map Build
  (newState, action)


-- | A no-operation handler for the state.
--
-- This function takes the current state and returns it unchanged along with
-- a no-op action. It is useful as a default handler when no specific action
-- is required.
--
-- @param state The current state.
-- @return A tuple containing the unchanged state and a no-op action.
noOpHandler :: State -> (State, Action Event)
noOpHandler state = do
  let newState = state
  let action = Action.none
  (newState, action)
