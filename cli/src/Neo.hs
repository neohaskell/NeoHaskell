module Neo (main) where

import Action qualified
import Array qualified
import Command qualified
import Core
import Neo.Build qualified as Build
import Neo.Core.Event (Event (..))
import Service qualified


data State = State
  { build :: Build.State
  }
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


init :: (State, Action Event)
init = do
  let emptyState = State {build = Build.initialState}
  let action =
        Command.parse
          Command.CommandOptions
            { name = "neo",
              description = "NeoHaskell's console helper",
              version = Just [version|0.5.0|],
              decoder = commandParser
            }
  (emptyState, action)


update :: Event -> State -> (State, Action Event)
update event state =
  case event of
    Build buildEvent -> do
      let (newBuildState, buildAction) = Build.update buildEvent state.build
      let newState = state {build = newBuildState}
      let action = buildAction |> Action.map Build
      (newState, action)
    NoOp -> do
      let newState = state
      let action = Action.none
      (newState, action)


view :: State -> Text
view state =
  Build.view state.build


triggers :: Array (Trigger Event)
triggers = Array.empty


main :: IO ()
main =
  Service.run
    Service.UserApp
      { init = init,
        view = view,
        triggers = triggers,
        update = update
      }