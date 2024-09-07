module Neo.Build (
  Event,
  BuildEvent (..),
  State (..),
  commandParser,
  initialState,
  update,
  view,
) where

import Action qualified
import Command qualified
import Core
import File qualified


type Event = BuildEvent


data BuildEvent
  = BuildStarted
  | ProjectFileNotFound
  | ReadProjectFile Text
  deriving (Show, Eq, Ord)


commandParser :: Command.OptionsParser BuildEvent
commandParser = do
  pure BuildStarted


data State = State
  { message :: Text
  }
  deriving (Show, Eq, Ord)


initialState :: State
initialState =
  State
    { message = "Build Started"
    }


update :: BuildEvent -> State -> (State, Action BuildEvent)
update event state =
  case event of
    BuildStarted -> do
      let opts =
            File.ReadOptions
              { path = "foo.txt",
                onSuccess = \txt -> ReadProjectFile txt,
                onError = \_ -> ProjectFileNotFound
              }
      (state, File.readText opts)
    ReadProjectFile text -> do
      let newState = state {message = text}
      (newState, Action.none)
    ProjectFileNotFound -> do
      let newState = state {message = "Project file not found"}
      (newState, Action.none)


view :: State -> View
view = dieWith "Not implemented yet"