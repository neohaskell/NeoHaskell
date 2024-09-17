module Neo.Build (
  Event (..),
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


data BuildConfig = BuildConfig
  { projectFile :: Path
  }
  deriving (Show, Eq, Ord)


data Event
  = BuildStarted BuildConfig
  | ProjectFileNotFound
  | ReadProjectFile Text
  deriving (Show, Eq, Ord)


configParser :: Command.OptionsParser BuildConfig
configParser = do
  projectFilePath <-
    Command.path
      Command.PathConfig
        { metavar = "PATH",
          short = 'c',
          help = "Path to the project configuration file",
          long = "projectConfig",
          value = Just [path|neo.json|]
        }
  pure (BuildConfig {projectFile = projectFilePath})


commandParser :: Command.OptionsParser Event
commandParser = do
  config <- configParser
  pure (BuildStarted config)


data State = State
  { message :: Text
  }
  deriving (Show, Eq, Ord)


initialState :: State
initialState =
  State
    { message = "Build Started"
    }


update :: Event -> State -> (State, Action Event)
update event state =
  case event of
    BuildStarted config -> do
      let handleRes res = case res of
            Ok text -> ReadProjectFile text
            Err _ -> ProjectFileNotFound
      let opts =
            File.ReadOptions
              { path = config.projectFile
              }
      (state, File.readText opts |> Action.map handleRes)
    ReadProjectFile text -> do
      let newState = state {message = text}
      (newState, Action.none)
    ProjectFileNotFound -> do
      let newState = state {message = "Project file not found"}
      (newState, Action.none)


view :: State -> View
view = dieWith "Not implemented yet"