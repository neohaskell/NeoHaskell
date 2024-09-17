module Neo.Build (
  Event (..),
  State (..),
  commandParser,
  initialState,
  update,
  view,
) where

import Action qualified
import Array (Array)
import Array qualified
import Command qualified
import Core
import File qualified
import Text qualified


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
  { messages :: Array Text,
    config :: Maybe BuildConfig
  }
  deriving (Show, Eq, Ord)


initialState :: State
initialState =
  State
    { messages = Array.empty,
      config = Nothing
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
      let newMessages = state.messages |> Array.push "Reading project file"
      let newState = state {config = Just config, messages = newMessages}
      (newState, File.readText opts |> Action.map handleRes)
    ReadProjectFile text -> do
      let newMessages = state.messages |> Array.push text
      let newState = state {messages = newMessages}
      (newState, Action.none)
    ProjectFileNotFound -> do
      let newMessages =
            state.messages
              |> Array.push "Project file not found"
              |> Array.push "Press CTRL+C to exit"
      let newState = state {messages = newMessages}
      (newState, Action.none)


view :: State -> View
view state = do
  let configText = toText state.config
  let messagesText =
        state.messages
          |> Text.joinWith "\n\n"
  [fmt|
=============
Config:
  {configText}
=============


{messagesText}
  |]