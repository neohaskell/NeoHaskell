module Neo.Build.State (
  State (..),
  initialState,
  update,
) where

import Action qualified
import Array qualified
import Core
import Directory qualified
import File qualified
import Json qualified
import Neo.Build.Event (Event)
import Neo.Build.Event qualified as Event
import Neo.Core.CommandFlags (CommandFlags (..))
import Neo.Core.ProjectConfiguration (ProjectConfiguration)


-- | Represents the state of the build
data State = State
  { messages :: Array Text,
    config :: Maybe CommandFlags
  }
  deriving (Show, Eq, Ord)


-- | The initial state of the build
initialState :: State
initialState =
  State
    { messages = Array.empty,
      config = Nothing
    }


-- | `update` is the function that updates the state of the build based on the event
-- that is passed to it. It is a dispatcher function that delegates the event to the
-- appropriate handler function.
update :: Event -> State -> (State, Action Event)
update event state =
  case event of
    Event.BuildStarted config ->
      buildStarted config state
    Event.ProjectFileRead text ->
      projectFileRead text state
    Event.ProjectFileNotFound ->
      projectFileNotFound state
    Event.FailedToParseProjectFile text ->
      failedToParseProjectFile text state
    Event.ProjectFileParsed config ->
      projectFileParsed config state
    Event.BuildDirectoryCreated dirPath ->
      buildDirectoryCreated dirPath state
    Event.FailedToCreateBuildDirectory dirPath ->
      failedToCreateBuildDirectory dirPath state
    Event.NixBuildStarted ->
      nixBuildStarted state


buildStarted :: CommandFlags -> State -> (State, Action Event)
buildStarted config state = do
  let handleRes res = case res of
        Ok text -> Event.ProjectFileRead text
        Err _ -> Event.ProjectFileNotFound
  let opts =
        File.ReadOptions
          { path = config.projectFile
          }
  let newState =
        state
          |> pushMessage "Reading project file"
  let nextAction = File.readText opts |> Action.map handleRes
  (newState, nextAction)


projectFileRead :: Text -> State -> (State, Action Event)
projectFileRead text state = do
  case Json.decodeText text of
    Ok config -> do
      let newState =
            state
              |> pushMessage "Project file parsed"
      let newEvent = Event.ProjectFileParsed config
      update newEvent newState
    Err err -> do
      let newState =
            state
              |> pushMessage [fmt|Failed to parse project file: {err} |]
      let newEvent = Event.FailedToParseProjectFile text
      update newEvent newState


projectFileNotFound :: State -> (State, Action Event)
projectFileNotFound state = do
  let newState =
        state
          |> pushMessage "Project file not found"
  (newState, Action.none)


failedToParseProjectFile :: Text -> State -> (State, Action Event)
failedToParseProjectFile text state = do
  let newState =
        state
          |> pushMessage [fmt|Failed to parse project file: {text}|]
  (newState, Action.none)


projectFileParsed :: ProjectConfiguration -> State -> (State, Action Event)
projectFileParsed config state = do
  let buildDir = [path|.neo|]
  let handleRes result = case result of
        Ok _ -> Event.BuildDirectoryCreated buildDir
        Err _ -> Event.FailedToCreateBuildDirectory buildDir
  let newState =
        state
          |> pushMessage [fmt|Project file parsed: {toText config}|]
  ( newState,
    Directory.create
      ( Directory.CreateOptions
          { path = buildDir
          }
      )
      |> Action.map handleRes
    )


buildDirectoryCreated :: Path -> State -> (State, Action Event)
buildDirectoryCreated dirPath state = do
  let newState =
        state
          |> pushMessage [fmt|Build directory created: {toText dirPath}|]
  (newState, Action.none)


failedToCreateBuildDirectory :: Path -> State -> (State, Action Event)
failedToCreateBuildDirectory dirPath state = do
  let newState =
        state
          |> pushMessage [fmt|Failed to create build directory: {toText dirPath}|]
  (newState, Action.none)


nixBuildStarted :: State -> (State, Action Event)
nixBuildStarted = dieWith "not implemented"


-- PRIVATE --

pushMessage :: Text -> State -> State
pushMessage message state =
  state {messages = state.messages |> Array.push message}