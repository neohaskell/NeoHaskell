{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Neo (main) where

import Command qualified
import Core
import File qualified
import Platform qualified
import Result qualified
import ToText (Show)
import Yaml qualified

type Model =
  Record
    '[ "project" := Maybe ProjectDefinition,
       "path" := Maybe Path,
       "status" := Text
     ]

type ProjectDefinition =
  Record
    '[ "name" := Text,
       "version" := Version
     ]

data Message
  = ProjectFileRead Text
  | ProjectFileAccessErrored File.Error
  | ProjectFileParsed ProjectDefinition
  | BuildStarted
  | BuildFailed FailureReason
  deriving (Show)

data FailureReason
  = ProjectFileParseError Text
  deriving (Show)

init :: (Model, Command Message)
init = do
  let emptyModel =
        ANON
          { project = Nothing,
            path = Nothing,
            status = "Starting up"
          }
  let command =
        File.readText
          ANON
            { path = [path|project.yaml|],
              onSuccess = ProjectFileRead,
              onError = ProjectFileAccessErrored
            }
  (emptyModel, command)

update :: Message -> Model -> (Model, Command Message)
update message model =
  case message of
    ProjectFileRead fileContent -> do
      let parsedContent = Yaml.parse fileContent
      let newModel = model {status = "Parsing project file"}
      case parsedContent of
        Result.Ok projectDefinition ->
          (newModel, Command.continueWith (ProjectFileParsed projectDefinition))
        Result.Err _ -> do
          let error = ProjectFileParseError fileContent
          (newModel, Command.continueWith (BuildFailed error))
    ProjectFileAccessErrored _ ->
      (model {status = "File Access Errored"}, Command.none)
    ProjectFileParsed projectDefinition ->
      (model {project = Just projectDefinition}, Command.none)
    BuildStarted ->
      (model {status = "Build Started"}, Command.none)
    BuildFailed _ ->
      (model {status = "Build Failed"}, Command.none)

view :: Model -> Text
view m =
  case m.project of
    Just project ->
      toText project
    Nothing ->
      m.status

main :: IO ()
main = Platform.init (ANON {init = init, view = view, update = update})
