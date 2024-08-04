{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Neo (main) where

import Action qualified
import Array qualified
import Core
import File qualified
import Platform qualified
import Result qualified
import ToText (Show)
import Trigger qualified
import Yaml qualified


type Model =
  Record
    '[ "project" := Maybe ProjectDefinition,
       "path" := Maybe Path,
       "count" := Int,
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
  | Tick
  | BuildFailed FailureReason
  deriving (Show)


data FailureReason
  = ProjectFileParseError Text
  deriving (Show)


init :: (Model, Action Message)
init = do
  let emptyModel =
        ANON
          { project = Nothing,
            path = Nothing,
            count = 0,
            status = "Starting up"
          }
  let action =
        File.readText
          ANON
            { path = [path|project.yaml|],
              onSuccess = ProjectFileRead,
              onError = ProjectFileAccessErrored
            }
  (emptyModel, action)


update :: Message -> Model -> (Model, Action Message)
update message model =
  case message of
    ProjectFileRead fileContent -> do
      let parsedContent = Yaml.parse fileContent
      let newModel = model {status = "Parsing project file"}
      case parsedContent of
        Result.Ok projectDefinition ->
          (newModel, Action.continueWith (ProjectFileParsed projectDefinition))
        Result.Err _ -> do
          let error = ProjectFileParseError fileContent
          (newModel, Action.continueWith (BuildFailed error))
    ProjectFileAccessErrored _ ->
      (model {status = "File Access Errored"}, Action.none)
    ProjectFileParsed projectDefinition ->
      (model {project = Just projectDefinition}, Action.none)
    BuildStarted ->
      (model {status = "Build Started!"}, Action.none)
    BuildFailed _ ->
      (model {status = "Build Failed!"}, Action.none)
    Tick ->
      ( model
          { count = model.count + 1,
            status = "Count: " ++ toText model.count
          },
        Action.none
      )


view :: Model -> Text
view m =
  case m.project of
    Just project ->
      m.status ++ "\n\n" ++ toText project
    Nothing ->
      m.status


main :: IO ()
main =
  Platform.init
    ( ANON
        { init = (init),
          view = (view),
          triggers =
            Array.fromLinkedList
              [ Trigger.everyMilliseconds 1000 (\_ -> Tick)
              ],
          update = (update)
        }
    )
