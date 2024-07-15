module Neo (main) where

import Command qualified
import Core
import File qualified


type Model =
  Record
    '[ "project" := Maybe ProjectDefinition
     ]


type ProjectDefinition =
  Record
    '[ "path" := Path,
       "name" := Text,
       "version" := Version
     ]


data Message
  = ProjectFileAccessed (Result File.Error Text)
  | ProjectFileParsed ProjectDefinition
  | BuildStarted
  | BuildFailed FailureReason


data FailureReason
  = NoProjectFile
  | ProjectFileParseError Text


init :: (Model, Command Message)
init = do
  let emptyModel = ANON {project = Nothing}
  let command =
        File.read
          ANON
            { path = "project.json",
              expect = File.expectText ProjectFileAccessed
            }
  (emptyModel, command)


update :: Message -> Model -> (Model, Command Message)
update message model =
  case message of
    ProjectFileAccessed (Ok fileContent) -> do
      let parsedContent = Yaml.parse fileContent
      case parsedContent of
        Ok projectDefinition ->
          model
            |> update (ProjectFileParsed projectDefinition)
        Err _ ->
          model
            |> update (BuildFailed ProjectFileParseError fileContent)
    ProjectFileAccessed (Err _) ->
      (model, Command.none)
    ProjectFileParsed projectDefinition ->
      (model {project = Just projectDefinition}, Command.none)
    BuildStarted ->
      (model, Command.none)
    BuildFailed _ ->
      (model, Command.none)


main :: IO ()
main = print "Hello world!"
