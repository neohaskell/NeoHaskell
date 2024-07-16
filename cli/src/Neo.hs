module Neo (main) where

import Command qualified
import Core
import File qualified
import Result qualified
import Yaml qualified


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
  = ProjectFileRead Text
  | ProjectFileAccessErrored File.Error
  | ProjectFileParsed ProjectDefinition
  | BuildStarted
  | BuildFailed FailureReason


data FailureReason
  = ProjectFileParseError Text


init :: (Model, Command Message)
init = do
  let emptyModel = ANON {project = Nothing}
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
      case parsedContent of
        Result.Ok projectDefinition ->
          model
            |> update (ProjectFileParsed projectDefinition)
        Result.Err _ -> do
          let error = ProjectFileParseError fileContent
          update (BuildFailed error) model
    ProjectFileAccessErrored _ ->
      (model, Command.none)
    ProjectFileParsed projectDefinition ->
      (model {project = Just projectDefinition}, Command.none)
    BuildStarted ->
      (model, Command.none)
    BuildFailed _ ->
      (model, Command.none)


view :: Model -> Html
view _ =
  [html|
  <div>
    Hello World!
  </div>
|]


main :: IO ()
main = do
  -- TODO: Implement the loop in the platform module
  let (model, _) = init
  let (newModel, _) = update BuildStarted model
  let viewHtml = view newModel
  viewHtml
    |> toText
    |> print
