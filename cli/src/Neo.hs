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
  = ProjectFileRead ProjectDefinition
  | BuildStarted


init :: (Model, Command Message)
init = do
  let emptyModel = ANON {project = Nothing}
  let command =
        File.read "project.json"
          |> Command.map ProjectFileRead
  (emptyModel, command)


main :: IO ()
main = print "Hello world!"
