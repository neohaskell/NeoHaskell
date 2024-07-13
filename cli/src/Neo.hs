module Neo (main) where

import Core
import File qualified
import Path qualified


type Model =
  Record
    '[ "project" := Maybe ProjectDefinition
     ]


type ProjectDefinition =
  Record
    '[ "path" := Path Absolute Directory,
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
