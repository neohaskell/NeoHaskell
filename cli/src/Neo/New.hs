module Neo.New (
  handle,
  Error (..),
  ProjectName (..),
) where

import Array qualified
import Directory qualified
import File qualified
import Maybe qualified
import Neo.Core
import Neo.New.Templates.GitIgnore qualified as GitIgnore
import Neo.New.Templates.MainModule qualified as MainModule
import Neo.New.Templates.NeoJson qualified as NeoJson
import Path qualified
import Subprocess qualified
import Task qualified
import Text qualified


newtype ProjectName = ProjectName Text
  deriving (Show, Eq, Ord)


data Error
  = CabalFileError
  | CustomError Text
  deriving (Show)


openGit :: Array Text -> Path -> Task Error Unit
openGit arguments projectDir = do
  completion <-
    Subprocess.open "git" arguments projectDir
      |> Task.mapError (\err -> CustomError [fmt|Git command failed: #{err}|])
  if completion.exitCode != 0
    then Task.throw (CustomError completion.stderr)
    else Task.yield ()


initGit :: Path -> Task Error Unit
initGit projectDir = do
  let gitignoreFileName = [path|.gitignore|]
  let gitignoreFilePath =
        Array.fromLinkedList [projectDir, gitignoreFileName]
          |> Path.joinPaths

  openGit (Array.fromLinkedList ["init"]) projectDir

  File.writeText gitignoreFilePath GitIgnore.template
    |> Task.mapError (\_ -> CustomError "Could not write .gitignore file")

  openGit (Array.fromLinkedList ["add", "."]) projectDir

  openGit (Array.fromLinkedList ["commit", "-m", "Initial NeoHaskell project"]) projectDir


handle :: ProjectName -> Task Error Unit
handle (ProjectName projectName) = do
  let kebabName = Text.toKebabCase projectName
  let projectDir =
        kebabName
          |> Text.toLower
          |> Path.fromText
          |> Maybe.getOrDie -- TODO: Handle this better
  let srcDir =
        Array.fromLinkedList [projectDir, "src"]
          |> Path.joinPaths

  let moduleName =
        projectName
          |> Text.toPascalCase
  let moduleFileName =
        [fmt|#{moduleName}.hs|]
          |> Path.fromText
          |> Maybe.getOrDie -- TODO: Handle this better
  let moduleFilePath =
        Array.fromLinkedList [srcDir, moduleFileName]
          |> Path.joinPaths
  let configFileName = [path|neo.json|]
  let configFilePath =
        Array.fromLinkedList [projectDir, configFileName]
          |> Path.joinPaths

  Directory.create projectDir
    |> Task.mapError (\_ -> [fmt|Could not create directory #{Path.toText projectDir}|] |> CustomError)

  File.writeText configFilePath (NeoJson.template kebabName)
    |> Task.mapError (\_ -> CustomError "Could not write config file")

  Directory.create srcDir
    |> Task.mapError (\_ -> [fmt|Could not create directory #{Path.toText srcDir}|] |> CustomError)

  File.writeText moduleFilePath (MainModule.template moduleName)
    |> Task.mapError (\_ -> CustomError "Could not write module file")

  initGitResult <- initGit projectDir |> Task.errorAsResult |> Task.mapError never
  case initGitResult of
    Nothing -> Task.yield ()
    Just err ->
      print
        [fmt|Warning: Could not initialize git repository
#{toPrettyText err}
|]

  print
    [fmt|Created project #{projectName} at ./#{Path.toText projectDir}

To build your project:
  cd #{Path.toText projectDir}
  neo build

To run your project:
  cd #{Path.toText projectDir}
  neo run
|]
