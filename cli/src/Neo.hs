{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Neo (
  run,
) where

import Array qualified
import Command qualified
import Console qualified
import Core
import File qualified
import Json qualified
import Neo.Build qualified as Build
import Neo.New qualified as New
import Neo.Run qualified as Run
import Task qualified
import Text qualified


data CommonFlags = CommonFlags
  { projectFile :: Path
  }
  deriving (Show, Eq, Ord)


data NeoCommand
  = Build CommonFlags
  | Run CommonFlags
  | New New.ProjectName
  deriving (Show, Eq, Ord)


run :: Task Text Unit
run = do
  let message1 = "⚠️ NEOHASKELL IS IN ITS EARLY DAYS ⚠️"
  let message2 = "HERE BE DRAGONS, BEWARE!"
  let msgLength = Text.length message1 + 4
  let paddedMessage1 = message1 |> Text.pad msgLength ' '
  let paddedMessage2 = message2 |> Text.pad msgLength ' '
  Console.print ""
  Console.print ""
  Console.print paddedMessage1
  Console.print paddedMessage2
  Console.print ""
  Console.print ""
  let parser =
        Command.CommandOptions
          { name = "neo",
            description = "NeoHaskell's console helper",
            version = Just [Core.version|0.8.2|],
            decoder = commandsParser
          }
  cmd <- Command.parseHandler parser
  handleCommand cmd
    |> Task.mapError
      \e ->
        [fmt|
Neo: While running your command, I've encountered an error:
     #{errorToText e}

     It looks like your last command failed. Remember, if it is taking
     you more than 15 minutes to figure it out, it is a bug in the system.

     Please go to:

     https://github.com/neohaskell/neohaskell/issues/new

     And report it. I'll be waiting for you.
    |]


commandsParser :: Command.OptionsParser NeoCommand
commandsParser = do
  let new =
        Command.CommandOptions
          { name = "new",
            description = "create a new project",
            version = Nothing,
            decoder = newParser
          }
  let build =
        Command.CommandOptions
          { name = "build",
            description = "build the project",
            version = Nothing,
            decoder = buildParser
          }
  let run =
        Command.CommandOptions
          { name = "run",
            description = "run the project",
            version = Nothing,
            decoder = runParser
          }
  Command.commands
    (Array.fromLinkedList [new, build, run])


buildParser :: Command.OptionsParser NeoCommand
buildParser = do
  common <- flagsParser
  pure (Build common)


runParser :: Command.OptionsParser NeoCommand
runParser = do
  common <- flagsParser
  pure (Run common)


newParser :: Command.OptionsParser NeoCommand
newParser = do
  projectName <- projectNameParser
  pure (New projectName)


flagsParser :: Command.OptionsParser CommonFlags
flagsParser = do
  projectFilePath <-
    Command.path
      Command.PathConfig
        { metavar = "PATH",
          short = 'c',
          help = "Path to the project configuration file",
          long = "projectConfig",
          value = Just [path|neo.json|]
        }
  pure (CommonFlags {projectFile = projectFilePath})


projectNameParser :: Command.OptionsParser New.ProjectName
projectNameParser = do
  projectName <-
    Command.text
      Command.TextConfig
        { metavar = "NAME",
          short = 'n',
          help = "Name of the project",
          long = "name",
          value = Nothing
        }
  pure (New.ProjectName projectName)


data Error
  = BuildError Build.Error
  | RunError Run.Error
  | NewError New.Error
  | Other


errorToText :: Error -> Text
errorToText err =
  case err of
    BuildError buildError ->
      [fmt|Error building the project, check the logs above for more details.


        Here's some more info though:

        #{toPrettyText buildError}|]
    RunError runError ->
      [fmt|Error running the project, check the logs above for more details.


        Here's some more info though:

        #{toPrettyText runError}|]
    NewError newError ->
      [fmt|Error creating the project, check the logs above for more details.


        Here's some more info though:

        #{toPrettyText newError}|]
    Other ->
      [fmt|An unknown error occurred, check the logs above for more details.|]


handleCommand :: NeoCommand -> Task Error ()
handleCommand command =
  case command of
    Build flags -> do
      txt <- File.readText flags.projectFile |> Task.mapError (\_ -> Other)
      case Json.decodeText txt of
        Err err -> panic err
        Ok config ->
          Build.handle config
            |> Task.mapError (\e -> BuildError e)
    Run flags -> do
      txt <- File.readText flags.projectFile |> Task.mapError (\_ -> Other)
      case Json.decodeText txt of
        Err err -> panic err
        Ok config -> do
          Build.handle config
            |> Task.mapError (\e -> BuildError e)
          Run.handle config
            |> Task.mapError (\e -> RunError e)
    New projectName -> do
      New.handle projectName
        |> Task.mapError (\e -> NewError e)
