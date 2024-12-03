{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Neo
  ( run,
  )
where

import Array qualified
import Command qualified
import Console qualified
import Core
import File qualified
import Json qualified
import Neo.Build qualified as Build
import Task qualified

data CommonFlags = CommonFlags
  { projectFile :: Path
  }
  deriving (Show, Eq, Ord)

data NeoCommand
  = Build CommonFlags
  deriving (Show, Eq, Ord)

run :: Task _ Unit
run = do
  Console.print "NEOHASKELL IS IN IT'S EARLY DAYS, HERE BE DRAGONS, BEWARE!"
  let parser =
        Command.CommandOptions
          { name = "neo",
            description = "NeoHaskell's console helper",
            version = Just [Core.version|0.5.0|],
            decoder = commandsParser
          }
  cmd <- Command.parseHandler parser
  handleCommand cmd

commandsParser :: Command.OptionsParser NeoCommand
commandsParser = do
  let build =
        Command.CommandOptions
          { name = "build",
            description = "build a file or directory",
            version = Nothing,
            decoder = buildParser
          }
  Command.commands
    (Array.fromLinkedList [build])

buildParser :: Command.OptionsParser NeoCommand
buildParser = do
  common <- flagsParser
  pure (Build common)

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

data Error
  = BuildError Build.Error
  | Other
  deriving (Show)

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
