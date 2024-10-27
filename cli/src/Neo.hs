module Neo (
  run,
  fromText
) where

import Array qualified
import Command qualified
import Core
import Json qualified


data CommonFlags = CommonFlags
  { projectFile :: Path
  }
  deriving (Show, Eq, Ord)


data NeoCommand
  = Build CommonFlags
  deriving (Show, Eq, Ord)


run :: IO ()
run = do
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


handleCommand :: NeoCommand -> IO ()
handleCommand command =
  case command of
    Build flags -> do
      toText flags |> print


-- Project config

data ProjectConfiguration = ProjectConfiguration
  { name :: Text,
    version :: Version,
    description :: Text,
    author :: Text,
    license :: Text
  }
  deriving (Show, Eq, Ord, Generic)


-- | We allow the `ProjectConfiguration` type to be converted from JSON.
instance Json.FromJSON ProjectConfiguration


-- | We allow the `ProjectConfiguration` type to be converted to JSON.
instance Json.ToJSON ProjectConfiguration


-- | The `ProjectConfiguration.fromText` function allows you to convert a JSON
-- `Text` value to a `ProjectConfiguration` value.
--
-- >>> fromText "{\"name\":\"neo\",\"version\":\"0.5.0\",\"description\":\"NeoHaskell's console helper\",\"author\":\"NeoHaskell\",\"license\":\"MIT\"}"
-- Ok (ProjectConfiguration {name = "neo", version = [version|0.5.0|], description = "NeoHaskell's console helper", author = "NeoHaskell", license = "MIT"})
--
-- >>> fromText "some invalid json"
-- Err "Error in $: not enough input"
fromText :: Text -> Result Text ProjectConfiguration
fromText someText = Json.decodeText someText
