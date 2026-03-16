module NeoHaskell.Cli.Lsp
  ( -- * Types
    Options (..)
  , LogLevel (..)
    -- * Command
  , command
    -- * Execution
  , run
  )
where

import Command (CommandOptions (..), FlagConfig (..), OptionsParser, TextConfig (..))
import Command qualified
import Core
import IO qualified
import NeoHaskell.LSP.Server qualified as Server


-- | Options for the @neo lsp@ subcommand.
data Options = Options
  { logLevel :: LogLevel
  }


-- | Log verbosity level.
-- Currently accepted but not wired to the @lsp@ library's logger.
data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving (Show)


-- | The @neo lsp@ subcommand definition.
command :: CommandOptions Options
command =
  CommandOptions
    { name = "lsp",
      description = "Start the NeoHaskell Language Server",
      version = Nothing,
      decoder = optionsParser
    }


-- | Parser for the @neo lsp@ subcommand options.
optionsParser :: OptionsParser Options
optionsParser = do
  _stdio <-
    Command.flag
      FlagConfig
        { help = "Communicate over stdin/stdout (default)",
          long = "stdio",
          short = defaultValue,
          value = Nothing
        }
  logLevelText <-
    Command.text
      TextConfig
        { help = "Log verbosity: debug|info|warn|error",
          long = "log-level",
          short = 'l',
          metavar = "LEVEL",
          value = Just "info"
        }
  pure Options {logLevel = parseLogLevel logLevelText}


-- | Run the LSP server.
run :: Options -> IO ()
run _opts = do
  exitCode <- Server.run
  if exitCode == 0
    then IO.exitSuccess
    else IO.exitFailure exitCode


-- | Parse a log level string into a 'LogLevel'.
parseLogLevel :: Text -> LogLevel
parseLogLevel level =
  if level == "debug"
    then Debug
    else
      if level == "warn"
        then Warn
        else
          if level == "error"
            then Error
            else Info
