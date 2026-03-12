module Main (main) where

import Prelude

import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

import NeoHaskell.LSP.Server qualified as Server


-- | NeoHaskell LSP server version.
version :: String
version = "0.1.0.0"


-- | Entry point for the @neohaskell-lsp@ executable.
--
-- Accepted flags:
--
-- * @--stdio@ (default): communicate over stdin\/stdout
-- * @--version@: print version and exit
-- * @--log-level=debug|info|warn|error@: control log verbosity (placeholder)
main :: IO ()
main = do
  args <- getArgs
  case parseMode args of
    PrintVersion -> do
      hPutStrLn stderr ("neohaskell-lsp " <> version)
      exitSuccess
    RunStdio _logLevel -> do
      exitCode <- Server.run
      case exitCode of
        0 -> exitSuccess
        n -> exitWith (ExitFailure n)


-- ---------------------------------------------------------------------------
-- CLI argument parsing
-- ---------------------------------------------------------------------------

-- | Operating mode parsed from command-line arguments.
data Mode
  = RunStdio LogLevel
  | PrintVersion

-- | Log verbosity level.
-- Currently accepted but not wired to the @lsp@ library's logger.
data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving (Show)


-- | Parse command-line arguments into a 'Mode'.
--
-- Recognises @--version@, @--stdio@, and @--log-level=LEVEL@.
-- Defaults to @RunStdio Info@ when no arguments are given.
parseMode :: [String] -> Mode
parseMode = go Info
  where
    go logLevel [] = RunStdio logLevel
    go logLevel (arg : rest) =
      case arg of
        "--version" -> PrintVersion
        "--stdio"   -> go logLevel rest
        _
          | Just lvl <- parseLogLevelFlag arg -> go lvl rest
          | otherwise -> go logLevel rest  -- ignore unknown flags

    parseLogLevelFlag :: String -> Maybe LogLevel
    parseLogLevelFlag flag =
      case flag of
        "--log-level=debug" -> Just Debug
        "--log-level=info"  -> Just Info
        "--log-level=warn"  -> Just Warn
        "--log-level=error" -> Just Error
        _                   -> Nothing
