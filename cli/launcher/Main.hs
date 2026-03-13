module Main (main) where

import Array qualified
import Console qualified
import Core
import Environment qualified
import IO qualified
import NeoHaskell.LSP.Server qualified as Server
import Task qualified


-- | NeoHaskell LSP server version.
serverVersion :: Text
serverVersion = "0.1.0.0"


-- | Entry point for the @neohaskell-lsp@ executable.
--
-- Accepted flags:
--
-- * @--stdio@ (default): communicate over stdin\/stdout
-- * @--version@: print version and exit
-- * @--log-level=debug|info|warn|error@: control log verbosity (placeholder)
main :: IO ()
main = do
  args <- Environment.getArgs |> Task.runNoErrors
  case parseMode args of
    PrintVersion -> do
      Console.error [fmt|neohaskell-lsp #{serverVersion}|]
      IO.exitSuccess
    RunStdio _logLevel -> do
      exitCode <- Server.run
      if exitCode == 0
        then IO.exitSuccess
        else IO.exitFailure exitCode


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
parseMode :: Array Text -> Mode
parseMode args = do
  let argList = args |> Array.toLinkedList
  parseArgs Info argList


parseArgs :: LogLevel -> LinkedList Text -> Mode
parseArgs logLevel args =
  case args of
    [] -> RunStdio logLevel
    (arg : rest) ->
      if arg == "--version"
        then PrintVersion
        else
          if arg == "--stdio"
            then parseArgs logLevel rest
            else case parseLogLevelFlag arg of
              Just lvl -> parseArgs lvl rest
              Nothing -> parseArgs logLevel rest


parseLogLevelFlag :: Text -> Maybe LogLevel
parseLogLevelFlag flag =
  if flag == "--log-level=debug"
    then Just Debug
    else
      if flag == "--log-level=info"
        then Just Info
        else
          if flag == "--log-level=warn"
            then Just Warn
            else
              if flag == "--log-level=error"
                then Just Error
                else Nothing
