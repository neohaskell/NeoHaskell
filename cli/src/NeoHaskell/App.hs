module NeoHaskell.App
  ( -- * Running
    run
  )
where

import Command (CommandOptions (..))
import Command qualified
import Core
import NeoHaskell.Cli.Lsp qualified as Lsp
import Task qualified


-- | All commands the @neo@ CLI supports.
data Cmd
  = CmdLsp Lsp.Options


-- | Top-level @neo@ command definition.
neoCommand :: CommandOptions Cmd
neoCommand =
  CommandOptions
    { name = "neo",
      description = "The NeoHaskell CLI",
      version = Nothing,
      decoder = Command.commands [liftCommand CmdLsp Lsp.command]
    }


-- | Lift a subcommand's 'CommandOptions' into the top-level 'Cmd' type.
liftCommand :: (opts -> Cmd) -> CommandOptions opts -> CommandOptions Cmd
liftCommand wrap cmdOpts =
  cmdOpts {decoder = Command.map wrap cmdOpts.decoder}


-- | Parse CLI arguments and dispatch to the appropriate subcommand.
run :: IO ()
run = do
  cmd <- Command.parseHandler neoCommand |> Task.runOrPanic
  case cmd of
    CmdLsp opts -> Lsp.run opts
