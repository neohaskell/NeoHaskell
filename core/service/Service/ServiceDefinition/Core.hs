{- HLINT ignore "Use camelCase" -}
module Service.ServiceDefinition.Core (
  Service,
  new,
  useServer,
  command,
  __internal_runServiceMain,
) where

import Basics
import GHC.IO qualified as GHC
import Record (Record)
import Record qualified
import Service.Command (NameOf)
import Service.Command.Core (Command (..))


data
  Service
    (commandRow :: Record.Row Type)
  = Service
  { commandDefinitions :: Record commandRow
  }


data CommandDefinition cmd
  = CommandDefinition


new :: Service '[]
new =
  Service
    { commandDefinitions = Record.empty
    }


useServer ::
  sv -> sd -> sd2
useServer _ _ =
  panic "use server not implemented"


-- | Register a command type in the service definition
command ::
  forall (cmd :: Type) originalCommands commandName.
  ( Command cmd,
    commandName ~ NameOf cmd,
    Record.KnownHash commandName,
    Record.KnownSymbol commandName
  ) =>
  Record.Proxy cmd ->
  Service originalCommands ->
  Service ((commandName 'Record.:= CommandDefinition cmd) ': originalCommands)
command _ serviceDefinition = do
  let cmdName :: Record.Field commandName = fromLabel
  let cmdVal :: Record.I (CommandDefinition cmd) = Record.I CommandDefinition
  let currentCmds :: Record originalCommands = serviceDefinition.commandDefinitions
  let cmds =
        currentCmds
          |> Record.insert cmdName cmdVal
  Service
    { commandDefinitions = cmds
    }


__internal_runServiceMain ::
  Service cmds -> GHC.IO Unit
__internal_runServiceMain _ = do
  panic "__internal_runServiceMain - not implemented"