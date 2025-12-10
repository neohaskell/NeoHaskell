{- HLINT ignore "Use camelCase" -}
module Service.ServiceDefinition.Core (
  Service,
  new,
  useServer,
  command,
  __internal_runServiceMain,
) where

import Array (Array)
import Array qualified
import Basics
import Console qualified
import GHC.IO qualified as GHC
import GHC.TypeLits qualified as GHC
import Record (Record)
import Record qualified
import Service.Command (NameOf)
import Service.Command.Core (Command (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


data
  Service
    (commandRow :: Record.Row Type)
  = Service
  { commandDefinitions :: Record commandRow,
    inspectDict :: Record.ContextRecord (Record.Dict CommandInspect) commandRow
  }


class CommandInspect cmd where
  commandName ::
    cmd ->
    Text


getSymbolText :: forall name. (GHC.KnownSymbol name) => Record.Proxy name -> Text
getSymbolText _ =
  GHC.symbolVal (Record.Proxy @name)
    |> Text.fromLinkedList


data CommandDefinition cmd
  = CommandDefinition
  { commandName :: Text
  }
  deriving (Show)


instance CommandInspect (CommandDefinition cmd) where
  commandName d = d.commandName


type instance NameOf (CommandDefinition cmd) = NameOf cmd


new :: Service '[]
new =
  Service
    { commandDefinitions = Record.empty,
      inspectDict = Record.empty
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
    Record.KnownSymbol commandName,
    CommandInspect (CommandDefinition cmd)
  ) =>
  Record.Proxy cmd ->
  Service originalCommands ->
  Service ((commandName 'Record.:= CommandDefinition cmd) ': originalCommands)
command _ serviceDefinition = do
  let cmdName :: Record.Field commandName = fromLabel
  let cmdVal :: Record.I (CommandDefinition cmd) =
        Record.I
          CommandDefinition
            { commandName = getSymbolText (Record.Proxy @commandName)
            }
  let currentCmds :: Record originalCommands = serviceDefinition.commandDefinitions
  let cmds =
        currentCmds
          |> Record.insert cmdName cmdVal
  let inspectDict :: Record.Dict CommandInspect (CommandDefinition cmd) = Record.Dict
  let newInspectDict =
        serviceDefinition.inspectDict
          |> Record.insert cmdName inspectDict
  Service
    { commandDefinitions = cmds,
      inspectDict = newInspectDict
    }


__internal_runServiceMain ::
  Service cmds -> GHC.IO Unit
__internal_runServiceMain s =
  case Record.reflectAllFields s.inspectDict of
    Record.Reflected ->
      runService s.commandDefinitions
        |> Task.runOrPanic


runService ::
  forall (cmds :: Record.Row Type).
  (Record.AllFields cmds CommandInspect) =>
  Record.ContextRecord Record.I cmds ->
  Task Text Unit
runService commandDefinitions = do
  let mapper ::
        forall cmd.
        (CommandInspect (cmd)) =>
        Record.I (cmd) -> Record.K Text (cmd)
      mapper (Record.I x) = Record.K (commandName x)

  let xs :: Array Text =
        commandDefinitions
          |> Record.cmap (Record.Proxy @CommandInspect) mapper
          |> Record.collapse
          |> Array.fromLinkedList
  let x = xs |> Text.joinWith ", "
  Console.print x