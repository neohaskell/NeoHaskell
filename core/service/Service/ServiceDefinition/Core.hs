{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
import Bytes (Bytes)
import Console qualified
import GHC.IO qualified as GHC
import GHC.TypeLits qualified as GHC
import Record (Record)
import Record qualified
import Service.Api.ApiBuilder (ApiBuilder (..), ApiEndpointHandler)
import Service.Command.Core (ApiOf, Command (..), NameOf)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


data
  Service
    (commandRow :: Record.Row Type)
    (apiRow :: Record.Row Type)
  = Service
  { commandDefinitions :: Record commandRow,
    inspectDict :: Record.ContextRecord (Record.Dict CommandInspect) commandRow,
    apis :: Record apiRow
  }


getSymbolText :: forall name. (GHC.KnownSymbol name) => Record.Proxy name -> Text
getSymbolText _ =
  GHC.symbolVal (Record.Proxy @name)
    |> Text.fromLinkedList


data CommandDefinition (name :: Symbol) (api :: Type) (cmd :: Type)
  = CommandDefinition
  { commandName :: Text
  }
  deriving (Show)


class CommandInspect cmd where
  type Cmd cmd


  commandName ::
    cmd ->
    Text


  buildCmdEP ::
    Record.Proxy cmd ->
    ApiOf (Cmd cmd) ->
    Record.Proxy (Cmd cmd) ->
    ApiEndpointHandler


instance
  (Command cmd, name ~ NameOf cmd, api ~ ApiOf cmd, Record.KnownSymbol name, Record.KnownSymbol name, ApiBuilder api) =>
  CommandInspect (CommandDefinition name api cmd)
  where
  type Cmd (CommandDefinition name api cmd) = cmd


  commandName :: (Record.KnownSymbol name) => CommandDefinition name api cmd -> Text
  commandName _ = getSymbolText (Record.Proxy @name)


  buildCmdEP ::
    Record.Proxy (CommandDefinition name api cmd) ->
    api ->
    Record.Proxy cmd ->
    ApiEndpointHandler
  buildCmdEP _ = buildCommandHandler


type instance NameOf (CommandDefinition name api cmd) = name


new :: Service '[] '[]
new =
  Service
    { commandDefinitions = Record.empty,
      inspectDict = Record.empty,
      apis = Record.empty
    }


useServer ::
  forall api apiName currentApis.
  ( ApiBuilder api,
    apiName ~ NameOf api,
    Record.KnownHash apiName,
    Record.KnownSymbol apiName
  ) =>
  api ->
  Service _ currentApis ->
  Service _ ((apiName 'Record.:= api) ': currentApis)
useServer _ _ =
  panic "use server not implemented"


-- | Register a command type in the service definition
command ::
  forall (cmd :: Type) originalCommands commandName commandApi.
  ( Command cmd,
    commandName ~ NameOf cmd,
    commandApi ~ ApiOf cmd,
    ApiBuilder commandApi,
    Record.KnownHash commandName,
    Record.KnownSymbol commandName,
    CommandInspect (CommandDefinition commandName commandApi cmd)
  ) =>
  Service originalCommands _ ->
  Service ((commandName 'Record.:= CommandDefinition commandName commandApi cmd) ': originalCommands) _
command serviceDefinition = do
  let cmdName :: Record.Field commandName = fromLabel
  let cmdVal :: Record.I (CommandDefinition commandName commandApi cmd) =
        Record.I
          CommandDefinition
            { commandName = getSymbolText (Record.Proxy @commandName)
            }
  let currentCmds :: Record originalCommands = serviceDefinition.commandDefinitions
  let cmds =
        currentCmds
          |> Record.insert cmdName cmdVal
  let inspectDict :: Record.Dict CommandInspect (CommandDefinition commandName commandApi cmd) = Record.Dict
  let newInspectDict =
        serviceDefinition.inspectDict
          |> Record.insert cmdName inspectDict
  Service
    { commandDefinitions = cmds,
      inspectDict = newInspectDict,
      apis = serviceDefinition.apis
    }


__internal_runServiceMain ::
  Service cmds apis -> GHC.IO Unit
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
        forall cmdDef cmd.
        ( CommandInspect (cmdDef),
          cmd ~ Cmd cmdDef
        ) =>
        Record.I (cmdDef) -> Record.K (Text, ApiEndpointHandler) (cmdDef)
      mapper (Record.I x) = Record.K (commandName x, buildCmdEP (Record.Proxy @cmdDef) apiBuilder (Record.Proxy @cmd))

  let xs :: Array (Text, ApiEndpointHandler) =
        commandDefinitions
          |> Record.cmap (Record.Proxy @CommandInspect) mapper
          |> Record.collapse
          |> Array.fromLinkedList
  xs |> Task.forEach \(cmdName, ep) -> do
    let respond :: Bytes -> Task Text Unit
        respond bb = do
          let bt = Text.fromBytes bb
          Console.print [fmt|/commands/#{cmdName} - RESPOND: #{bt}|]
    let fakeBody = "FAKE BODY" |> Text.toBytes
    ep fakeBody respond