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
import Maybe (Maybe (..))
import Record (Record)
import Record qualified
import Service.Api.ApiBuilder (ApiBuilder (..), ApiEndpointHandler)
import Service.Command.Core (ApiOf, Command (..), NameOf)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Unsafe.Coerce qualified as GHC


data
  Service
    (commandRow :: Record.Row Type)
    (apiRow :: Record.Row Type)
  = Service
  { commandDefinitions :: Record commandRow,
    -- inspectDict :: Record.ContextRecord (Record.Dict (CommandInspect apiRow)) commandRow,
    apis :: Record apiRow
  }


getSymbolText :: forall name. (GHC.KnownSymbol name) => Record.Proxy name -> Text
getSymbolText _ =
  GHC.symbolVal (Record.Proxy @name)
    |> Text.fromLinkedList


data CommandDefinition (name :: Symbol) (api :: Type) (cmd :: Type) (apiName :: Symbol)
  = CommandDefinition
  { commandName :: Text,
    api :: Maybe api
  }
  deriving (Show)


class CommandInspect (apis :: Record.Row Type) definition where
  type Cmd definition


  commandName ::
    definition ->
    Text


  buildCmdEP ::
    definition ->
    Record apis ->
    Record.Proxy (Cmd definition) ->
    ApiEndpointHandler


instance
  ( Command cmd,
    name ~ NameOf cmd,
    Record.KnownSymbol name,
    Record.KnownHash name,
    -- api
    api ~ ApiOf cmd,
    Record.RowHasField apiName apis api,
    apiName ~ NameOf api,
    Record.KnownSymbol apiName,
    Record.KnownHash apiName,
    ApiBuilder api
  ) =>
  CommandInspect apis (CommandDefinition name api cmd apiName)
  where
  type Cmd (CommandDefinition name api cmd apiName) = cmd


  commandName :: (Record.KnownSymbol name) => CommandDefinition name api cmd apiName -> Text
  commandName _ = getSymbolText (Record.Proxy @name)


  buildCmdEP ::
    (CommandDefinition name api cmd apiName) ->
    Record apis ->
    Record.Proxy cmd ->
    ApiEndpointHandler
  buildCmdEP _ apisRecord cmd = do
    let (Record.I api) = apisRecord |> Record.get (fromLabel @apiName)
    buildCommandHandler api cmd


type instance NameOf (CommandDefinition name api cmd apiName) = name


new :: Service '[] '[]
new =
  Service
    { commandDefinitions = Record.empty,
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
  forall (cmd :: Type) originalCommands commandName commandApi apis apiName.
  ( Command cmd,
    commandName ~ NameOf cmd,
    Record.KnownSymbol commandName,
    Record.KnownHash commandName
    -- CommandInspect apis (CommandDefinition commandName commandApi cmd apiName)
  ) =>
  Service originalCommands apis ->
  Service ((commandName 'Record.:= CommandDefinition commandName commandApi cmd apiName) ': originalCommands) apis
command serviceDefinition = do
  let cmdName :: Record.Field commandName = fromLabel
  let cmdVal :: Record.I (CommandDefinition commandName commandApi cmd apiName) =
        Record.I
          CommandDefinition
            { commandName = getSymbolText (Record.Proxy @commandName),
              api = Nothing
            }
  let currentCmds :: Record originalCommands = serviceDefinition.commandDefinitions
  let cmds =
        currentCmds
          |> Record.insert cmdName cmdVal
  Service
    { commandDefinitions = cmds,
      apis = serviceDefinition.apis
    }


__internal_runServiceMain ::
  forall cmds apis.
  Service cmds apis -> GHC.IO Unit
__internal_runServiceMain s = do
  let inspectDict = Record.reifyAllFields @cmds (Record.Proxy @(CommandInspect apis))
  case Record.reflectAllFields inspectDict of
    Record.Reflected ->
      runService s.commandDefinitions s.apis
        |> Task.runOrPanic


runService ::
  forall (cmds :: Record.Row Type) (apis :: Record.Row Type).
  (Record.AllFields cmds (CommandInspect apis)) =>
  Record.ContextRecord Record.I cmds ->
  Record apis ->
  Task Text Unit
runService commandDefinitions apis = do
  let mapper ::
        forall cmdDef cmd.
        ( CommandInspect apis cmdDef,
          cmd ~ Cmd cmdDef
        ) =>
        Record.I (cmdDef) ->
        Record.K (Text, ApiEndpointHandler) (cmdDef)
      mapper (Record.I x) = do
        Record.K (commandName @apis x, buildCmdEP x apis (Record.Proxy @cmd))

  let xs :: Array (Text, ApiEndpointHandler) =
        commandDefinitions
          |> Record.cmap (Record.Proxy @(CommandInspect apis)) mapper
          |> Record.collapse
          |> Array.fromLinkedList
  xs |> Task.forEach \(cmdName, ep) -> do
    let respond :: Bytes -> Task Text Unit
        respond bb = do
          let bt = Text.fromBytes bb
          Console.print [fmt|/commands/#{cmdName} - RESPOND: #{bt}|]
    let fakeBody = "FAKE BODY" |> Text.toBytes
    ep fakeBody respond