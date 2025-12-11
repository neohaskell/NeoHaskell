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
import Map (Map)
import Map qualified
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
    (commandApiNames :: [Symbol])
    (providedApiNames :: [Symbol])
  = Service
  { commandDefinitions :: Record commandRow,
    inspectDict :: Record.ContextRecord (Record.Dict (CommandInspect)) commandRow,
    apis :: Map Text ApiBuilderValue
  }


data ApiBuilderValue
  = forall api.
    (ApiBuilder api) =>
    ApiBuilderValue api


getApiBuilderValue ::
  ApiBuilderValue -> api
getApiBuilderValue (ApiBuilderValue api) = GHC.unsafeCoerce api


getSymbolText :: forall name. (GHC.KnownSymbol name) => Record.Proxy name -> Text
getSymbolText _ =
  GHC.symbolVal (Record.Proxy @name)
    |> Text.fromLinkedList


data CommandDefinition (name :: Symbol) (api :: Type) (cmd :: Type) (apiName :: Symbol)
  = CommandDefinition
  { commandName :: Text
  }
  deriving (Show)


class CommandInspect definition where
  type Cmd definition
  type Api definition


  commandName ::
    definition ->
    Text


  buildCmdEP ::
    definition ->
    Api definition ->
    Record.Proxy (Cmd definition) ->
    ApiEndpointHandler


instance
  ( Command cmd,
    ApiBuilder api,
    name ~ NameOf cmd,
    Record.KnownSymbol name,
    Record.KnownHash name
  ) =>
  CommandInspect (CommandDefinition name api cmd apiName)
  where
  type Cmd (CommandDefinition name api cmd apiName) = cmd
  type Api (CommandDefinition name api cmd apiName) = api


  commandName :: (Record.KnownSymbol name) => CommandDefinition name api cmd apiName -> Text
  commandName _ = getSymbolText (Record.Proxy @name)


  buildCmdEP ::
    (CommandDefinition name api cmd apiName) ->
    api ->
    Record.Proxy cmd ->
    ApiEndpointHandler
  buildCmdEP _ api cmd = do
    buildCommandHandler @api (api) cmd


type instance NameOf (CommandDefinition name api cmd apiName) = name


new :: Service '[] '[] '[] '[]
new =
  Service
    { commandDefinitions = Record.empty,
      inspectDict = Record.empty,
      apis = Map.empty
    }


useServer ::
  forall api apiName currentApis commandApiNames providedApiNames cmds.
  ( ApiBuilder api,
    apiName ~ NameOf api,
    Record.KnownHash apiName,
    Record.KnownSymbol apiName
  ) =>
  api ->
  Service cmds currentApis commandApiNames providedApiNames ->
  Service cmds ((apiName 'Record.:= api) ': currentApis) commandApiNames (apiName ': providedApiNames)
useServer _ _ =
  panic "use server not implemented"


-- | Register a command type in the service definition
command ::
  forall
    (cmd :: Type)
    originalCommands
    commandName
    commandApi
    apis
    (apiName :: Symbol)
    (commandApiNames :: [Symbol])
    providedApiNames.
  ( Command cmd,
    commandName ~ NameOf cmd,
    commandApi ~ ApiOf cmd,
    apiName ~ NameOf commandApi,
    Record.KnownSymbol commandName,
    Record.KnownHash
      commandName,
    CommandInspect
      (CommandDefinition commandName commandApi cmd apiName)
  ) =>
  Service originalCommands apis commandApiNames providedApiNames ->
  Service
    ((commandName 'Record.:= CommandDefinition commandName commandApi cmd apiName) ': originalCommands)
    apis
    (apiName ': commandApiNames)
    providedApiNames
command serviceDefinition = do
  let cmdName :: Record.Field commandName = fromLabel
  let cmdVal :: Record.I (CommandDefinition commandName commandApi cmd apiName) =
        Record.I
          CommandDefinition
            { commandName = getSymbolText (Record.Proxy @commandName)
            }
  let currentCmds :: Record originalCommands = serviceDefinition.commandDefinitions
  let cmds =
        currentCmds
          |> Record.insert cmdName cmdVal
  let inspectDict :: Record.Dict (CommandInspect) (CommandDefinition commandName commandApi cmd apiName) = Record.Dict
  let newInspectDict =
        serviceDefinition.inspectDict
          |> Record.insert cmdName inspectDict

  Service
    { commandDefinitions = cmds,
      apis = serviceDefinition.apis,
      inspectDict = newInspectDict
    }


__internal_runServiceMain ::
  forall cmds apis commandApiNames providedApiNames.
  Service cmds apis commandApiNames providedApiNames -> GHC.IO Unit
__internal_runServiceMain s = do
  case Record.reflectAllFields s.inspectDict of
    Record.Reflected ->
      runService s.commandDefinitions s.apis
        |> Task.runOrPanic


runService ::
  forall (cmds :: Record.Row Type).
  (Record.AllFields cmds (CommandInspect)) =>
  Record.ContextRecord Record.I cmds ->
  Map Text ApiBuilderValue ->
  Task Text Unit
runService commandDefinitions apis = do
  let mapper ::
        forall cmdDef cmd.
        ( CommandInspect cmdDef,
          cmd ~ Cmd cmdDef
        ) =>
        Record.I (cmdDef) ->
        Record.K (Text, ApiEndpointHandler) (cmdDef)
      mapper (Record.I x) = do
        case apis |> Map.get (commandName x) of
          Nothing -> panic [fmt|The impossible happened, couldnt find API config for #{commandName x}|]
          Just apiBV -> do
            let api = getApiBuilderValue apiBV
            Record.K (commandName x, buildCmdEP x (api) (Record.Proxy @cmd))

  let xs :: Array (Text, ApiEndpointHandler) =
        commandDefinitions
          |> Record.cmap (Record.Proxy @(CommandInspect)) mapper
          |> Record.collapse
          |> Array.fromLinkedList
  xs |> Task.forEach \(cmdName, ep) -> do
    let respond :: Bytes -> Task Text Unit
        respond bb = do
          let bt = Text.fromBytes bb
          Console.print [fmt|/commands/#{cmdName} - RESPOND: #{bt}|]
    let fakeBody = "FAKE BODY" |> Text.toBytes
    ep fakeBody respond