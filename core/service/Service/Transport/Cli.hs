module Service.Transport.Cli (
  CliTransport (..),
  cli,
) where

import Array qualified
import Core
import Command qualified
import Console qualified
import GHC.TypeLits qualified as GHC
import Json qualified
import Map qualified
import Record qualified
import Service.Auth (RequestContext)
import Service.Auth qualified as Auth
import Service.CommandExecutor.TH (deriveKnownHash)
import Service.Response (CommandResponse)
import Service.Response qualified as Response
import Service.Transport (EndpointHandler, EndpointSchema (..), Endpoints (..), Transport (..))
import Service.Transport.Cli.Output qualified as Output
import Task qualified
import Text qualified


data CliTransport = CliTransport
  { programName :: Text
  , version :: Text
  , description :: Text
  }


type instance NameOf CliTransport = "CliTransport"


deriveKnownHash "CliTransport"


-- | Default CLI transport configuration.
cli :: CliTransport
cli = CliTransport
  { programName = "app"
  , version = "0.0.0"
  , description = ""
  }


instance Transport CliTransport where
  type Request CliTransport = Bytes
  type Response CliTransport = Bytes
  type RunnableTransport CliTransport = Task Text Unit


  buildHandler ::
    forall command name.
    ( Command command,
      Json.FromJSON command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    CliTransport ->
    Record.Proxy command ->
    (RequestContext -> command -> Task Text CommandResponse) ->
    EndpointHandler
  buildHandler _transport _ handler requestContext body respond = do
    let commandName =
          GHC.symbolVal (Record.Proxy @name)
            |> Text.fromLinkedList
    let commandValue = body |> Json.decodeBytes @command
    case commandValue of
      Ok cmd -> do
        response <- handler requestContext cmd
        let responseJson = Json.encodeText response |> Text.toBytes
        respond (response, responseJson)
      Err _err -> do
        let errorResponse =
              Response.Failed
                { error = [fmt|Invalid input for command #{commandName}|]
                }
        let responseJson = Json.encodeText errorResponse |> Text.toBytes
        respond (errorResponse, responseJson)


  assembleTransport ::
    Endpoints CliTransport ->
    Task Text Unit
  assembleTransport endpoints = do
    let cliTransport = endpoints.transport
    let commandSchemaEntries = endpoints.commandSchemas |> Map.entries
    -- Helper: strip "Entity" suffix from entity names
    let stripEntitySuffix entityName =
          case Text.endsWith "Entity" entityName of
            True -> Text.dropRight 6 entityName
            False -> entityName
    -- Helper: add a command to the correct entity group
    let addToGroup (cmdName, schema) groups = do
          let entityRawName = case schema.entityName of
                Just entityNameValue -> entityNameValue
                Nothing -> "general"
          let entityGroupName = stripEntitySuffix entityRawName |> Text.toKebabCase
          let cmdKebabName = cmdName |> Text.toKebabCase
          let existing = case Map.get entityGroupName groups of
                Just arr -> arr
                Nothing -> Array.empty
          let newEntry = Array.fromLinkedList [(cmdKebabName, cmdName, schema)]
          let updated = existing |> Array.append newEntry
          Map.set entityGroupName updated groups
    -- Group commands by entity name
    let entityGroups = commandSchemaEntries |> Array.foldl addToGroup Map.empty
    let entityGroupEntries = entityGroups |> Map.entries
    -- Build entity subcommands
    let entitySubcommands = entityGroupEntries |> Array.map (\(entityGroupName, cmds) -> do
          let commandSubcommands = cmds |> Array.map (\(cmdKebabName, originalCmdName, schema) -> do
                let parser = case schema.requestSchema of
                      Just reqSchema -> Command.fromSchema reqSchema
                      Nothing -> Command.fromSchema (SObject Array.empty)
                let cmdHandler = case Map.get originalCmdName endpoints.commandEndpoints of
                      Just h -> h
                      Nothing -> panic [fmt|No handler found for command #{originalCmdName}|]
                Command.CommandOptions
                  { name = cmdKebabName
                  , description = schema.description
                  , version = Nothing
                  , decoder = Command.map (\jsonVal -> (originalCmdName, cmdHandler, jsonVal)) parser
                  }
                )
          Command.CommandOptions
            { name = entityGroupName
            , description = [fmt|Commands for #{entityGroupName}|]
            , version = Nothing
            , decoder = Command.commands commandSubcommands
            }
          )
    let topParser = Command.commands entitySubcommands
    let options = Command.CommandOptions
          { name = cliTransport.programName
          , description = cliTransport.description
          , version = Nothing
          , decoder = topParser
          }
    -- Parse CLI args
    (_cmdName, selectedHandler, jsonVal) <- Command.parseHandler options
      |> Task.mapError (\_ -> "Failed to parse CLI arguments")
    -- Build request context (anonymous for CLI)
    requestContext <- Auth.anonymousContext
    -- Encode JSON value to bytes
    let requestBytes = Json.encodeText jsonVal |> Text.toBytes
    -- Call the handler
    selectedHandler requestContext requestBytes (\(response, _responseBytes) -> do
      let output = Output.formatResponse Output.JsonOutput response
      Console.print output
      )


  runTransport :: CliTransport -> Task Text Unit -> Task Text Unit
  runTransport _transport task = task
