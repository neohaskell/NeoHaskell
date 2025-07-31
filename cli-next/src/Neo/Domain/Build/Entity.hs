module Neo.Domain.Build.Entity where

import Array qualified
import Core
import Maybe qualified
import Neo.Domain.Build.Events
import Neo.Domain.Build.Types


-- ================================================================================
-- DOMAIN ENTITY
-- ================================================================================

-- Build Entity: Maintains build state
-- In NeoHaskell, each entity instance has its own single stream
data BuildEntity = BuildEntity
  { buildId :: BuildId,
    status :: BuildStatus,
    projectPath :: Path,
    config :: Maybe ProjectConfiguration,
    nixExpression :: Maybe Text,
    startInt :: Maybe Int,
    endInt :: Maybe Int,
    logs :: Array LogEntry,
    exitCode :: Maybe Int,
    resultPath :: Maybe Path
  }
  deriving (Show)


-- Apply events to entity
applyBuildEvent :: BuildEntity -> BuildEvent -> BuildEntity
applyBuildEvent entity ev =
  case ev of
    InitiatedE event ->
      entity
        { buildId = event.buildId,
          status = Initializing,
          startInt = Maybe.Just event.timestamp,
          logs = Array.wrap (LogEntry event.timestamp Info "Build initiated")
        }
    ConfigLoadedE event ->
      entity
        { status = LoadingConfig,
          config = Just (ProjectConfiguration event.projectName event.overrideNeohaskell event.dependencies),
          logs = entity.logs |> Array.push (LogEntry event.timestamp Info "Configuration loaded")
        }
    NixPreparedE event ->
      entity
        { status = PreparingNixExpression,
          nixExpression = Just event.nixExpression
        }
    NixStartedE _ ->
      entity
        { status = RunningNixBuild
        }
    OutputReceivedE event ->
      do
        let logLevel = case event.outputType of
              StdOut -> Info
              StdErr -> Warning
        let newLog = LogEntry event.timestamp logLevel event.content
        entity {logs = entity.logs |> Array.push newLog}
    CompletedE event ->
      do
        let newStatus = case event.exitCode of
              0 -> Completed
              _ -> Failed (event.errorMessage |> Maybe.withDefault "Unknown error")
        entity
          { status = newStatus,
            endInt = Just event.endInt,
            exitCode = Just event.exitCode,
            resultPath = event.resultPath
          }