module Neo.Domain.Build.Entity where

import Core
import Neo.Domain.Build.Types
import Neo.Domain.Build.Events
import Array qualified
import Maybe qualified


-- ================================================================================
-- DOMAIN ENTITY
-- ================================================================================

-- Build Entity: Maintains build state
-- In NeoHaskell, each entity instance has its own single stream
data BuildEntity = BuildEntity
  { buildId :: BuildId
  , status :: BuildStatus
  , projectPath :: Path
  , config :: Maybe ProjectConfiguration
  , nixExpression :: Maybe Text
  , startTime :: Maybe Time
  , endTime :: Maybe Time
  , logs :: Array LogEntry
  , exitCode :: Maybe Int
  , resultPath :: Maybe Path
  }
  deriving (Show)


-- Apply events to entity
applyBuildEvent :: BuildEntity -> BuildEvent -> BuildEntity
applyBuildEvent entity event =
  case event of
    InitiatedE e ->
      entity
        { buildId = e.buildId
        , status = Initializing
        , startTime = e.timestamp
        , logs = Array.singleton (LogEntry e.timestamp Info "Build initiated")
        }
    
    ConfigLoadedE e ->
      entity
        { status = LoadingConfig
        , config = Just (ProjectConfiguration e.projectName e.overrideNeohaskell e.dependencies)
        , logs = entity.logs |> Array.append (LogEntry e.buildId Info "Configuration loaded")
        }
    
    NixPreparedE e ->
      entity
        { status = PreparingNixExpression
        , nixExpression = Just e.nixExpression
        }
    
    NixStartedE e ->
      entity
        { status = RunningNixBuild
        }
    
    OutputReceivedE e ->
      do
        let logLevel = case e.outputType of
              StdOut -> Info
              StdErr -> Warning
        let newLog = LogEntry e.timestamp logLevel e.content
        entity { logs = entity.logs |> Array.append newLog }
    
    CompletedE e ->
      do
        let newStatus = case e.exitCode of
              0 -> Completed
              _ -> Failed (e.errorMessage |> Maybe.withDefault "Unknown error")
        entity
          { status = newStatus
          , endTime = Just e.endTime
          , exitCode = Just e.exitCode
          , resultPath = e.resultPath
          }