module Neo.Domain.Build.Events where

import Core
import Neo.Domain.Build.Types


-- ================================================================================
-- DOMAIN EVENTS
-- ================================================================================

data BuildInitiatedEvent = BuildInitiatedEvent
  { buildId :: BuildId
  , projectPath :: Path
  , timestamp :: Time
  }
  deriving (Show)
  -- Example: BuildInitiatedEvent
  --   { buildId = "build-abc123"
  --   , projectPath = "/Users/dev/my-neohaskell-project"
  --   , timestamp = 2024-01-15T10:30:00Z
  --   }

data ProjectConfigLoadedEvent = ProjectConfigLoadedEvent
  { buildId :: BuildId
  , projectName :: Text
  , overrideNeohaskell :: Maybe Text
  , dependencies :: Array Text
  , timestamp :: Time
  }
  deriving (Show)
  -- Example: ProjectConfigLoadedEvent
  --   { buildId = "build-abc123"
  --   , projectName = "MyApp"
  --   , overrideNeohaskell = Just "https://github.com/NeoHaskell/NeoHaskell/archive/refs/heads/main.tar.gz"
  --   , dependencies = ["nhcore", "servant"]
  --   , timestamp = 2024-01-15T10:30:01Z
  --   }

data NixExpressionPreparedEvent = NixExpressionPreparedEvent
  { buildId :: BuildId
  , nixExpression :: Text
  , neoHaskellSource :: Text
  , timestamp :: Time
  }
  deriving (Show)
  -- Example: NixExpressionPreparedEvent
  --   { buildId = "build-abc123"
  --   , nixExpression = "let nhroot = builtins.fetchTarball \"...\"; lib = import (nhroot + \"/nix/lib.nix\") {}; in lib.buildNeoProject { ... }"
  --   , neoHaskellSource = "builtins.fetchTarball \"https://github.com/NeoHaskell/NeoHaskell/archive/refs/heads/main.tar.gz\""
  --   , timestamp = 2024-01-15T10:30:02Z
  --   }

data NixBuildStartedEvent = NixBuildStartedEvent
  { buildId :: BuildId
  , nixCommand :: Array Text
  , workingDirectory :: Path
  , timestamp :: Time
  }
  deriving (Show)
  -- Example: NixBuildStartedEvent
  --   { buildId = "build-abc123"
  --   , nixCommand = ["nix-build", "--show-trace", "-E", "let nhroot = ..."]
  --   , workingDirectory = "/Users/dev/my-neohaskell-project"
  --   , timestamp = 2024-01-15T10:30:03Z
  --   }

data BuildOutputReceivedEvent = BuildOutputReceivedEvent
  { buildId :: BuildId
  , outputType :: OutputType
  , content :: Text
  , timestamp :: Time
  }
  deriving (Show)
  -- Example: BuildOutputReceivedEvent
  --   { buildId = "build-abc123"
  --   , outputType = StdOut
  --   , content = "building '/nix/store/abc123-my-neohaskell-project.drv'..."
  --   , timestamp = 2024-01-15T10:30:05Z
  --   }

data BuildCompletedEvent = BuildCompletedEvent
  { buildId :: BuildId
  , exitCode :: Int
  , endTime :: Time
  , resultPath :: Maybe Path
  , errorMessage :: Maybe Text
  , buildDuration :: Int -- seconds
  }
  deriving (Show)
  -- Example: BuildCompletedEvent
  --   { buildId = "build-abc123"
  --   , exitCode = 0
  --   , endTime = 2024-01-15T10:32:15Z
  --   , resultPath = Just "/nix/store/xyz789-my-neohaskell-project"
  --   , errorMessage = Nothing
  --   , buildDuration = 135
  --   }

-- Union type for all build events
data BuildEvent
  = InitiatedE BuildInitiatedEvent
  | ConfigLoadedE ProjectConfigLoadedEvent
  | NixPreparedE NixExpressionPreparedEvent
  | NixStartedE NixBuildStartedEvent
  | OutputReceivedE BuildOutputReceivedEvent
  | CompletedE BuildCompletedEvent
  deriving (Show)