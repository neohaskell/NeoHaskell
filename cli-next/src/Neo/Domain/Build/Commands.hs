module Neo.Domain.Build.Commands where

import Core
import Neo.Domain.Build.Types


-- ================================================================================
-- DOMAIN COMMANDS
-- ================================================================================

data InitiateBuildCommand = InitiateBuildCommand
  { buildId :: BuildId,
    projectPath :: Path,
    requestedAt :: Int
  }
  deriving (Show)


-- Example: InitiateBuildCommand
--   { buildId = "build-abc123"
--   , projectPath = "/Users/dev/my-neohaskell-project"
--   , requestedAt = 2024-01-15T10:30:00Z
--   }

data LoadProjectConfigCommand = LoadProjectConfigCommand
  { buildId :: BuildId,
    configPath :: Path
  }
  deriving (Show)


-- Example: LoadProjectConfigCommand
--   { buildId = "build-abc123"
--   , configPath = "/Users/dev/my-neohaskell-project/neo.json"
--   }

data PrepareNixExpressionCommand = PrepareNixExpressionCommand
  { buildId :: BuildId,
    config :: ProjectConfiguration,
    neoJsonPath :: Path,
    srcPath :: Path
  }
  deriving (Show)


-- Example: PrepareNixExpressionCommand
--   { buildId = "build-abc123"
--   , config = ProjectConfiguration { projectName = "MyApp", overrideNeohaskell = Just "...", dependencies = ["nhcore"] }
--   , neoJsonPath = "/Users/dev/my-neohaskell-project/neo.json"
--   , srcPath = "/Users/dev/my-neohaskell-project/src"
--   }

data ExecuteNixBuildCommand = ExecuteNixBuildCommand
  { buildId :: BuildId,
    nixExpression :: Text,
    workingDirectory :: Path,
    buildOptions :: Array Text
  }
  deriving (Show)


-- Example: ExecuteNixBuildCommand
--   { buildId = "build-abc123"
--   , nixExpression = "let nhroot = builtins.fetchTarball \"...\"; ..."
--   , workingDirectory = "/Users/dev/my-neohaskell-project"
--   , buildOptions = ["--show-trace"]
--   }

data ProcessBuildOutputCommand = ProcessBuildOutputCommand
  { buildId :: BuildId,
    outputLines :: Array Text
  }
  deriving (Show)


-- Example: ProcessBuildOutputCommand
--   { buildId = "build-abc123"
--   , outputLines = ["building '/nix/store/abc123-my-neohaskell-project.drv'...", "build succeeded"]
--   }

data CompleteBuildCommand = CompleteBuildCommand
  { buildId :: BuildId,
    exitCode :: Int,
    finalOutput :: Text,
    endInt :: Int,
    startInt :: Int
  }
  deriving (Show)

-- Example: CompleteBuildCommand
--   { buildId = "build-abc123"
--   , exitCode = 0
--   , finalOutput = "/nix/store/xyz789-my-neohaskell-project"
--   , endInt = 2024-01-15T10:32:15Z
--   , startInt = 2024-01-15T10:30:00Z
--   }
