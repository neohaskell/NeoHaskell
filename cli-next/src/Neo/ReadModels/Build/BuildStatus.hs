module Neo.ReadModels.Build.BuildStatus where

import Core
import Neo.Domain.Build.Types


-- ================================================================================
-- BUILD STATUS READ MODEL
-- ================================================================================

data BuildStatusView = BuildStatusView
  { buildId :: BuildId,
    currentPhase :: Text,
    progress :: Int, -- 0-100
    elapsedInt :: Int, -- seconds
    recentLogs :: Array LogEntry
  }
  deriving (Show)

-- Example: BuildStatusView
--   { buildId = "build-abc123"
--   , currentPhase = "Running Nix Build"
--   , progress = 65
--   , elapsedInt = 45
--   , recentLogs = [LogEntry 2024-01-15T10:30:45Z Info "Compiling Main.hs...", ...]
--   }
