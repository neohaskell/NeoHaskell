module Neo.ReadModels.Build.BuildResult where

import Core
import Neo.Domain.Build.Types


-- ================================================================================
-- BUILD RESULT READ MODEL
-- ================================================================================

data BuildResultView = BuildResultView
  { buildId :: BuildId,
    success :: Bool,
    duration :: Int, -- seconds
    outputPath :: Maybe Path,
    errorSummary :: Maybe ErrorDetails,
    timestamp :: Time
  }
  deriving (Show)

-- Example: BuildResultView
--   { buildId = "build-abc123"
--   , success = True
--   , duration = 135
--   , outputPath = Just "/nix/store/xyz789-my-neohaskell-project"
--   , errorSummary = Nothing
--   , timestamp = 2024-01-15T10:32:15Z
--   }