module Neo.ReadModels.Build.ProjectConfig where

import Core
import Neo.Domain.Build.Types


-- ================================================================================
-- PROJECT CONFIGURATION READ MODEL
-- ================================================================================

data ProjectConfigView = ProjectConfigView
  { projectName :: Text
  , buildTarget :: Text
  , dependencies :: Array Text
  , configPath :: Path
  }
  deriving (Show)
  -- Example: ProjectConfigView
  --   { projectName = "MyNeoHaskellApp"
  --   , buildTarget = "executable"
  --   , dependencies = ["nhcore", "servant", "postgresql-simple"]
  --   , configPath = "/Users/dev/my-project/neo.json"
  --   }