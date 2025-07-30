module Neo where

import qualified Console
import Core
import qualified Neo.Model as Model

-- Import the organized event model modules
import Neo.Domain.Build.Types
import Neo.Domain.Build.Commands
import Neo.Domain.Build.Events
import Neo.Domain.Build.Entity
import Neo.ReadModels.Build.ProjectConfig
import Neo.ReadModels.Build.BuildStatus
import Neo.ReadModels.Build.BuildResult


run :: Task Text ()
run = do
  Console.print [fmt|Hello, #{Model.model}|]