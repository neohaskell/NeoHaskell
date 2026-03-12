module Neo.Project.Integrations (
  projectIntegrations,
) where

import Console qualified
import Core
import Integration qualified
import Integration.Command qualified as Command
import Neo.Build.Commands.StartBuild (StartBuild (..))
import Neo.Project.Core
import System.Exit qualified as GhcExit
import Task qualified


projectIntegrations :: () -> ProjectEntity -> ProjectEvent -> Integration.Outbound
projectIntegrations _config _entity event = case event of
  ProjectInitRequested e ->
    Integration.batch
      [ Integration.outbound Command.Emit
          { command = StartBuild
              { projectPath = e.path
              }
          }
      ]
  ProjectInitCompleted _ ->
    Integration.batch
      [ Integration.outbound ProjectCompletedReporter
      ]
  _ -> Integration.none


data ProjectCompletedReporter = ProjectCompletedReporter


instance Integration.ToAction ProjectCompletedReporter where
  toAction _ = Integration.action \_ctx -> do
    Console.print "✓ Project initialized successfully!"
      |> Task.ignoreError
    _ <- Task.fromIO GhcExit.exitSuccess
    Integration.noCommand
