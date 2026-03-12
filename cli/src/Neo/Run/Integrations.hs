module Neo.Run.Integrations (
  appIntegrations,
) where

import Console qualified
import Core
import Integration qualified
import Integration.Command qualified as Command
import Neo.Build.Commands.StartBuild (StartBuild (..))
import Neo.Run.Core
import System.Exit qualified as GhcExit
import Task qualified


appIntegrations :: () -> AppEntity -> AppEvent -> Integration.Outbound
appIntegrations _config _entity event = case event of
  RunRequested e ->
    Integration.batch
      [ Integration.outbound Command.Emit
          { command = StartBuild
              { projectPath = e.projectPath
              }
          }
      ]
  AppStarted _ ->
    Integration.batch
      [ Integration.outbound AppStartedReporter
      ]
  AppStopped _ ->
    Integration.batch
      [ Integration.outbound AppStoppedReporter
      ]


data AppStartedReporter = AppStartedReporter


instance Integration.ToAction AppStartedReporter where
  toAction _ = Integration.action \_ctx -> do
    Console.print "✓ Running on http://localhost:8080"
      |> Task.ignoreError
    Integration.noCommand


data AppStoppedReporter = AppStoppedReporter


instance Integration.ToAction AppStoppedReporter where
  toAction _ = Integration.action \_ctx -> do
    Console.print "✓ App stopped"
      |> Task.ignoreError
    _ <- Task.fromIO GhcExit.exitSuccess
    Integration.noCommand
