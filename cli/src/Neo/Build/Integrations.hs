module Neo.Build.Integrations (
  buildIntegrations,
) where

import Console qualified
import Core
import Integration qualified
import Neo.Build.Core
import System.Exit qualified as GhcExit
import Task qualified


buildIntegrations :: () -> BuildEntity -> BuildEvent -> Integration.Outbound
buildIntegrations _config _entity event = case event of
  BuildStarted _ ->
    Integration.batch
      [ Integration.outbound BuildStartedReporter
      ]
  BuildSucceeded _ ->
    Integration.batch
      [ Integration.outbound BuildCompletedReporter
      ]
  BuildFailed _ ->
    Integration.batch
      [ Integration.outbound BuildFailedReporter
      ]
  _ -> Integration.none


data BuildStartedReporter = BuildStartedReporter


instance Integration.ToAction BuildStartedReporter where
  toAction _ = Integration.action \_ctx -> do
    Console.print "🔨 Build started..."
      |> Task.ignoreError
    Integration.noCommand


data BuildCompletedReporter = BuildCompletedReporter


instance Integration.ToAction BuildCompletedReporter where
  toAction _ = Integration.action \_ctx -> do
    Console.print "✓ Build complete"
      |> Task.ignoreError
    _ <- Task.fromIO GhcExit.exitSuccess
    Integration.noCommand


data BuildFailedReporter = BuildFailedReporter


instance Integration.ToAction BuildFailedReporter where
  toAction _ = Integration.action \_ctx -> do
    Console.print "✗ Build failed"
      |> Task.ignoreError
    _ <- Task.fromIO (GhcExit.exitWith (GhcExit.ExitFailure 1))
    Integration.noCommand
