module App (app) where

import Core
import Neo.Build.Integrations (buildIntegrations)
import Neo.Build.Service qualified as BuildService
import Neo.Project.Integrations (projectIntegrations)
import Neo.Project.Service qualified as ProjectService
import Neo.Run.Integrations (appIntegrations)
import Neo.Run.Service qualified as RunService
import Service.Application qualified as Application
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.Transport.Cli (CliTransport (..))


app :: Application.Application
app =
  Application.new
    |> Application.withEventStore @() (\_ -> SimpleEventStore {basePath = ".neo/events", persistent = True})
    |> Application.withTransport (CliTransport {programName = "neo", version = "0.1.0", description = "The NeoHaskell CLI"})
    |> Application.withService ProjectService.service
    |> Application.withService BuildService.service
    |> Application.withService RunService.service
    |> Application.withOutbound projectIntegrations
    |> Application.withOutbound buildIntegrations
    |> Application.withOutbound appIntegrations
