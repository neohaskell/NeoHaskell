module App (appEventModel) where

import Core
import Maybe qualified
import Path qualified
import Service.EventModel qualified as EventModel
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.Transport.Web qualified as WebTransport
import Starter.Config (StarterConfig (..))
import Starter.Counter.Feature (counterFlow)


-- To switch to Postgres, see the commented block below.
-- import Service.EventStore.Postgres (PostgresEventStore (..))
-- (and remove the SimpleEventStore import above)

-- | The application's executable Event Model.
--
-- The starter ships with the in-memory event store so `cabal run` works
-- with zero external dependencies. Follow the README "Switch to Postgres"
-- steps when you are ready for durability.
appEventModel :: Result Text EventModel.EventModel
appEventModel = EventModel.eventModel do
  EventModel.appName "Counter"
  EventModel.withConfig @StarterConfig
  EventModel.withEventStore
    (\(_ :: StarterConfig) -> SimpleEventStore {basePath = Path.fromText ".neo/events" |> Maybe.getOrDie, persistent = False})
  -- SWITCH TO POSTGRES:
  --   1. Comment out the `withEventStore` line above.
  --   2. Uncomment `withEventStore makePostgresConfig` below.
  --   3. Uncomment `makePostgresConfig` at the bottom of this file.
  --   4. Uncomment the Postgres fields in src/Starter/Config.hs.
  --   5. Uncomment the Postgres import near the top of this file.
  --   6. `cp .env.example .env` (edit if needed) and `docker compose up -d`.
  -- EventModel.withEventStore makePostgresConfig
  EventModel.withTransport WebTransport.server
  EventModel.features do
    counterFlow


-- | Postgres event store factory. Uncomment once the Postgres fields exist on
-- `StarterConfig` (see src/Starter/Config.hs).
--
-- makePostgresConfig :: StarterConfig -> PostgresEventStore
-- makePostgresConfig config =
--   PostgresEventStore
--     { user = config.dbUser
--     , password = config.dbPassword
--     , host = config.dbHost
--     , databaseName = config.dbName
--     , port = config.dbPort
--     , erasableApplicationScope = "my-application"
--     }
