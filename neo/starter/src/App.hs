module App (app) where

import Core
import Maybe qualified
import Path qualified
import Service.Application (Application)
import Service.Application qualified as Application
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.Transport.Web qualified as WebTransport
import Starter.Config (StarterConfig (..))
import Starter.Counter.Queries.CounterView (CounterView)
import Starter.Counter.Service qualified as Counter

-- To switch to Postgres, see the commented block below.
-- import Service.EventStore.Postgres (PostgresEventStore (..))
-- (and remove the SimpleEventStore import above)


-- | The application.
--
-- The starter ships with the in-memory event store so `cabal run` works
-- with zero external dependencies. Follow the README "Switch to Postgres"
-- steps when you are ready for durability.
app :: Application
app =
  Application.new
    |> Application.withConfig @StarterConfig
    |> Application.withEventStore (\(_ :: StarterConfig) -> SimpleEventStore {basePath = Path.fromText ".neo/events" |> Maybe.getOrDie, persistent = False})
    -- SWITCH TO POSTGRES:
    --   1. Comment out the `withEventStore` line above.
    --   2. Uncomment `withEventStore makePostgresConfig` below.
    --   3. Uncomment `makePostgresConfig` at the bottom of this file.
    --   4. Uncomment the Postgres fields in src/Starter/Config.hs.
    --   5. Uncomment the Postgres import near the top of this file.
    --   6. `cp .env.example .env` (edit if needed) and `docker compose up -d`.
    -- |> Application.withEventStore makePostgresConfig
    |> Application.withTransport WebTransport.server
    |> Application.withService Counter.service
    |> Application.withQuery @CounterView


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
--     }
