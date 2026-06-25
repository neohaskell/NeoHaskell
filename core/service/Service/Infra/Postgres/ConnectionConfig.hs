module Service.Infra.Postgres.ConnectionConfig (
  ConnectionParams (..),
  toConnectionParams,
  toPoolConfig,
  logPoolObservation,
) where

-- | Single shared Postgres connection-settings builder (ADR-0062).

import Core
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting qualified as Hasql
import Hasql.Connection.Setting.Connection qualified as ConnectionSettingConnection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Pool.Config qualified as HasqlPoolConfig
import Hasql.Pool.Observation (ConnectionStatus (..), ConnectionTerminationReason (..), Observation (..))
import Log qualified
import Prelude qualified
import Task qualified


-- | Internal connection inputs shared by all three Postgres pools.
-- Derives (Eq, Ord) only — Show is dropped per sec-04-001 so the password
-- field can never be rendered by an accidental show/trace/exception.
data ConnectionParams = ConnectionParams
  { host :: Text,
    databaseName :: Text,
    user :: Text,
    password :: Text,
    port :: Int
  }
  deriving (Eq, Ord)


-- | The single place libpq connection params are constructed (ADR-0062 §2).
-- Carries the four ADR-0037 keepalives for every pool.
toConnectionParams :: ConnectionParams -> LinkedList Hasql.Setting
toConnectionParams cfg = do
  let params =
        ConnectionSettingConnection.params
          [ Param.host cfg.host,
            Param.port (fromIntegral cfg.port),
            Param.dbname cfg.databaseName,
            Param.user cfg.user,
            Param.password cfg.password,
            -- TCP keepalive: detect dead connections in cloud
            -- environments (ADR-0037, #397) — now on ALL three pools.
            Param.other "keepalives" "1",
            Param.other "keepalives_idle" "30",
            Param.other "keepalives_interval" "10",
            Param.other "keepalives_count" "5"
            -- WI-5 (#684) adds: Param.other "sslmode" "<mode>" here.
          ]
  [params |> ConnectionSetting.connection]


-- | The single place 'Hasql.Pool.Config' is constructed (ADR-0062 §3).
-- Parameterised by 'poolSize' so each pool keeps its ADR-0060 size.
toPoolConfig :: Int -> LinkedList Hasql.Setting -> HasqlPoolConfig.Config
toPoolConfig poolSize settings =
  [ HasqlPoolConfig.staticConnectionSettings settings,
    HasqlPoolConfig.size poolSize,
    HasqlPoolConfig.agingTimeout 300,
    HasqlPoolConfig.idlenessTimeout 60,
    HasqlPoolConfig.observationHandler logPoolObservation
  ]
    |> HasqlPoolConfig.settings


-- | Log connection pool lifecycle events for observability.
-- Only logs termination events to avoid overhead under high load.
-- See ADR-0027 for rationale.
logPoolObservation :: Observation -> Prelude.IO ()
logPoolObservation observation = case observation of
  ConnectionObservation _uuid status -> case status of
    TerminatedConnectionStatus reason -> case reason of
      AgingConnectionTerminationReason ->
        ((Log.debug "[Pool] Connection terminated (aging timeout)" |> Task.ignoreError :: Task Text Unit) |> Task.runOrPanic)
      IdlenessConnectionTerminationReason ->
        ((Log.debug "[Pool] Connection terminated (idleness timeout)" |> Task.ignoreError :: Task Text Unit) |> Task.runOrPanic)
      NetworkErrorConnectionTerminationReason err ->
        ((Log.critical [fmt|[Pool] Connection terminated (network error: #{show err})|] |> Task.ignoreError :: Task Text Unit) |> Task.runOrPanic)
      ReleaseConnectionTerminationReason ->
        Prelude.pure ()
      InitializationErrorTerminationReason err ->
        ((Log.critical [fmt|[Pool] Connection terminated (init error: #{show err})|] |> Task.ignoreError :: Task Text Unit) |> Task.runOrPanic)
    _ -> Prelude.pure ()
