module Service.Infra.Postgres.ConnectionConfig (
  ConnectionParams (..),
  ResolvedParams (..),
  validatePort,
  resolveParams,
  toConnectionParams,
  toPoolConfig,
  logPoolObservation,
) where

-- | Single shared Postgres connection-settings builder (ADR-0062).

import Core
import Data.Word (Word16)
import Hasql.Connection.Setting (Setting)
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection qualified as ConnectionSettingConnection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Pool.Config (Config)
import Hasql.Pool.Config qualified as HasqlPoolConfig
import Hasql.Pool.Observation (ConnectionStatus (..), ConnectionTerminationReason (..), Observation (..))
import Log qualified
import Result qualified
import Task qualified
import Prelude qualified


-- | Internal connection inputs shared by all three Postgres pools.
-- Derives (Eq, Ord) only -- Show is dropped per sec-04-001 so the password
-- field can never be rendered by an accidental show/trace/exception.
data ConnectionParams = ConnectionParams
  { host :: Text,
    databaseName :: Text,
    user :: Text,
    password :: Text,
    port :: Int
  }
  deriving (Eq, Ord)


-- | The fully resolved, INSPECTABLE libpq parameter set built from a
-- 'ConnectionParams'. Every value that 'toConnectionParams' hands to libpq
-- is captured here as plain data so tests can assert each one exactly --
-- including the four ADR-0037 keepalive entries, guarding against a dropped
-- or mis-mapped field. 'port' is the validated 'Word16' (1..65535).
--
-- Derives (Eq) only -- Show is dropped for the same password-hygiene reason
-- as 'ConnectionParams' (sec-04-001).
data ResolvedParams = ResolvedParams
  { host :: Text,
    databaseName :: Text,
    user :: Text,
    password :: Text,
    port :: Word16,
    keepalives :: Text,
    keepalivesIdle :: Text,
    keepalivesInterval :: Text,
    keepalivesCount :: Text
  }
  deriving (Eq)


-- | Validate a TCP port. 'Param.port' wants a 'Word16', so a raw
-- 'fromIntegral' would silently wrap an out-of-range 'Int' (-1 -> 65535,
-- 65536 -> 0). Fail fast with a clear typed error instead, mirroring the
-- fail-fast @poolSize > 0@ validation WI-1 added to the pool-acquire paths.
validatePort :: Int -> Result Text Word16
validatePort p =
  case p >= 1 && p <= 65535 of
    True -> Ok (fromIntegral p)
    False -> Err [fmt|port must be in 1..65535, got #{p}|]


-- | The single validation point for connection inputs (ADR-0062 section 2):
-- resolve a 'ConnectionParams' into the inspectable 'ResolvedParams',
-- validating the port along the way. All three pools route through here (via
-- 'toConnectionParams'), so all three get fail-fast port validation.
resolveParams :: ConnectionParams -> Result Text ResolvedParams
resolveParams cfg =
  cfg.port
    |> validatePort
    |> Result.map \validPort ->
      ResolvedParams
        { host = cfg.host,
          databaseName = cfg.databaseName,
          user = cfg.user,
          password = cfg.password,
          port = validPort,
          -- TCP keepalive: detect dead connections in cloud
          -- environments (ADR-0037, #397) -- now on ALL three pools.
          keepalives = "1",
          keepalivesIdle = "30",
          keepalivesInterval = "10",
          keepalivesCount = "5"
        }


-- | The single place libpq connection params are constructed (ADR-0062
-- section 2). Carries the four ADR-0037 keepalives for every pool and fails
-- fast on an out-of-range port (see 'validatePort'). Returns a 'Result' so
-- the three pools surface the typed error in their own error channel.
toConnectionParams :: ConnectionParams -> Result Text (LinkedList Setting)
toConnectionParams cfg =
  cfg
    |> resolveParams
    |> Result.map \resolved -> do
      let params =
            ConnectionSettingConnection.params
              [ Param.host resolved.host,
                Param.port resolved.port,
                Param.dbname resolved.databaseName,
                Param.user resolved.user,
                Param.password resolved.password,
                Param.other "keepalives" resolved.keepalives,
                Param.other "keepalives_idle" resolved.keepalivesIdle,
                Param.other "keepalives_interval" resolved.keepalivesInterval,
                Param.other "keepalives_count" resolved.keepalivesCount
                -- WI-5 (#684) adds: Param.other "sslmode" "<mode>" here.
              ]
      [params |> ConnectionSetting.connection]


-- | The single place 'Hasql.Pool.Config' is constructed (ADR-0062 section 3).
-- Parameterised by 'poolSize' so each pool keeps its ADR-0060 size.
toPoolConfig :: Int -> LinkedList Setting -> Config
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
