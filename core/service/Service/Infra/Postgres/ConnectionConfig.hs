module Service.Infra.Postgres.ConnectionConfig (
  SslMode (..),
  ConnectionParams (..),
  ResolvedParams (..),
  validatePort,
  sslModeToText,
  textToSslMode,
  resolveParams,
  toParamPairs,
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
import LinkedList qualified
import Log qualified
import Result qualified
import Task qualified
import Service.Infra.Postgres.SslMode (SslMode (..), sslModeToText, textToSslMode)
import Prelude qualified


-- | Internal connection inputs shared by all three Postgres pools.
-- Derives (Eq, Ord) only -- Show is dropped per sec-04-001 so the password
-- field can never be rendered by an accidental show/trace/exception.
data ConnectionParams = ConnectionParams
  { host :: Text,
    databaseName :: Text,
    user :: Text,
    password :: Text,
    port :: Int,
    -- WI-5 (#684): optional TLS hardening. 'SslModeUnset' (the default the
    -- pool configs pass) emits no sslmode param, so libpq keeps its 'prefer'
    -- default and localhost/dev is unchanged.
    sslMode :: SslMode,
    -- Optional CA-bundle path for 'verify-full' (root CAs only; never an
    -- intermediate or server cert).
    sslRootCert :: Maybe Text
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
    keepalivesCount :: Text,
    -- WI-5 (#684): carried through unchanged so the spec can assert exactly
    -- which TLS params each mode emits. 'SslModeUnset' here means
    -- 'toConnectionParams' appends no sslmode param.
    sslMode :: SslMode,
    sslRootCert :: Maybe Text
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
          keepalivesCount = "5",
          -- WI-5 (#684): copy both TLS fields through unchanged.
          sslMode = cfg.sslMode,
          sslRootCert = cfg.sslRootCert
        }


-- | The base libpq (key, value) params every pool always sends: the five
-- connection coordinates plus the four ADR-0037 keepalive entries. Plain
-- inspectable data (see 'toParamPairs').
baseParamPairs :: ResolvedParams -> LinkedList (Text, Text)
baseParamPairs resolved =
  [ ("host", resolved.host),
    ("port", portToText resolved.port),
    ("dbname", resolved.databaseName),
    ("user", resolved.user),
    ("password", resolved.password),
    -- TCP keepalive: detect dead connections in cloud environments
    -- (ADR-0037, #397) -- on ALL three pools.
    ("keepalives", resolved.keepalives),
    ("keepalives_idle", resolved.keepalivesIdle),
    ("keepalives_interval", resolved.keepalivesInterval),
    ("keepalives_count", resolved.keepalivesCount)
  ]


-- | Render the validated port to its libpq decimal text form. A standalone
-- helper so the 'fmt' splice only ever sees a bare variable -- the quasiquoter
-- does not parse a 'resolved.port' record-dot accessor (OverloadedRecordDot).
portToText :: Word16 -> Text
portToText p = [fmt|#{p}|]


-- | Conditional TLS (key, value) params appended AFTER the base params.
-- Returns [] for 'SslModeUnset' (byte-identical to pre-WI-5 — the dev/CI
-- no-regression guarantee, ADR-0064 §2). For any set mode it emits the
-- sslmode param; for a verifying mode WITH a root cert it additionally emits
-- sslrootcert (and for verify-full: channel_binding=require). NEVER emits
-- sslcert/sslkey (ADR-0064 §3).
sslParamPairs :: ResolvedParams -> LinkedList (Text, Text)
sslParamPairs resolved =
  case sslModeToText resolved.sslMode of
    Nothing -> []
    Just token ->
      ("sslmode", token) : rootCertParamPairs resolved.sslMode resolved.sslRootCert


-- | The verifying-mode companion params. 'sslrootcert' is emitted ONLY for the
-- verifying modes ('SslModeVerifyCa' / 'SslModeVerifyFull') and only when a
-- root cert path is supplied — a root cert is meaningless for the non-verifying
-- modes ('disable'/'allow'/'prefer'/'require'), so it is NEVER sent for them
-- even if the environment provides a path (ADR-0064 §3). 'verify-full'
-- additionally emits channel_binding=require. Forwards a PATH only — pins no
-- cert (ADR-0064 §"Root-only trust"). NEVER emits sslcert/sslkey.
rootCertParamPairs :: SslMode -> Maybe Text -> LinkedList (Text, Text)
rootCertParamPairs mode maybeRootCert =
  case (mode, maybeRootCert) of
    (SslModeVerifyCa, Just path) ->
      [("sslrootcert", path)]
    (SslModeVerifyFull, Just path) ->
      [ ("sslrootcert", path),
        ("channel_binding", "require")
      ]
    _ ->
      []


-- | The complete, ORDERED libpq (key, value) parameter set — base params first,
-- then any conditional TLS params — that the single connection 'Setting' is
-- built from (ADR-0062 section 2). Carries the four ADR-0037 keepalives for
-- every pool and fails fast on an out-of-range port (see 'validatePort').
--
-- Exposed as plain inspectable data because the hasql 'Param'/'Setting' types
-- are opaque (no Eq/Show) and the modules that render them to a connection
-- string are internal to hasql, so this is the ONLY place a test can assert the
-- exact params — including that 'host' survives a set sslMode (the #694
-- regression) and that no sslcert/sslkey ever leaks in (ADR-0064 §3).
toParamPairs :: ConnectionParams -> Result Text (LinkedList (Text, Text))
toParamPairs cfg =
  cfg
    |> resolveParams
    |> Result.map \resolved -> baseParamPairs resolved ++ sslParamPairs resolved


-- | The single place libpq connection params are constructed (ADR-0062
-- section 2). Returns a 'Result' so the three pools surface the typed error in
-- their own error channel.
--
-- Every param — base AND the conditional TLS params — goes into ONE
-- 'ConnectionSettingConnection.params', i.e. a SINGLE 'ConnectionSetting.connection'
-- (#694). hasql's 'Hasql.Pool.Config.staticConnectionSettings' applies each
-- 'ConnectionSetting.connection' by REPLACING the whole connection string
-- (last-wins, not merge), so emitting the TLS params as a second connection
-- Setting silently dropped host/port/dbname and libpq fell back to the local
-- Unix socket. Returning a one-element list keeps the 'LinkedList Setting'
-- shape every pool already threads through 'toPoolConfig'.
toConnectionParams :: ConnectionParams -> Result Text (LinkedList Setting)
toConnectionParams cfg =
  cfg
    |> toParamPairs
    |> Result.map \pairs ->
      [ pairs
          |> LinkedList.map (\(key, value) -> Param.other key value)
          |> ConnectionSettingConnection.params
          |> ConnectionSetting.connection
      ]


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
