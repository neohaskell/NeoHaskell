module Service.Query.Checkpoint (
  CheckpointStore (..),
  CheckpointState (..),
  new,
) where

import Array qualified
import Basics
import Bytes qualified
import Data.Functor.Contravariant ((>$<))
import Data.Semigroup ((<>))
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection qualified as ConnectionSettingConnection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pool qualified as HasqlPool
import Hasql.Pool.Config qualified as HasqlPoolConfig
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Prelude qualified
import Result (Result (..), fromEither)
import Service.Event.StreamPosition (StreamPosition (..))
import Service.EventStore.Postgres (PostgresEventStore (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


data CheckpointStore = CheckpointStore
  { getPositions :: Task Text (Map Text StreamPosition)
  , setPosition :: Text -> StreamPosition -> Task Text Unit
  , getMinPosition :: Task Text (Maybe StreamPosition)
  }


data CheckpointState
  = CheckpointValid StreamPosition
  | CheckpointStale
  | CheckpointAbsent


-- | Create a new PostgreSQL-backed CheckpointStore.
--
-- Creates the query_checkpoint table if it doesn't exist.
new :: PostgresEventStore -> Task Text CheckpointStore
new config = do
  pool <- createPool config
    |> Task.mapError (\err -> [fmt|Failed to create database pool: #{err}|])

  initializeTable pool

  Task.yield
    CheckpointStore
      { getPositions = getPositionsImpl pool
      , setPosition = setPositionImpl pool
      , getMinPosition = getMinPositionImpl pool
      }


-- Internal: create a connection pool from PostgresEventStore config
createPool :: PostgresEventStore -> Task Text HasqlPool.Pool
createPool cfg = do
  let params =
        ConnectionSettingConnection.params
          [ Param.host cfg.host
          , Param.port (fromIntegral cfg.port)
          , Param.dbname cfg.databaseName
          , Param.user cfg.user
          , Param.password cfg.password
          ]
  let settings = [params |> ConnectionSetting.connection]
  let poolConfig =
        [ HasqlPoolConfig.staticConnectionSettings settings
        , HasqlPoolConfig.agingTimeout 300
        , HasqlPoolConfig.idlenessTimeout 60
        ]
          |> HasqlPoolConfig.settings
  HasqlPool.acquire poolConfig
    |> Task.fromIO


-- | Run a session on the pool.
runPool :: HasqlPool.Pool -> Session.Session a -> Task Text a
runPool pool session = do
  result <-
    HasqlPool.use pool session
      |> Task.fromIO
      |> Task.map fromEither
  case result of
    Ok val -> Task.yield val
    Err err -> Task.throw [fmt|Database error: #{show err}|]


-- | Create the query_checkpoint table if it doesn't exist.
initializeTable :: HasqlPool.Pool -> Task Text ()
initializeTable pool = do
  let sql = "CREATE TABLE IF NOT EXISTS query_checkpoint ( query_name TEXT NOT NULL PRIMARY KEY, last_global_position BIGINT NOT NULL );"
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) Encoders.noParams Decoders.noResult True
  runPool pool (Session.statement () statement)


-- | Get all checkpoint positions.
getPositionsImpl :: HasqlPool.Pool -> Task Text (Map Text StreamPosition)
getPositionsImpl pool = do
  let sql = "SELECT query_name, last_global_position FROM query_checkpoint"
  let encoder = Encoders.noParams
  let decoder = Decoders.rowList do
        queryName <- Decoders.column (Decoders.nonNullable Decoders.text)
        position <- Decoders.column (Decoders.nonNullable Decoders.int8)
        Prelude.pure (queryName, position)
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) encoder decoder True
  rows <- runPool pool (Session.statement () statement)
  Task.yield (rows |> Array.fromLinkedList |> Array.reduce (\(k, v) acc -> acc |> Map.set k (StreamPosition v)) Map.empty)


-- | Set or update a checkpoint position for a query.
setPositionImpl :: HasqlPool.Pool -> Text -> StreamPosition -> Task Text Unit
setPositionImpl pool queryName position = do
  let (StreamPosition pos) = position
  let sql = "INSERT INTO query_checkpoint (query_name, last_global_position) VALUES ($1, $2) ON CONFLICT (query_name) DO UPDATE SET last_global_position = EXCLUDED.last_global_position"
  let encoder =
        (Prelude.fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (Prelude.snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
  let decoder = Decoders.noResult
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) encoder decoder True
  runPool pool (Session.statement (queryName, pos) statement)


-- | Get the minimum checkpoint position across all queries.
getMinPositionImpl :: HasqlPool.Pool -> Task Text (Maybe StreamPosition)
getMinPositionImpl pool = do
  let sql = "SELECT MIN(last_global_position) FROM query_checkpoint"
  let encoder = Encoders.noParams
  let decoder = Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.int8))
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) encoder decoder True
  result <- runPool pool (Session.statement () statement)
  case result of
    Nothing -> Task.yield Nothing
    Just pos -> Task.yield (Just (StreamPosition pos))
