{-| Keep protected event keys and erasure tombstones in PostgreSQL.
Run these sessions on the event store's connection so event insertion can share a transaction with key creation.
-}
module Service.EventStore.Postgres.ErasableKeys (
  KeyState (..),
  createTable,
  insertKey,
  readKey,
  readKeyBytes,
  referenceExists,
  eraseKey,
) where

import Basics
import Bytes qualified
import Data.ByteString qualified as ByteString
import Data.Functor.Contravariant ((>$<))
import Data.Semigroup ((<>))
import Erasable.Cipher (Key)
import Erasable.Cipher qualified as Cipher
import Erasable.Internal (KeyState (..))
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))
import Maybe (Maybe (..))
import Result (Result (..))
import Text (Text)


createTable :: Session.Session ()
createTable =
  Session.sql
    "CREATE TABLE IF NOT EXISTS erasable_keys (reference TEXT PRIMARY KEY, key_bytes BYTEA NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now())"


insertKey :: Text -> Key -> Session.Session ()
insertKey reference key = do
  let statement :: Statement (Text, ByteString.ByteString) () =
        Statement
          "INSERT INTO erasable_keys (reference, key_bytes) VALUES ($1, $2)"
          (((\(ref, _) -> ref) >$< Encoders.param (Encoders.nonNullable Encoders.text))
            <> ((\(_, bytes) -> bytes) >$< Encoders.param (Encoders.nonNullable Encoders.bytea)))
          Decoders.noResult
          True
  Session.statement (reference, Cipher.keyToBytes key |> Bytes.unwrap) statement


readKey :: Text -> Session.Session (Result Text KeyState)
readKey reference = do
  state <- readKeyBytes reference
  case state of
    Nothing -> pure (Ok Missing)
    Just Nothing -> pure (Ok Erased)
    Just (Just bytes) ->
      case Cipher.keyFromBytes (Bytes.fromLegacy bytes) of
        Err err -> pure (Err err)
        Ok key -> pure (Ok (Active key))


readKeyBytes :: Text -> Session.Session (Maybe (Maybe ByteString.ByteString))
readKeyBytes reference = do
  let statement :: Statement Text (Maybe (Maybe ByteString.ByteString)) =
        Statement
          "SELECT key_bytes FROM erasable_keys WHERE reference = $1"
          (Encoders.param (Encoders.nonNullable Encoders.text))
          (Decoders.rowMaybe (Decoders.column (Decoders.nullable Decoders.bytea)))
          True
  Session.statement reference statement


eraseKey :: Text -> Session.Session (Result Text KeyState)
eraseKey reference = do
  let statement :: Statement Text () =
        Statement
          "UPDATE erasable_keys SET key_bytes = NULL WHERE reference = $1 AND key_bytes IS NOT NULL"
          (Encoders.param (Encoders.nonNullable Encoders.text))
          Decoders.noResult
          True
  Session.statement reference statement
  readKey reference


referenceExists :: Text -> Session.Session Bool
referenceExists reference = do
  let statement :: Statement Text Bool =
        Statement
          "SELECT EXISTS (SELECT 1 FROM erasable_keys WHERE reference = $1)"
          (Encoders.param (Encoders.nonNullable Encoders.text))
          (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
          True
  Session.statement reference statement
