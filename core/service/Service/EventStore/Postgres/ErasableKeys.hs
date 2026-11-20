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
  ScopedErasure (..),
  eraseScoped,
) where

import Basics
import Bytes qualified
import Data.ByteString qualified as ByteString
import Data.Functor.Contravariant ((>$<))
import Data.Semigroup ((<>))
import Default (Default (..))
import Erasable.Cipher (Key)
import Erasable.Cipher qualified as Cipher
import Erasable.Internal (KeyState (..))
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Event.EntityName qualified as EntityName
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core (ErasureTarget (..))
import Text (Text)


createTable :: Session.Session ()
createTable =
  do
    Session.sql
      "CREATE TABLE IF NOT EXISTS erasable_keys (reference TEXT PRIMARY KEY, key_bytes BYTEA NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now())"
    Session.sql "ALTER TABLE erasable_keys ADD COLUMN IF NOT EXISTS application_scope TEXT NOT NULL DEFAULT ''"
    Session.sql "ALTER TABLE erasable_keys ADD COLUMN IF NOT EXISTS owner_entity TEXT NOT NULL DEFAULT ''"
    Session.sql "ALTER TABLE erasable_keys ADD COLUMN IF NOT EXISTS owner_stream TEXT NOT NULL DEFAULT ''"


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


data ScopedErasure = ErasedNow | AlreadyErased | UnknownReference | ScopeDenied | CorruptKey
  deriving (Eq, Show)


instance Default ScopedErasure where
  def = UnknownReference


eraseScoped :: Text -> ErasureTarget -> Session.Session ScopedErasure
eraseScoped applicationScope target = do
  erased <- updateScoped applicationScope target
  case erased of
    Just _ -> pure ErasedNow
    Nothing -> do
      stored <- readScoped target.reference
      case stored of
        Nothing -> pure UnknownReference
        Just (storedApplication, storedEntity, storedStream, maybeKey) ->
          if storedApplication != applicationScope
            || storedEntity != EntityName.toText target.entityName
            || storedStream != StreamId.toText target.streamId
            then pure ScopeDenied
            else case maybeKey of
              Nothing -> pure AlreadyErased
              Just _ -> pure CorruptKey


updateScoped :: Text -> ErasureTarget -> Session.Session (Maybe Text)
updateScoped applicationScope target = do
  let statement :: Statement (Text, Text, Text, Text) (Maybe Text) =
        Statement
          "UPDATE erasable_keys SET key_bytes = NULL WHERE reference = $1 AND application_scope = $2 AND owner_entity = $3 AND owner_stream = $4 AND octet_length(key_bytes) = 32 RETURNING reference"
          ( ((\(reference, _, _, _) -> reference) >$< Encoders.param (Encoders.nonNullable Encoders.text))
              <> ((\(_, scope, _, _) -> scope) >$< Encoders.param (Encoders.nonNullable Encoders.text))
              <> ((\(_, _, entity, _) -> entity) >$< Encoders.param (Encoders.nonNullable Encoders.text))
              <> ((\(_, _, _, stream) -> stream) >$< Encoders.param (Encoders.nonNullable Encoders.text))
          )
          (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.text)))
          True
  Session.statement
    ( target.reference,
      applicationScope,
      EntityName.toText target.entityName,
      StreamId.toText target.streamId
    )
    statement


readScoped :: Text -> Session.Session (Maybe (Text, Text, Text, Maybe ByteString.ByteString))
readScoped reference = do
  let row = do
        applicationScope <- Decoders.column (Decoders.nonNullable Decoders.text)
        entity <- Decoders.column (Decoders.nonNullable Decoders.text)
        stream <- Decoders.column (Decoders.nonNullable Decoders.text)
        key <- Decoders.column (Decoders.nullable Decoders.bytea)
        pure (applicationScope, entity, stream, key)
  let statement :: Statement Text (Maybe (Text, Text, Text, Maybe ByteString.ByteString)) =
        Statement
          "SELECT application_scope, owner_entity, owner_stream, key_bytes FROM erasable_keys WHERE reference = $1"
          (Encoders.param (Encoders.nonNullable Encoders.text))
          (Decoders.rowMaybe row)
          True
  Session.statement reference statement


referenceExists :: Text -> Session.Session Bool
referenceExists reference = do
  let statement :: Statement Text Bool =
        Statement
          "SELECT EXISTS (SELECT 1 FROM erasable_keys WHERE reference = $1)"
          (Encoders.param (Encoders.nonNullable Encoders.text))
          (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
          True
  Session.statement reference statement
