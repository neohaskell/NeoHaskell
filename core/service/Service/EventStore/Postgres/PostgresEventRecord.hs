module Service.EventStore.Postgres.PostgresEventRecord where

import Core
import Hasql.Decoders
import Json qualified
import Mappable qualified
import Uuid qualified


data PostgresEventRecord = PostgresEventRecord
  { eventId :: Uuid,
    globalPosition :: Int64,
    localPosition :: Int64,
    inlinedStreamId :: Text,
    entityName :: Text,
    eventData :: Json.Value,
    metadata :: Json.Value
  }
  deriving (Eq, Show, Ord, Generic)


instance Default PostgresEventRecord where
  def =
    PostgresEventRecord
      { eventId = def,
        globalPosition = def,
        localPosition = def,
        inlinedStreamId = def,
        entityName = def,
        eventData = def,
        metadata = def
      }


rowDecoder :: Row PostgresEventRecord
rowDecoder = do
  eventId <- nonNullable uuid' |> column
  globalPosition <- nonNullable int8 |> column
  localPosition <- nonNullable int8 |> column
  inlinedStreamId <- nonNullable text |> column
  entityName <- nonNullable text |> column
  eventData <- nonNullable jsonb |> column
  metadata <- nonNullable jsonb |> column
  pure
    PostgresEventRecord
      { eventId,
        globalPosition,
        localPosition,
        inlinedStreamId,
        entityName,
        eventData,
        metadata
      }


uuid' :: Value Uuid
uuid' =
  uuid
    |> Mappable.fmap (Uuid.fromLegacy)
