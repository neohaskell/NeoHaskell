module Service.EventStore.Postgres.Internal.Core where

import Array qualified
import Core
import Hasql.Session qualified as Session
import Maybe qualified
import Service.Event qualified as Event
import Service.Event.EntityName (EntityName)
import Service.Event.EntityName qualified as EntityName
import Text qualified


data PostgresStoreError
  = SessionError Session.SessionError
  | ConnectionAcquisitionError Text
  | CoreInsertionError Event.InsertionFailure
  deriving (Eq, Show)


data RelativePosition
  = Start
  | End
  | FromAndAfter Event.StreamPosition
  | Before Event.StreamPosition
  deriving (Eq, Show, Ord, Generic)


relativePositionToInt64 :: Maybe RelativePosition -> Maybe Int64
relativePositionToInt64 pos =
  case pos of
    Just (FromAndAfter (Event.StreamPosition p)) -> Just p
    Just (Before (Event.StreamPosition p)) -> Just p
    _ -> Nothing


data ReadDirection
  = Forwards
  | Backwards
  deriving (Eq, Show, Ord, Generic)


batchSize :: Int
batchSize = 500


toPostgresDirection :: Maybe RelativePosition -> Maybe ReadDirection -> Text
toPostgresDirection pos dir = do
  case (pos, dir) of
    (Just Start, _) -> "ASC"
    (Just End, _) -> "DESC"
    (_, Just Forwards) -> "ASC"
    (_, Just Backwards) -> "DESC"
    (_, Nothing) -> "ASC"


toPostgresGlobalPositionComparison :: Maybe ReadDirection -> Text
toPostgresGlobalPositionComparison dir =
  case dir of
    Just Backwards -> "<="
    _ -> ">="


toPostgresPosition :: Maybe RelativePosition -> Maybe ReadDirection -> Int64
toPostgresPosition pos dir =
  case (pos, dir) of
    (Just Start, _) -> 0
    (Just End, _) -> maxValue
    (p, Just Forwards) -> relativePositionToInt64 p |> Maybe.withDefault 0
    (p, Just Backwards) -> relativePositionToInt64 p |> Maybe.withDefault maxValue
    _ -> 0


toPostgresEntityFilters :: Maybe (Array EntityName) -> Text
toPostgresEntityFilters maybeEntityNames = do
  let nameToText (EntityName.EntityName n) = [fmt|'#{n}'|]
  let entityNames = maybeEntityNames |> Maybe.withDefault Array.empty
  let entityNamesText = entityNames |> Array.map nameToText |> Text.joinWith ", "
  if (entityNames |> Array.length) == 0
    then ""
    else [fmt| AND Entity = ANY(ARRAY[#{entityNamesText}])|]
