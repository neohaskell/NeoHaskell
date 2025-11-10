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


data RelativePosition
  = Start
  | End
  deriving (Eq, Show, Ord, Generic)


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
    (Nothing, Just Forwards) -> "ASC"
    (Nothing, Just Backwards) -> "DESC"
    (Nothing, Nothing) -> "ASC"


toPostgresGlobalPositionComparison :: Maybe ReadDirection -> Text
toPostgresGlobalPositionComparison dir =
  case dir of
    Just Backwards -> "<"
    _ -> ">="


toPostgresPosition :: Maybe RelativePosition -> Maybe ReadDirection -> Int64
toPostgresPosition pos dir =
  case (pos, dir) of
    (Just Start, _) -> 0
    (Just End, _) -> maxValue
    (Nothing, Just Forwards) -> 0
    (Nothing, Just Backwards) -> maxValue
    (Nothing, Nothing) -> 0


toPostgresEntityFilters :: Maybe (Array EntityName) -> Text
toPostgresEntityFilters maybeEntityNames = do
  let entityNames = maybeEntityNames |> Maybe.withDefault Array.empty
  let entityNamesText = entityNames |> Array.map EntityName.toText |> Text.joinWith ", "
  if (entityNames |> Array.length) == 0
    then ""
    else [fmt| AND Entity = ANY (#{entityNamesText})|]
