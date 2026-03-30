-- | Pagination types and utilities for query endpoints.
--
-- Provides 'QueryPageRequest' for parsing client pagination parameters
-- and 'QueryPageResponse' for wrapping paginated results with metadata.
--
-- Pagination is applied automatically by the query endpoint handler.
-- Jess doesn't need to do anything — all query endpoints are paginated
-- by default with sensible limits.
--
-- To override the maximum results per page for a specific query, define
-- @maxResults :: Int@ in the query module before calling @deriveQuery@.
module Service.Query.Pagination (
  -- * Types
  QueryPageRequest (..),
  QueryPageResponse (..),

  -- * Constants
  defaultLimit,
  absoluteMaxLimit,

  -- * Parsing
  parsePageRequest,
) where

import Array (Array)
import Basics
import Json qualified
import Maybe (Maybe (..))
import Maybe qualified
import Text (Text)
import Text qualified


-- | Pagination request parameters parsed from query string.
--
-- @
-- -- Parse from raw query params:
-- let pageRequest = parsePageRequest (Just "25") (Just "50")
-- -- QueryPageRequest { limit = 25, offset = 50 }
-- @
data QueryPageRequest = QueryPageRequest
  { limit :: !Int
  , offset :: !Int
  }
  deriving (Eq, Show, Generic)


-- | Paginated response wrapper with metadata.
--
-- The @total@ field reflects the count of items AFTER authorization filtering.
-- It never exposes counts of records the user is not authorized to see.
--
-- The @effectiveLimit@ field surfaces any capping that was applied.
-- When the client requests @limit=5000@ but the server caps to 1000,
-- @effectiveLimit@ will be 1000 so the client can detect the difference.
--
-- @
-- -- Example response:
-- -- { "items": [...], "total": 142, "hasMore": true, "effectiveLimit": 100 }
-- @
data QueryPageResponse a = QueryPageResponse
  { items :: !(Array a)
  , total :: !Int
  , hasMore :: !Bool
  , effectiveLimit :: !Int
  }
  deriving (Eq, Show, Generic)

instance (Json.ToJSON a) => Json.ToJSON (QueryPageResponse a)
instance (Json.FromJSON a) => Json.FromJSON (QueryPageResponse a)


-- | Default page size when no limit is specified (100).
defaultLimit :: Int
defaultLimit = 100


-- | Absolute maximum page size — hard cap (1000).
absoluteMaxLimit :: Int
absoluteMaxLimit = 1000


-- | Parse pagination parameters from raw query string values.
--
-- Invalid or missing values fall back to defaults.
-- Limit is clamped to @[1, absoluteMaxLimit]@.
-- Offset is clamped to @[0, 10_000_000]@.
--
-- @
-- parsePageRequest (Just "25") (Just "50")
-- -- QueryPageRequest { limit = 25, offset = 50 }
--
-- parsePageRequest Nothing Nothing
-- -- QueryPageRequest { limit = 100, offset = 0 }
--
-- parsePageRequest (Just "abc") (Just "-5")
-- -- QueryPageRequest { limit = 100, offset = 0 }
-- @
parsePageRequest :: Maybe Text -> Maybe Text -> QueryPageRequest
parsePageRequest maybeLimitText maybeOffsetText = do
  let parsedLimit =
        maybeLimitText
          |> Maybe.andThen Text.toInt
          |> Maybe.withDefault defaultLimit
  let parsedOffset =
        maybeOffsetText
          |> Maybe.andThen Text.toInt
          |> Maybe.withDefault 0
  QueryPageRequest
    { limit = parsedLimit |> max 1 |> min absoluteMaxLimit
    , offset = parsedOffset |> max 0 |> min 10_000_000
    }
