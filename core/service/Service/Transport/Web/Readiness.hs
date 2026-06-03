module Service.Transport.Web.Readiness (
  ReadinessConfig (..),
  handleReadinessRequest,
  handleQueryReadinessRequest,
) where

import Basics
import Task (Task)
import Task qualified
import Text (Text)


-- | Configuration for the /ready endpoint.
data ReadinessConfig = ReadinessConfig
  { readinessPath :: Text
    -- ^ URL path for the readiness endpoint (default: "ready").
  , includeQueryStatus :: Bool
    -- ^ Whether /ready includes per-query lag and names (default: True).
  }
  deriving (Eq, Show)


-- | Handle GET /ready.
--
-- Returns Ok Unit when the handler runs; the HTTP status code (200 or 503)
-- and response body are determined by the caller based on the subscriber's
-- readiness state. This function exists as a placeholder entry-point for the
-- route registration and is exercised by unit tests against the readiness state.
handleReadinessRequest :: Task Text Unit
handleReadinessRequest = Task.yield unit


-- | Handle GET /queries/{name} readiness degradation.
--
-- Returns Ok Unit when the handler runs. The caller is responsible for
-- checking per-query readiness via Subscriber.readinessOfQuery and writing
-- the appropriate HTTP response (200, 503, or 404).
handleQueryReadinessRequest :: Task Text Unit
handleQueryReadinessRequest = Task.yield unit
