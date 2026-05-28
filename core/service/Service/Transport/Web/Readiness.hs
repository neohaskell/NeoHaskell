module Service.Transport.Web.Readiness (
  ReadinessConfig (..),
  handleReadinessRequest,
  handleQueryReadinessRequest,
) where

import Basics
import Task (Task)
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
-- Stub — not implemented.
handleReadinessRequest :: Task Text Unit
handleReadinessRequest = panic "not implemented: Service.Transport.Web.Readiness.handleReadinessRequest"


-- | Handle GET /queries/{name} readiness degradation.
--
-- Stub — not implemented.
handleQueryReadinessRequest :: Task Text Unit
handleQueryReadinessRequest = panic "not implemented: Service.Transport.Web.Readiness.handleQueryReadinessRequest"
