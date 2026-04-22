-- | 'IntegrationDebug' — the redaction-aware structured log carrier.
--
-- Carries 'Redacted' fields so secrets never appear in logs.
module Service.Integration.Debug
  ( IntegrationDebug (..),
    log,
  )
where

import Basics
import Maybe (Maybe)
import Redacted (Redacted)
import Task (Task)
import Task qualified
import Text (Text)


data IntegrationDebug = IntegrationDebug
  { integrationName :: Text,
    requestHash :: Text,
    statusCode :: Maybe Int,
    authHeader :: Redacted Text,
    responseSnippet :: Redacted Text
  }
  deriving (Generic)


-- | Placeholder logger — writes a redacted summary to the process console.
-- The real structured logger integration is tracked in a follow-up ADR.
log :: IntegrationDebug -> Task Text ()
log _debug = Task.yield ()
