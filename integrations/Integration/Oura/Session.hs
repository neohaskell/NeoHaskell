module Integration.Oura.Session
  ( Session (..)
  )
where

import Array (Array)
import Integration.Oura.Types (SessionData)
import Maybe (Maybe)
import Text (Text)


-- Note: Session uses datetime (ISO 8601 with timezone), not date
-- Example: "2021-11-01T00:00:00-08:00"
-- IMPORTANT: If using positive timezone offsets, the + must be URL-encoded as %2B
-- Recommendation: Use negative offsets (e.g., "-08:00") or handle encoding in Internal.hs
data Session command = Session
  { userId :: Text
  , startDatetime :: Text    -- ISO 8601 with timezone (prefer negative offsets)
  , endDatetime :: Text      -- ISO 8601 with timezone (prefer negative offsets)
  , onSuccess :: Array SessionData -> command
  , onError :: Maybe (Text -> command)
  }
