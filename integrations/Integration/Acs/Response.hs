-- | Response types for ACS transactional email integration.
--
-- Provides the 'Response' record decoded from ACS's HTTP 202 response.
-- The ACS wire field is @id@; the Haskell field is 'operationId'.
-- Custom 'FromJSON' and 'ToJSON' instances map between the two so that
-- round-trips and ACS wire-format decoding both work correctly.
module Integration.Acs.Response
  ( Response (..)
  ) where

import Basics
import Json qualified
import Text (Text)


-- | ACS's success payload for an accepted email send.
-- ACS is asynchronous; 'operationId' identifies the send for later
-- delivery-status polling.  Decoded from the ACS wire field @id@ (not @operationId@).
data Response = Response
  { operationId :: Text
  } deriving (Eq, Show, Generic)


-- | Custom ToJSON: writes 'operationId' using the ACS wire key @id@,
-- ensuring that round-trips match the ACS wire format.
instance Json.ToJSON Response where
  toJSON resp =
    Json.object [("id", Json.encode resp.operationId)]


-- | Custom FromJSON: reads the ACS wire key @id@ into 'operationId'.
-- This is the mapping required by ADR-0065; the wire field is @id@, not
-- @operationId@.
instance Json.FromJSON Response where
  parseJSON = Json.withObject "Response" \obj -> do
    opId <- obj Json..: "id"
    Json.yield Response { operationId = opId }
