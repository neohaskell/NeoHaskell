-- | Response types for ACS transactional email integration.
--
-- Provides the 'Response' record decoded from ACS's HTTP 202 response.
-- The ACS wire field is @id@; the Haskell field is 'operationId'.
-- Phase 10 will implement the custom FromJSON instance that maps the two.
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


-- Stub: generic ToJSON encodes as {"operationId":"..."} — mismatches the wire
-- field "id", which the real phase-10 instance will fix.
instance Json.ToJSON Response


-- Stub: always fails so every round-trip assertion is red.
-- Phase 10 will replace this with a custom parser reading the "id" key.
instance Json.FromJSON Response where
  parseJSON _ = Json.fail "Response.fromJSON stub: not implemented"
