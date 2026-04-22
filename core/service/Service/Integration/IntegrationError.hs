-- | The narrowed IntegrationError ADT from ADR-0055 §11.
--
-- Payloads are contractually public-safe text only — no request bodies, no
-- headers, no URLs containing tokens, no environment-variable values, no
-- rendered response bodies. Sensitive diagnostic context travels through
-- 'Service.Integration.Debug.IntegrationDebug' instead.
module Service.Integration.IntegrationError
  ( IntegrationError (..),
  )
where

import Basics
import Text (Text)


-- | ADR-0055 §11 narrowed integration error ADT.
--
-- 'AuthenticationFailure' carries no payload deliberately — there is no safe
-- free-form text for "your API key was rejected" that cannot also contain the
-- rejected key or a portion of it.
data IntegrationError
  = TransportFailure Text
  | AuthenticationFailure
  | PermanentFailure Text
  | TransientFailure Text
  | ValidationFailure Text
  deriving (Eq, Show, Generic)
