-- | Authentication options for endpoints.
-- Permission checks should be done in the command's decide method, not here.
module Auth.Options (
  AuthOptions (..),
) where

import Basics


-- | Authentication requirements for an endpoint.
-- Note: Permission checks belong in the command's decide method.
data AuthOptions
  = -- | No authentication required (public endpoint)
    Everyone
  | -- | Valid JWT required
    Authenticated
  deriving (Generic, Show, Eq)
