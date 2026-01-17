-- | Declarative authentication requirements for endpoints.
-- This is the DSL Jess uses to specify auth requirements.
module Auth.Options (
  AuthOptions (..),
  AuthOptionsError (..),
) where

import Array (Array)
import Auth.Claims (UserClaims)
import Basics
import Result (Result)
import Text (Text)


-- | Declarative authentication requirements for an endpoint.
data AuthOptions
  = -- | No authentication required
    Everyone
  | -- | Valid JWT required, any permissions
    Authenticated
  | -- | JWT + must have ALL listed permissions
    RequireAllPermissions (Array Text)
  | -- | JWT + must have at least ONE permission
    RequireAnyPermission (Array Text)
  | -- | Custom validation logic
    Custom (UserClaims -> Result AuthOptionsError ())
  deriving (Generic)


-- | Error type for Custom validation
data AuthOptionsError = AuthOptionsError Text
  deriving (Generic, Show, Eq)
