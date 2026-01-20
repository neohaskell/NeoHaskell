-- | User claims extracted from a validated JWT.
-- This is the primary type Jess interacts with - everything else is internal.
module Auth.Claims (
  UserClaims (..),
) where

import Array (Array)
import Basics
import Json qualified
import Map (Map)
import Maybe (Maybe)
import Text (Text)


-- | User identity extracted from a validated JWT.
-- Available to command handlers when authentication is required.
data UserClaims = UserClaims
  { sub :: Text,
    -- ^ Subject (user ID from OAuth provider)
    email :: Maybe Text,
    -- ^ User email (if provided)
    name :: Maybe Text,
    -- ^ Display name (if provided)
    permissions :: Array Text,
    -- ^ Scopes/permissions/roles
    tenantId :: Maybe Text,
    -- ^ For multi-tenant apps
    rawClaims :: Map Text Json.Value
    -- ^ All claims for custom access
  }
  deriving (Generic, Show, Eq)


instance Json.FromJSON UserClaims


instance Json.ToJSON UserClaims
