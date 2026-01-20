-- | Query authorization types and helper combinators.
--
-- This module provides the building blocks for declarative query authorization.
-- Queries define two functions:
--
-- * 'canAccess': Checked before fetching data. "Can this user access this query type at all?"
-- * 'canView': Checked after fetching data. "Can this user view this specific instance?"
--
-- Example usage:
--
-- @
-- -- Public query (e.g., product catalog)
-- canAccess = publicAccess
-- canView = publicView
--
-- -- Owner-only query (e.g., user's cart)
-- canAccess = authenticatedAccess
-- canView = ownerOnly (.ownerId)
--
-- -- Admin-only query
-- canAccess = requirePermission "admin:read"
-- canView = publicView
-- @
module Service.Query.Auth (
  -- * Re-exports
  UserClaims (..),

  -- * Error Types
  QueryAuthError (..),
  QueryEndpointError (..),

  -- * Pre-fetch Helpers (canAccess)
  publicAccess,
  authenticatedAccess,
  requirePermission,
  requireAnyPermission,
  requireAllPermissions,

  -- * Post-fetch Helpers (canView)
  publicView,
  ownerOnly,
) where

import Array (Array)
import Array qualified
import Auth.Claims (UserClaims (..))
import Basics
import Maybe (Maybe (..))
import Text (Text)


-- | Errors that can occur during query authorization.
data QueryAuthError
  = -- | No user identity provided (not authenticated)
    Unauthenticated
  | -- | User lacks permission to view this specific instance
    Forbidden
  | -- | User lacks required permission/scope
    InsufficientPermissions (Array Text)
  deriving (Generic, Eq, Show)


-- | Errors that can occur during query endpoint execution.
-- This typed error enables proper HTTP status mapping without string matching.
data QueryEndpointError
  = -- | Authorization failed (maps to 401 or 403)
    AuthorizationError QueryAuthError
  | -- | Storage/serialization failed (maps to 500)
    StorageError Text
  deriving (Generic, Eq, Show)


-- ============================================================================
-- Pre-fetch Helpers (canAccess)
-- ============================================================================

-- | Allow any user (authenticated or not) to access this query type.
--
-- Use for public data like product catalogs.
--
-- @
-- canAccess = publicAccess
-- @
publicAccess :: Maybe UserClaims -> Maybe QueryAuthError
publicAccess _ = Nothing


-- | Require authentication to access this query type.
--
-- Use when only logged-in users should see the data.
--
-- @
-- canAccess = authenticatedAccess
-- @
authenticatedAccess :: Maybe UserClaims -> Maybe QueryAuthError
authenticatedAccess user = case user of
  Nothing -> Just Unauthenticated
  Just _ -> Nothing


-- | Require a specific permission to access this query type.
--
-- @
-- canAccess = requirePermission "inventory:read"
-- @
requirePermission :: Text -> Maybe UserClaims -> Maybe QueryAuthError
requirePermission permission user = case user of
  Nothing -> Just Unauthenticated
  Just claims ->
    case claims.permissions |> Array.contains permission of
      True -> Nothing
      False -> Just (InsufficientPermissions [permission])


-- | Require ANY of the specified permissions to access this query type.
--
-- @
-- canAccess = requireAnyPermission ["admin:read", "manager:read"]
-- @
requireAnyPermission :: Array Text -> Maybe UserClaims -> Maybe QueryAuthError
requireAnyPermission requiredPermissions user = case user of
  Nothing -> Just Unauthenticated
  Just claims -> do
    let hasAny =
          requiredPermissions
            |> Array.any (\p -> claims.permissions |> Array.contains p)
    case hasAny of
      True -> Nothing
      False -> Just (InsufficientPermissions requiredPermissions)


-- | Require ALL of the specified permissions to access this query type.
--
-- @
-- canAccess = requireAllPermissions ["inventory:read", "warehouse:access"]
-- @
requireAllPermissions :: Array Text -> Maybe UserClaims -> Maybe QueryAuthError
requireAllPermissions requiredPermissions user = case user of
  Nothing -> Just Unauthenticated
  Just claims -> do
    let missingPermissions =
          requiredPermissions
            |> Array.takeIf (\p -> not (claims.permissions |> Array.contains p))
    case Array.isEmpty missingPermissions of
      True -> Nothing
      False -> Just (InsufficientPermissions missingPermissions)


-- ============================================================================
-- Post-fetch Helpers (canView)
-- ============================================================================

-- | Allow any user to view any instance of this query.
--
-- Use for public data where instance-level access control is not needed.
--
-- @
-- canView = publicView
-- @
publicView :: forall query. Maybe UserClaims -> query -> Maybe QueryAuthError
publicView _ _ = Nothing


-- | Restrict viewing to the owner of the resource.
--
-- Takes a function that extracts the owner ID from the query instance
-- and compares it to the authenticated user's subject (sub) claim.
--
-- @
-- canView = ownerOnly (.ownerId)
-- @
ownerOnly ::
  forall query.
  (query -> Text) ->
  Maybe UserClaims ->
  query ->
  Maybe QueryAuthError
ownerOnly getOwnerId user query = case user of
  Nothing -> Just Unauthenticated
  Just claims ->
    case claims.sub == getOwnerId query of
      True -> Nothing
      False -> Just Forbidden
