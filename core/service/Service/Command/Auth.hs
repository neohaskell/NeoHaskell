-- | Command authorization types and helper combinators.
--
-- Re-exports 'QueryAuthError' as 'CommandAuthError' to unify error types
-- across the Command and Query paths.
--
-- == Usage
--
-- @
-- -- Public command (no token required)
-- canAccess = publicAccess
--
-- -- Authenticated command (default)
-- canAccess = authenticatedAccess
--
-- -- Permission-gated command
-- canAccess = requirePermission "admin:delete"
-- @
module Service.Command.Auth (
  -- * Error type (alias of Service.Query.Auth.QueryAuthError)
  CommandAuthError,

  -- * Re-exported type with constructors
  QueryAuthError (Unauthenticated, Forbidden, InsufficientPermissions),

  -- * Pre-dispatch helpers (canAccess)
  publicAccess,
  authenticatedAccess,
  requirePermission,
  requireAnyPermission,
  requireAllPermissions,
) where

import Service.Query.Auth (
  QueryAuthError (..),
  authenticatedAccess,
  publicAccess,
  requireAllPermissions,
  requireAnyPermission,
  requirePermission,
 )


-- | Alias for 'QueryAuthError' on the command path.
-- Using a single error type across both query and command authorization
-- avoids duplication and ensures the same constructors are available everywhere.
type CommandAuthError = QueryAuthError
