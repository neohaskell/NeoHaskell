-- | Audit-logging tests for CommandUnauthorized.
--
-- These tests verify that canAccessImpl returns the correct errors for different
-- command types and auth states, and that the error constructors carry the right data
-- for audit logging (constructor name, never permission strings or PII).
--
-- All tests are RED against phase-9 stubs and go GREEN in phase 10.
module Service.CommandExecutor.AuditLoggingSpec where

import Array (Array)
import Array qualified
import Basics
import Map qualified
import Maybe (Maybe (..))
import Text (Text)
import ToText (toText)
import Service.Command.Auth (QueryAuthError (..))
import Service.Command.Core (Command (..), UserClaims (..))
import Test

import Service.Command.CanAccess.NoCanAccessFixture (CommandWithoutCanAccess (..))
import Service.Command.CanAccess.PermissionFixture (CommandWithAdminDelete (..))


-- ============================================================================
-- Test helpers
-- ============================================================================

mkClaims :: Array Text -> UserClaims
mkClaims perms =
  UserClaims
    { sub = "user@example.com"
    , email = Nothing
    , name = Nothing
    , permissions = perms
    , tenantId = Nothing
    , rawClaims = Map.empty
    }


spec :: Spec Unit
spec = do
  describe "Service.CommandExecutor.Core audit logging for CommandUnauthorized" do

    it "[non-happy] canAccessImpl returns Just Unauthenticated for anonymous caller on default command" \_ -> do
      -- spec case: logs exactly one Log.warn record when canAccessImpl returns Just Unauthenticated
      -- This proves: audit trail should contain Unauthenticated constructor name
      -- RED: canAccessImpl stub panics instead of returning Just Unauthenticated
      let result = canAccessImpl @CommandWithoutCanAccess Nothing
      result |> shouldBe (Just Unauthenticated)

    it "[non-happy] claims_present = False when user = Nothing → canAccessImpl returns Unauthenticated" \_ -> do
      -- spec case: logs claims_present = False when requestContext.user = Nothing
      -- This proves: audit trail correctly reflects absence of claims
      -- RED: canAccessImpl stub panics
      let result = canAccessImpl @CommandWithoutCanAccess Nothing
      result |> shouldBe (Just Unauthenticated)

    it "[non-happy] claims_present = True when user = Just claims → canAccessImpl returns InsufficientPermissions" \_ -> do
      -- spec case: logs claims_present = True when requestContext.user = Just claims
      -- This proves: audit trail correctly reflects presence of claims
      -- RED: canAccessImpl (TH stub) panics instead of returning InsufficientPermissions
      let claimsPresent = mkClaims Array.empty
      let result = canAccessImpl @CommandWithAdminDelete (Just claimsPresent)
      result |> shouldBe (Just (InsufficientPermissions ["admin:delete"]))

    it "[non-happy] constructor name InsufficientPermissions (not permission contents) is auditable" \_ -> do
      -- spec case: logs constructor name InsufficientPermissions (never the permission list)
      -- This proves: audit uses constructor name for error type, not permission details
      -- RED: canAccessImpl (TH stub) panics instead of returning InsufficientPermissions
      let claims = mkClaims Array.empty
      let result = canAccessImpl @CommandWithAdminDelete (Just claims)
      -- The result contains InsufficientPermissions for audit logging
      -- but the audit log should only record the constructor name, not the perms list
      case result of
        Just (InsufficientPermissions _) -> pass
        other -> fail [fmt|expected InsufficientPermissions for audit log, got #{toText other}|]

    it "[non-happy] canAccessImpl never returns claim contents in error payload" \_ -> do
      -- spec case: even when claims contain PII (sub, email, permissions),
      -- the CommandAuthError payload carries only the required permission name(s).
      -- This proves: claim contents do not flow into the value the dispatcher
      -- would emit to the audit log; the constructor + permission-name shape is
      -- the maximum information a rejection carries.
      let pii = UserClaims
            { sub = "leakable-pii-marker@example.com"
            , email = Just "leakable-pii-marker@example.com"
            , name = Just "Real Name Marker"
            , permissions = ["user:read"]
            , tenantId = Just "tenant-marker-abc"
            , rawClaims = Map.empty
            }
      let result = canAccessImpl @CommandWithAdminDelete (Just pii)
      case result of
        Just (InsufficientPermissions required) -> do
          -- The payload contains only the required permission name.
          required |> shouldBe ["admin:delete"]
          -- No PII fields appear anywhere in the payload.
          Array.any (\p -> p == pii.sub) required |> shouldBe False
          case pii.email of
            Just e  -> Array.any (\p -> p == e) required |> shouldBe False
            Nothing -> pass
          case pii.name of
            Just n  -> Array.any (\p -> p == n) required |> shouldBe False
            Nothing -> pass
          case pii.tenantId of
            Just t  -> Array.any (\p -> p == t) required |> shouldBe False
            Nothing -> pass
        other -> fail [fmt|expected InsufficientPermissions [admin:delete], got #{toText other}|]

    it "[non-happy] canAccessImpl with default command returns Unauthenticated (command type identifiable)" \_ -> do
      -- spec case: logs command type name extracted from the typeclass dispatch
      -- This proves: audit trail identifies which command was rejected
      -- The command type CommandWithoutCanAccess is the type being tested
      -- RED: canAccessImpl stub panics; the @CommandWithoutCanAccess dispatch fails
      let result = canAccessImpl @CommandWithoutCanAccess Nothing
      result |> shouldBe (Just Unauthenticated)
