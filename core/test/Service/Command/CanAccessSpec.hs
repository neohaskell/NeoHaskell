-- | TH resolution tests for canExecuteImpl.
--
-- Tests verify that:
-- 1. Command with no canAccess definition uses the typeclass default.
-- 2. Command with canAccess = publicAccess gets wired through TH.
-- 3. Command with canAccess = requirePermission "x:y" works correctly.
-- 4. Type errors on wrong-typed canAccess are caught at compile time.
--
-- All tests are RED against the phase-9 stubs and go GREEN in phase 10.
module Service.Command.CanAccessSpec where

import Array (Array)
import Array qualified
import Basics
import Map qualified
import Maybe (Maybe (..))
import Text (Text)
import Service.Query.Auth (
  AccessError (..),
 )
import Service.Command.CanAccess.NoCanAccessFixture (CommandWithoutCanAccess (..))
import Service.Command.CanAccess.PermissionFixture (CommandWithAdminDelete (..))
import Service.Command.CanAccess.PublicAccessFixture (CommandWithPublicAccess (..))
import Service.Command.Core (Command (..), UserClaims (..))
import Test


-- ============================================================================
-- Test helpers
-- ============================================================================

mkClaims :: Array Text -> UserClaims
mkClaims perms =
  UserClaims
    { sub = "test-user"
    , email = Nothing
    , name = Nothing
    , permissions = perms
    , tenantId = Nothing
    , rawClaims = Map.empty
    }


spec :: Spec Unit
spec = do
  describe "Service.CommandExecutor.TH canAccess binding" do

    it "[non-happy] default canExecuteImpl resolves to authenticatedAccess when no user-defined canAccess exists" \_ -> do
      -- spec case: default canExecuteImpl resolves to authenticatedAccess when no canAccess defined
      -- This proves: typeclass default locks the command by default
      -- RED: canExecuteImpl stub throws error "canExecuteImpl stub not implemented"
      let result = canExecuteImpl @CommandWithoutCanAccess Nothing
      result |> shouldBe (Just Unauthenticated)

    it "[non-happy] canExecuteImpl = publicAccess resolves from user-defined canAccess" \_ -> do
      -- spec case: canExecuteImpl = publicAccess resolves from user-defined canAccess
      -- This proves: TH macro correctly wires user-defined canAccess into canExecuteImpl
      -- RED: TH stub generates error "TH stub for canAccess not implemented"
      let resultNoClaims = canExecuteImpl @CommandWithPublicAccess Nothing
      resultNoClaims |> shouldBe Nothing
      let resultWithClaims = canExecuteImpl @CommandWithPublicAccess (Just (mkClaims Array.empty))
      resultWithClaims |> shouldBe Nothing

    it "[happy] canExecuteImpl = requirePermission resolves correctly when permission present" \_ -> do
      -- spec case: canExecuteImpl = requirePermission "x:y" resolves correctly when permission present
      -- This proves: permission helpers are correctly wired and resolve permission sets
      -- RED: TH stub generates error "TH stub for canAccess not implemented"
      let claimsWithPerm = mkClaims ["admin:delete"]
      let result = canExecuteImpl @CommandWithAdminDelete (Just claimsWithPerm)
      result |> shouldBe Nothing

    it "[non-happy] canExecuteImpl = requirePermission rejects when permission absent" \_ -> do
      -- spec case: canExecuteImpl = requirePermission "x:y" rejects when permission absent
      -- This proves: permission check correctly enforces required scope
      -- RED: TH stub generates error "TH stub for canAccess not implemented"
      let claimsWithoutPerm = mkClaims ["user:read"]
      let result = canExecuteImpl @CommandWithAdminDelete (Just claimsWithoutPerm)
      result |> shouldBe (Just (InsufficientPermissions ["admin:delete"]))
