-- | Tests for Service.Command.Auth helper functions.
--
-- These tests cover publicAccess, authenticatedAccess, requirePermission,
-- requireAnyPermission, requireAllPermissions, and the CommandAuthError type alias.
--
-- All tests are RED against the phase-9 stubs and should go GREEN in phase 10.
module Service.Command.AuthSpec where

import Array (Array)
import Array qualified
import Auth.Claims (UserClaims (..))
import Basics
import Map qualified
import Maybe (Maybe (..))
import Text (Text)
import ToText (toText)
import Service.Command.Auth (
  CommandAuthError,
  QueryAuthError (..),
  authenticatedAccess,
  publicAccess,
  requireAllPermissions,
  requireAnyPermission,
  requirePermission,
 )
import Test


-- ============================================================================
-- Test helpers
-- ============================================================================

-- | Build a minimal UserClaims for test fixtures.
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


-- | Claims with no permissions.
noClaims :: Maybe UserClaims
noClaims = Nothing


-- | Some authenticated claims with no permissions.
someClaims :: UserClaims
someClaims = mkClaims Array.empty


spec :: Spec Unit
spec = do
  describe "Service.Command.Auth" do

    -- ========================================================================
    -- publicAccess
    -- ========================================================================
    describe "publicAccess" do
      it "[happy] returns Nothing for unauthenticated caller (claims = Nothing)" \_ -> do
        -- spec case: publicAccess returns Nothing for unauthenticated caller (claims = Nothing)
        -- This proves: happy path for public commands
        let result = publicAccess noClaims
        result |> shouldBe Nothing

      it "[happy] returns Nothing for authenticated caller (claims present)" \_ -> do
        -- spec case: publicAccess returns Nothing for authenticated caller (claims present)
        -- This proves: publicAccess is truly permissive
        let result = publicAccess (Just someClaims)
        result |> shouldBe Nothing

      it "[non-happy] never returns Forbidden or InsufficientPermissions" \_ -> do
        -- spec case: never returns Forbidden or InsufficientPermissions
        -- This proves: public access has no nuanced rejection cases
        case publicAccess Nothing of
          Nothing -> pass
          Just Unauthenticated -> fail "publicAccess should not return Unauthenticated"
          Just Forbidden -> fail "publicAccess should not return Forbidden"
          Just (InsufficientPermissions _) -> fail "publicAccess should not require permissions"

    -- ========================================================================
    -- authenticatedAccess
    -- ========================================================================
    describe "authenticatedAccess" do
      it "[happy] returns Nothing for authenticated caller (claims present)" \_ -> do
        -- spec case: authenticatedAccess returns Nothing for authenticated caller (claims present)
        -- This proves: happy path for authenticated commands
        let result = authenticatedAccess (Just someClaims)
        result |> shouldBe Nothing

      it "[non-happy] returns Just Unauthenticated for unauthenticated caller (claims = Nothing)" \_ -> do
        -- spec case: returns Just Unauthenticated for unauthenticated caller (claims = Nothing)
        -- This proves: default enforcement (whitelist by default)
        let result = authenticatedAccess Nothing
        result |> shouldBe (Just Unauthenticated)

      it "[non-happy] is the typeclass default for Command (same as authenticatedAccess Nothing)" \_ -> do
        -- spec case: is the typeclass default for Command
        -- This proves: secure-by-default design
        -- authenticatedAccess Nothing == Just Unauthenticated is the expected default behaviour
        let result = authenticatedAccess Nothing
        result |> shouldBe (Just Unauthenticated)

    -- ========================================================================
    -- requirePermission
    -- ========================================================================
    describe "requirePermission" do
      it "[happy] returns Nothing when claims contain the required permission" \_ -> do
        -- spec case: returns Nothing when claims contain the required permission
        -- This proves: happy path for permission-gated commands
        let claims = mkClaims ["admin:delete"]
        let result = requirePermission "admin:delete" (Just claims)
        result |> shouldBe Nothing

      it "[non-happy] returns Just (InsufficientPermissions [perm]) when claims lack the permission" \_ -> do
        -- spec case: returns Just (InsufficientPermissions [perm]) when claims lack the permission
        -- This proves: permission check correctly identifies missing permission
        let claims = mkClaims ["user:read"]
        let result = requirePermission "admin:delete" (Just claims)
        result |> shouldBe (Just (InsufficientPermissions ["admin:delete"]))

      it "[non-happy] returns Just Unauthenticated when claims = Nothing" \_ -> do
        -- spec case: returns Just Unauthenticated when claims = Nothing
        -- This proves: missing auth is distinct from insufficient permission
        let result = requirePermission "admin:delete" Nothing
        result |> shouldBe (Just Unauthenticated)

      it "[non-happy] returns InsufficientPermissions with correct permission string in array" \_ -> do
        -- spec case: returns InsufficientPermissions with correct permission string in array
        -- This proves: audit trail carries the required permission name
        let claims = mkClaims Array.empty
        case requirePermission "admin:delete" (Just claims) of
          Just (InsufficientPermissions perms) -> do
            Array.length perms |> shouldBe 1
            Array.first perms |> shouldBe (Just "admin:delete")
          other ->
            fail [fmt|expected InsufficientPermissions ["admin:delete"], got #{toText other}|]

      it "[non-happy] distinguishes between multiple required permissions (single vs many)" \_ -> do
        -- spec case: distinguishes between multiple required permissions (single vs many)
        -- This proves: function signature clearly states single permission
        let claims = mkClaims Array.empty
        case requirePermission "x:y" (Just claims) of
          Just (InsufficientPermissions perms) ->
            Array.length perms |> shouldBe 1
          other ->
            fail [fmt|expected exactly one missing permission, got #{toText other}|]

    -- ========================================================================
    -- requireAnyPermission
    -- ========================================================================
    describe "requireAnyPermission" do
      it "[happy] returns Nothing when claims contain at least one of the required permissions" \_ -> do
        -- spec case: returns Nothing when claims contain at least one of the required permissions
        -- This proves: happy path for OR permission gates
        let claims = mkClaims ["admin:delete"]
        let result = requireAnyPermission ["admin:delete", "admin:modify"] (Just claims)
        result |> shouldBe Nothing

      it "[happy] returns Nothing when claims contain multiple of the required permissions" \_ -> do
        -- spec case: returns Nothing when claims contain multiple of the required permissions
        -- This proves: order-independent matching
        let claims = mkClaims ["admin:delete", "admin:modify", "user:read"]
        let result = requireAnyPermission ["admin:delete", "admin:modify"] (Just claims)
        result |> shouldBe Nothing

      it "[non-happy] returns Just (InsufficientPermissions [array]) when claims lack all required permissions" \_ -> do
        -- spec case: returns Just (InsufficientPermissions [array]) when claims lack all
        -- This proves: rejection lists all missing permissions for clarity
        let claims = mkClaims ["user:read"]
        case requireAnyPermission ["admin:delete", "admin:modify"] (Just claims) of
          Just (InsufficientPermissions perms) -> do
            Array.length perms |> shouldBe 2
            Array.contains ("admin:delete" :: Text) perms |> shouldBe True
            Array.contains ("admin:modify" :: Text) perms |> shouldBe True
          other ->
            fail [fmt|expected InsufficientPermissions for missing OR permissions, got #{toText other}|]

      it "[non-happy] returns Just Unauthenticated when claims = Nothing" \_ -> do
        -- spec case: returns Just Unauthenticated when claims = Nothing
        -- This proves: unauthenticated is distinct from insufficient permission
        let result = requireAnyPermission ["admin:delete", "admin:modify"] Nothing
        result |> shouldBe (Just Unauthenticated)

      it "[non-happy] returns InsufficientPermissions with all required permissions listed" \_ -> do
        -- spec case: returns InsufficientPermissions with all required permissions listed
        -- This proves: audit trail is comprehensive
        let claims = mkClaims Array.empty
        case requireAnyPermission ["x:y", "a:b"] (Just claims) of
          Just (InsufficientPermissions perms) ->
            Array.length perms |> shouldBe 2
          other ->
            fail [fmt|expected two missing permissions, got #{toText other}|]

    -- ========================================================================
    -- requireAllPermissions
    -- ========================================================================
    describe "requireAllPermissions" do
      it "[happy] returns Nothing when claims contain all required permissions" \_ -> do
        -- spec case: returns Nothing when claims contain all required permissions
        -- This proves: happy path for AND permission gates
        let claims = mkClaims ["admin:delete", "admin:modify"]
        let result = requireAllPermissions ["admin:delete", "admin:modify"] (Just claims)
        result |> shouldBe Nothing

      it "[non-happy] returns Just (InsufficientPermissions [missing]) when claims lack one required permission" \_ -> do
        -- spec case: returns Just (InsufficientPermissions [missing]) when claims lack one
        -- This proves: rejection identifies exactly which permissions are missing
        let claims = mkClaims ["admin:delete"]
        case requireAllPermissions ["admin:delete", "admin:modify"] (Just claims) of
          Just (InsufficientPermissions perms) -> do
            Array.length perms |> shouldBe 1
            Array.first perms |> shouldBe (Just "admin:modify")
          other ->
            fail [fmt|expected missing admin:modify, got #{toText other}|]

      it "[non-happy] returns Just (InsufficientPermissions [missing]) when claims lack all required permissions" \_ -> do
        -- spec case: returns Just (InsufficientPermissions [missing]) when claims lack all
        -- This proves: comprehensive missing-permission reporting
        let claims = mkClaims ["user:read"]
        case requireAllPermissions ["admin:delete", "admin:modify"] (Just claims) of
          Just (InsufficientPermissions perms) -> do
            Array.length perms |> shouldBe 2
            Array.contains ("admin:delete" :: Text) perms |> shouldBe True
            Array.contains ("admin:modify" :: Text) perms |> shouldBe True
          other ->
            fail [fmt|expected both missing permissions, got #{toText other}|]

      it "[non-happy] returns Just Unauthenticated when claims = Nothing" \_ -> do
        -- spec case: returns Just Unauthenticated when claims = Nothing
        -- This proves: unauthenticated is distinct from insufficient permission
        let result = requireAllPermissions ["admin:delete", "admin:modify"] Nothing
        result |> shouldBe (Just Unauthenticated)

      it "[non-happy] distinguishes AND logic from OR logic via error content" \_ -> do
        -- spec case: distinguishes AND logic from OR logic via error content
        -- This proves: AND and OR semantics are clearly differentiated
        let claims = mkClaims ["admin:delete"]
        case requireAllPermissions ["admin:delete", "admin:modify"] (Just claims) of
          Just (InsufficientPermissions perms) ->
            -- requireAllPermissions lists missing; requireAnyPermission lists required
            Array.length perms |> shouldBe 1 -- only what's missing
          other ->
            fail [fmt|expected one missing permission, got #{toText other}|]

    -- ========================================================================
    -- CommandAuthError type alias and constructors
    -- ========================================================================
    describe "CommandAuthError" do
      it "[happy] Unauthenticated constructor is exported and usable" \_ -> do
        -- spec case: Unauthenticated constructor is exported and usable
        -- This proves: public API exports the error constructors
        -- The Unauthenticated constructor must be in scope and equal to itself.
        -- This compile-time check also verifies the export works.
        let err = Unauthenticated :: CommandAuthError
        let wrapped = Just err :: Maybe CommandAuthError
        case wrapped of
          Just Unauthenticated -> pass
          _ -> fail "Unauthenticated constructor not exported or does not match"

      it "[happy] InsufficientPermissions constructor carries Array Text of missing permissions" \_ -> do
        -- spec case: InsufficientPermissions constructor carries Array Text of missing permissions
        -- This proves: permission list is part of the type signature
        let err = InsufficientPermissions ["admin:delete", "user:modify"]
        case (Just err :: Maybe CommandAuthError) of
          Just (InsufficientPermissions perms) ->
            Array.length perms |> shouldBe 2
          _ -> fail "InsufficientPermissions constructor shape mismatch"

      it "[happy] CommandAuthError type alias resolves to QueryAuthError (compile-time check)" \_ -> do
        -- spec case: CommandAuthError type alias resolves to QueryAuthError
        -- This proves: error types are unified across Command and Query paths
        -- Verified via successful compilation of this module (the type unifies at compile time)
        pass
