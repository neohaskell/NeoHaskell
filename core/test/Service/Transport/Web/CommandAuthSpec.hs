-- | Web transport command authorization tests.
--
-- The Web transport itself is a thin shell over two load-bearing parts:
--
--   * The dispatcher (@Service.CommandExecutor.Core.execute@) which calls
--     'canAccessImpl' before 'decideImpl'. Every transport routes through
--     this single entry point, so parity is by construction.
--   * The HTTP rendering of 'CommandAuthError' done by Web.hs via
--     'unauthorizedResponse' and 'unauthorizedResponseBody'.
--
-- This spec therefore exercises three layers:
--
--   * /Section A/ — typeclass dispatch resolution for the three command
--     policies ('authenticatedAccess' default, 'publicAccess' override,
--     'requirePermission' override).
--   * /Section B/ — the pure HTTP-rendering helpers exported from Web.hs
--     (status code, message, JSON envelope).
--   * /Section C/ — shared-dispatcher parity, via direct
--     'CommandExecutor.execute' calls. Because every transport routes
--     through this single function, exercising it once exercises the gate
--     for every transport.
module Service.Transport.Web.CommandAuthSpec where

import Array (Array)
import Array qualified
import Basics
import Map qualified
import Maybe (Maybe (..))
import Network.HTTP.Types.Status qualified as HTTP
import Service.Auth qualified as Auth
import Service.Command.Auth (QueryAuthError (..))
import Service.Command.Core (Command (..), UserClaims (..))
import Service.CommandExecutor qualified as CommandExecutor
import Service.CommandExecutor.Core (ExecutionResult (..))
import Service.EntityFetcher.Core (EntityFetcher)
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Entity.Core (Entity (..))
import Service.Event qualified as Event
import Service.Event.EntityName (EntityName (..))
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Service.Transport.Web (unauthorizedResponse, unauthorizedResponseBody)
import Task (Task)
import Task qualified
import Test
import Test.Service.Command.Core (AddItemToCart (..), AuthenticatedAddItem (..), CartEntity)
import Test.Service.EventStore.Core (CartEvent)
import Text (Text)
import Text qualified
import ToText (toText)
import Uuid qualified

-- Fixtures for typeclass-dispatch tests
import Service.Command.CanAccess.NoCanAccessFixture (CommandWithoutCanAccess (..))
import Service.Command.CanAccess.PermissionFixture (CommandWithAdminDelete (..))
import Service.Command.CanAccess.PublicAccessFixture (CommandWithPublicAccess (..))


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


cartEntityName :: Event.EntityName
cartEntityName = EntityName "CartEntity"


-- | In-memory CartEvent store + fetcher for the dispatcher parity tests.
-- Mirrors the helper at Service.MultiTenantSpec; duplicated locally to
-- keep the testlib changes scoped to fixtures only.
newCartStoreAndFetcher :: Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent)
newCartStoreAndFetcher = do
  store <-
    InMemory.new
      |> Task.map (EventStore.castEventStore @CartEvent)
      |> Task.mapError toText
  fetcher <-
    EntityFetcher.new
      store
      (initialStateImpl @CartEntity)
      (updateImpl @CartEntity)
      |> Task.mapError toText
  Task.yield (store, fetcher)


-- ============================================================================
-- Spec
-- ============================================================================


spec :: Spec Unit
spec = do
  describe "Service.Transport.Web command authorization" do
    typeclassDispatchSpecs
    httpRenderingSpecs
    dispatcherParitySpecs


-- ----------------------------------------------------------------------------
-- Section A — typeclass dispatch resolution
-- ----------------------------------------------------------------------------


typeclassDispatchSpecs :: Spec Unit
typeclassDispatchSpecs = do
  describe "canAccessImpl resolves correctly per command policy (TH wiring)" do
    describe "default command (canAccess omitted → authenticatedAccess)" do
      it "[non-happy] no claims → Just Unauthenticated" \_ -> do
        let result = canAccessImpl @CommandWithoutCanAccess Nothing
        result |> shouldBe (Just Unauthenticated)

      it "[happy] claims present → Nothing" \_ -> do
        let claims = mkClaims Array.empty
        let result = canAccessImpl @CommandWithoutCanAccess (Just claims)
        result |> shouldBe Nothing

    describe "public command (canAccess = publicAccess)" do
      it "[happy] no claims → Nothing" \_ -> do
        let result = canAccessImpl @CommandWithPublicAccess Nothing
        result |> shouldBe Nothing

      it "[happy] claims present → Nothing" \_ -> do
        let claims = mkClaims Array.empty
        let result = canAccessImpl @CommandWithPublicAccess (Just claims)
        result |> shouldBe Nothing

    describe "permission-gated command (canAccess = requirePermission \"admin:delete\")" do
      it "[non-happy] no claims → Just Unauthenticated" \_ -> do
        let result = canAccessImpl @CommandWithAdminDelete Nothing
        result |> shouldBe (Just Unauthenticated)

      it "[happy] permission present → Nothing" \_ -> do
        let claims = mkClaims ["admin:delete"]
        let result = canAccessImpl @CommandWithAdminDelete (Just claims)
        result |> shouldBe Nothing

      it "[non-happy] permission absent → Just InsufficientPermissions" \_ -> do
        let claims = mkClaims ["user:read"]
        let result = canAccessImpl @CommandWithAdminDelete (Just claims)
        result |> shouldBe (Just (InsufficientPermissions ["admin:delete"]))


-- ----------------------------------------------------------------------------
-- Section B — Web HTTP rendering helpers
-- ----------------------------------------------------------------------------


httpRenderingSpecs :: Spec Unit
httpRenderingSpecs = do
  describe "Web transport renders CommandAuthError to HTTP via pure helpers" do
    it "Unauthenticated → 401 with 'Authentication required'" \_ -> do
      case unauthorizedResponse Unauthenticated of
        (status, msg) -> do
          HTTP.statusCode status |> shouldBe 401
          msg |> shouldBe "Authentication required"

    it "Forbidden → 403 with 'Access denied'" \_ -> do
      case unauthorizedResponse Forbidden of
        (status, msg) -> do
          HTTP.statusCode status |> shouldBe 403
          msg |> shouldBe "Access denied"

    it "InsufficientPermissions → 403 with 'Insufficient permissions'" \_ -> do
      case unauthorizedResponse (InsufficientPermissions ["admin:delete"]) of
        (status, msg) -> do
          HTTP.statusCode status |> shouldBe 403
          msg |> shouldBe "Insufficient permissions"

    it "InsufficientPermissions HTTP body never contains the required permission name" \_ -> do
      -- The audit log carries the permission name; the HTTP body never does.
      -- This is a load-bearing privacy guarantee: an external caller cannot
      -- enumerate which permission they would need by reading the 403 body.
      case unauthorizedResponse (InsufficientPermissions ["admin:delete", "secret:scope"]) of
        (_, msg) -> do
          Text.contains "admin:delete" msg |> shouldBe False
          Text.contains "secret:scope" msg |> shouldBe False

    it "unauthorizedResponseBody wraps message in {\"error\": ...} JSON envelope" \_ -> do
      unauthorizedResponseBody "Authentication required"
        |> shouldBe "{\"error\":\"Authentication required\"}"

    it "unauthorizedResponseBody escapes message contents to keep JSON well-formed" \_ -> do
      -- Defensive: even if a future message contains characters that need
      -- JSON escaping, the envelope must remain parseable.
      let body = unauthorizedResponseBody "msg \"with\" quotes"
      Text.startsWith "{\"error\":" body |> shouldBe True
      Text.endsWith "}" body |> shouldBe True
      -- No raw, unescaped inner quote breaks the JSON.
      Text.contains "\"with\"" body |> shouldBe False


-- ----------------------------------------------------------------------------
-- Section C — shared-dispatcher parity (proves the gate fires once for all transports)
-- ----------------------------------------------------------------------------


dispatcherParitySpecs :: Spec Unit
dispatcherParitySpecs = do
  describe "shared dispatcher: canAccessImpl fires before decideImpl" do
    -- The dispatcher is the single entry point all transports route through.
    -- Exercising it once proves parity by construction: Web, Cli, Mcp, and
    -- Internal each delegate to 'CommandExecutor.execute', so any transport
    -- that bypasses the gate would have to bypass this function — an
    -- architecturally visible change.

    it "anonymous + default-auth command → dispatcher rejects with CommandUnauthorized Unauthenticated" \_ -> do
      -- AuthenticatedAddItem inherits 'canAccessImpl = authenticatedAccess'
      -- (no override in testlib), so anonymous context must reject at the
      -- dispatcher BEFORE decideImpl runs. We verify this by passing an
      -- emptyContext and asserting the dispatcher emits CommandUnauthorized,
      -- not a CommandRejected from the decider's own check.
      (store, fetcher) <- newCartStoreAndFetcher
      cartId <- Uuid.generate
      let cmd = AuthenticatedAddItem {cartId = cartId, itemId = Uuid.nil, amount = 1}
      result <- CommandExecutor.execute store fetcher cartEntityName Auth.emptyContext cmd
      case result of
        CommandUnauthorized {authError} -> authError |> shouldBe Unauthenticated
        other -> fail [fmt|Expected CommandUnauthorized Unauthenticated from dispatcher gate, got #{toText other}|]

    it "anonymous + publicAccess command → dispatcher passes through (gate does NOT short-circuit)" \_ -> do
      -- AddItemToCart has 'canAccessImpl = publicAccess' in testlib. The
      -- dispatcher must pass through to decideImpl; the resulting reject
      -- comes from the business rule (Cart does not exist), proving the
      -- gate is per-command and not a blanket reject.
      (store, fetcher) <- newCartStoreAndFetcher
      cartId <- Uuid.generate
      let cmd = AddItemToCart {cartId = cartId, itemId = Uuid.nil, amount = 1}
      result <- CommandExecutor.execute store fetcher cartEntityName Auth.emptyContext cmd
      case result of
        CommandUnauthorized {} ->
          fail "Expected dispatcher to pass publicAccess command through to decideImpl, but it short-circuited with CommandUnauthorized"
        _ ->
          -- Any other outcome (Accepted or Rejected from decideImpl) proves
          -- the gate opened. The decider rejects with 'Cart does not exist'
          -- because we did not seed the entity, which is the load-bearing
          -- signal here.
          pass

    it "authenticated user + default-auth command → dispatcher passes through" \_ -> do
      -- AuthenticatedAddItem with a valid user must pass the
      -- 'authenticatedAccess' default and reach decideImpl. Again the
      -- business outcome ('Cart does not exist') is what we expect — what
      -- matters is that we did NOT short-circuit at the dispatcher.
      (store, fetcher) <- newCartStoreAndFetcher
      cartId <- Uuid.generate
      ctx <- Auth.authenticatedContext (mkClaims Array.empty)
      let cmd = AuthenticatedAddItem {cartId = cartId, itemId = Uuid.nil, amount = 1}
      result <- CommandExecutor.execute store fetcher cartEntityName ctx cmd
      case result of
        CommandUnauthorized {} ->
          fail "Expected dispatcher to pass authenticated user through to decideImpl, but it short-circuited with CommandUnauthorized"
        _ ->
          pass
