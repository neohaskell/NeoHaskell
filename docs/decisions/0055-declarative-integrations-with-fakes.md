# ADR-0055: Declarative Integrations with Real/Fake Parity

## Status

Proposed

## Context

### Current State

NeoHaskell has two ADRs governing outbound integrations:

- [ADR-0008](0008-integration-pattern.md) defined an imperative `ToAction` typeclass: Nick writes `toAction :: config -> Action` where `Action` wraps a `Task IntegrationError (Maybe CommandPayload)`. HTTP calls, retries, and authentication live inside the `ToAction` instance body.
- [ADR-0049](0049-outboundintegration-typeclass-dispatch-generation.md) layered an `OutboundIntegration` typeclass on top for typed event dispatch, but left `ToAction` untouched as the adapter mechanism.

Inbound integrations (webhooks, file watchers, Stripe consumers) are defined in ADR-0008 as `Inbound` workers — Nick writes the worker body imperatively with `Http.serve` and `emit`.

Neither layer says anything about testing. Today a NeoHaskell application that depends on Sendgrid, Stripe, or any other third-party API has three unattractive options for tests:

1. **Run against a real sandbox.** Requires credentials in CI, non-determinism, network flakes, rate limits.
2. **Hand-build a mock per project.** Every application reinvents the same fake. Silent divergence from the real API is never caught — the mock says "yes, sent" long after the real endpoint started returning 422.
3. **Skip integration tests entirely.** The majority path in practice.

There is no framework-level affordance for swapping a fake in at wire-up time, no auto-generated stub from request/response types, and no contract mechanism to detect when a hand-written fake has drifted from the real API.

### Use Cases

- **Jess writes a unit test for a command handler that emits an outbound Sendgrid email.** She does not have Sendgrid credentials, does not want the test to hit the network, and does not want to hand-build a mock.
- **Nick ships `integration-stripe` and wants fixtures for the payment state machine.** Arbitrary responses are not enough — a second `chargeIntent` call after `authorizeIntent` must return `status = "captured"`, not a random status.
- **A project team records real sandbox responses once and commits them as fixtures.** The fake plays back exactly those responses during CI.
- **CI needs to catch drift** when Sendgrid changes its response shape and the hand-written fake still returns the old shape.
- **Property-based tests** over an integration's response space — generate 100 arbitrary requests and assert an invariant over the responses.
- **Inbound symmetry.** A test wants to simulate a Stripe webhook firing with a chosen payload, without running an HTTP server.

### Design Goals

1. **Every integration ships both a real and a fake implementation.** No additional opt-in. Selection is a wiring choice, not a code change.
2. **Zero-ceremony default fakes.** If Nick does nothing special, a fake is auto-derived from the response type via QuickCheck's `Arbitrary`. Stateless cases are free.
3. **Override path for stateful fakes.** When a state machine (payment lifecycle, order lifecycle) needs to be modelled, Nick overrides the default with a hand-written implementation using whatever concurrency primitives he likes.
4. **Fixture fallback.** Before falling back to the generated stub, the fake checks `tests/fixtures/<integration>/<hash>.json` for a recorded response keyed by request hash. A project can commit real recorded traffic.
5. **Drift detection is automatic.** The framework generates a property test per `Integration` instance that fails when the fake's response no longer parses as the declared response type, or the real response no longer parses either.
6. **Declarative, not imperative.** An integration is a request type, a response type, and a `runReal` function. Everything else (fake, schema, contract test) derives from the declaration.
7. **Inbound parity.** Inbound integrations follow the same real/fake structure with a symmetric API.

### GitHub Issue

No tracking issue exists yet.

## Decision

### 1. Integration as a Declarative Schema

An `Integration` is a request type, a response type, and a `runReal` function. `runFake` has a *constrained default* that falls back to `QuickCheck.generate arbitrary @Response` — but `Arbitrary` is **not** a superclass constraint of `Integration`. The default is only required when Nick does not override `runFake`. This keeps QuickCheck out of the production dispatch dictionary when every integration defines its own `runFake` (or in production builds that are linked against only `runReal`-only integrations).

This supersedes the `ToAction` typeclass from [ADR-0008](0008-integration-pattern.md) and replaces the raw `runReal`-equivalent body that today lives inside `OutboundIntegration` handlers from [ADR-0049](0049-outboundintegration-typeclass-dispatch-generation.md).

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Two typeclass instances (`ToAction (Real Cfg)`, `ToAction (Fake Cfg)`) selected by a wrapper | Rejected | Forces Nick to write two instances even in the common zero-state case. Wrapper types pollute every call site. Cannot auto-derive the fake. |
| A single `Adapter req resp` record with `runReal`/`runFake` function fields | Rejected | Records cannot carry a default — Nick must always supply both. No hook for the framework to inject Arbitrary-based fallback. Value-level dispatch prevents per-instance fixture lookup without runtime reflection. |
| Schema-driven runtime generation (`ToSchema` drives `genValueFromSchema :: Schema -> Gen Value`) | Rejected | No runtime value-generator for `Schema` exists today ([Schema.hs:94](../../core/schema/Schema.hs)). Building one is net-new infrastructure; the `Arbitrary` ecosystem already solves this with `Generic` derivation. Revisit if the Schema infrastructure grows a value-generation helper. |
| `Arbitrary` as a **superclass constraint** on `Integration` | Rejected | Every production binary would link `QuickCheck` transitively via the class dictionary. Arbitrary is a test concern; the production dispatch path must not depend on it. |
| A typeclass with `runReal` as a method and `runFake` as a method with a **constrained default** (`default runFake :: (Arbitrary (Response request)) => ...`) | **Chosen** | Zero-ceremony default when Nick wants it (`deriving Arbitrary` on the response type is a Generic one-liner). Override is trivial (define `runFake` on the instance). The constraint lives on the *default method body*, not the class head, so production builds whose integrations all override `runFake` link no `QuickCheck` code. |

```haskell
module Service.Integration.Adapter (Integration (..)) where

import Core
import Task qualified
import Test.QuickCheck qualified as QuickCheck

class Integration request where
  type Response request :: Type

  runReal :: request -> Task IntegrationError (Response request)

  runFake :: request -> Task IntegrationError (Response request)
  default runFake ::
    (QuickCheck.Arbitrary (Response request)) =>
    request ->
    Task IntegrationError (Response request)
  runFake _ = do
    generated <- Task.fromIO (QuickCheck.generate QuickCheck.arbitrary)
    Task.yield generated
```

`Arbitrary` appears only on the constrained default. Nick who overrides `runFake` for a stateful integration pays nothing for QuickCheck; a `--integrations=real` production build of a project whose every integration overrides `runFake` (or whose response types omit `deriving Arbitrary`) links zero QuickCheck symbols into the outbound dispatch path.

**What Nick writes for the zero-state case:**

```haskell
-- Config values that hold secrets use Redacted. The API key is read from the
-- environment at wire-up, wrapped immediately, and never appears in Show/JSON.
data SendEmailConfig = SendEmailConfig
  { apiKey :: Redacted Text
  }
  deriving (Generic)

-- Request / response types derive Arbitrary only when the default runFake is
-- desired. Show is still safe here because neither type holds a secret.
data SendEmail = SendEmail
  { to :: Text
  , templateId :: Text
  , variables :: Array (Text, Text)
  }
  deriving (Generic, Arbitrary, ToJSON, FromJSON, Show)

data SendEmailResponse = SendEmailResponse
  { messageId :: Text
  , status :: Text
  }
  deriving (Generic, Arbitrary, ToJSON, FromJSON, Show)

instance Integration SendEmail where
  type Response SendEmail = SendEmailResponse
  runReal req = do
    rawKey <- Environment.getVariable "SENDGRID_API_KEY"
    let apiKey = Redacted.wrap rawKey
    Http.post "https://api.sendgrid.com/v3/mail/send"
      |> Http.bearer apiKey
      |> Http.jsonBody req
      |> Http.send
      |> Task.mapError toIntegrationError
```

No `runFake` needed — the constrained default kicks in because `SendEmailResponse` derives `Arbitrary`.

**Secret fields**: any field that holds a bearer token, API key, signing secret, short-lived OAuth code, PSP client secret, or similar **must** be typed as `Redacted a` (see [Redacted.hs:46](../../core/core/Redacted.hs)). `Redacted` deliberately omits `ToJSON`, `Eq`, and `Generic` instances, and its `Show` instance prints `<redacted>`. Response types containing `Redacted` fields therefore cannot derive `Generic, Arbitrary, ToJSON, FromJSON, Show` as a single line — the framework's `runFake` default does not apply (no `Arbitrary`), and the author must define `runFake` explicitly, typically returning a scripted response with `Redacted.wrap "<fake-token>"`. This is deliberate: secret-bearing integrations cannot fall through to Arbitrary-generated data and cannot accidentally serialise real secrets through contract-test failure output.

**What Nick writes for a stateful fake:**

```haskell
instance Integration ChargeIntent where
  type Response ChargeIntent = ChargeIntentResponse
  runReal req = Http.post "https://api.stripe.com/v1/payment_intents/..." |> ...

  runFake req = do
    state <- PaymentFakeState.get req.intentId
    case state of
      Authorized -> do
        PaymentFakeState.set req.intentId Captured
        Task.yield (ChargeIntentResponse {status = "captured", ...})
      Captured ->
        Task.throw (PermanentFailure "already captured")
      NotFound ->
        Task.throw (ValidationError "unknown intent")
```

The state store (`PaymentFakeState`) is Nick's concern — whatever `IORef`, `TVar`, or pure-structure approach fits the integration. The framework does not prescribe a state-machine DSL.

### 2. Inbound Symmetry

Inbound integrations mirror the shape. An `InboundIntegration` declares the trigger type (the external event that arrives), `runReal` listens for it, `runFake` is driven by the test harness.

```haskell
class InboundIntegration inbound where
  type Trigger inbound :: Type

  runReal :: (Trigger inbound -> Task IntegrationError ()) -> Task IntegrationError Void

  runFake :: InboundHandle (Trigger inbound)
  default runFake ::
    (QuickCheck.Arbitrary (Trigger inbound)) =>
    InboundHandle (Trigger inbound)
  runFake = Inbound.controllableHandle
```

As with `Integration`, `Arbitrary (Trigger inbound)` is a constrained-default requirement, not a superclass constraint. Production dispatch does not link QuickCheck via this class.

`Inbound.controllableHandle` is backed by a **bounded** channel (`Channel.newBounded` at [Channel.hs:99](../../core/concurrency/Channel.hs:99)) so that fake-mode inbound dispatch exercises the same backpressure semantics as the real implementation. Default capacity is 1024; the framework exposes `Inbound.controllableHandleWith :: Int -> InboundHandle t` for authors whose integration genuinely needs a different bound. `simulate` uses the channel's blocking `write`, matching real-inbound behaviour where a slow consumer applies backpressure to the producer. A fake that silently buffered an unbounded queue would mask exactly the kind of bug integration tests are meant to catch.

`simulate` is **not** re-exported from `Core` or from the top-level `Integration` module. It lives in `Test.Integration` (shipped with nhcore), so a production module that imports only `Core` / `Integration` cannot invoke it. Tests opt in explicitly:

```haskell
import Test.Integration qualified as TestIntegration

test_webhookDispatch = do
  TestIntegration.simulate @StripeWebhook syntheticEvent
  ...
```

Compile-time exclusion from production module paths is a stronger guarantee than a runtime gate. Nick overrides `runFake` only when the inbound needs its own autonomous behaviour in smoke tests (e.g. a fake file watcher that periodically emits synthetic files).

### 3. Selection via `Application.run` — CLI Flag Gated by Environment Variable

Selection happens in the default CLI surface of `Application.run`. No Haskell source change is required to flip from real to fake. The CLI flag alone is insufficient: `--integrations=fake` and `--integrations=hybrid --fake=NAME` are **only honoured** when the environment variable `NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=1` is present in the process environment. Without the env var, `Application.run` rejects the flag at startup with a clear error and exits non-zero.

| Flag | Env-var requirement | Meaning |
|------|---------------------|---------|
| `--integrations=real` | None (default) | All integrations call `runReal`. Default when flag is absent. |
| `--integrations=fake` | `NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=1` required | All integrations call `runFake` (with fixture fallback). Startup aborts if the env var is not set. |
| `--integrations=hybrid --fake=Sendgrid --fake=Stripe` | `NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=1` required | Named integrations use `runFake`, the rest use `runReal`. Useful for staging. Startup aborts if the env var is not set. |

**Dual-gate rationale.** A CLI flag alone can be set by:

- A Kubernetes manifest copied from staging to production
- A systemd unit-file argument left over from a previous rollout
- A rogue `kubectl exec`/`docker run` by an operator

An environment variable is set deliberately in a deployment config (a Helm chart, a Nix module, a `.env` file, a `docker run -e` flag) and is reviewed as part of that config. Requiring *both* means an attacker or a misconfiguration needs to compromise both surfaces simultaneously. The env var is the production-safety opt-in; the CLI flag is the per-invocation toggle. Production deployment manifests simply do not include `NEOHASKELL_ALLOW_FAKE_INTEGRATIONS`, and any leftover `--integrations=fake` in a production binary's args will fail loudly at startup.

Additionally, on startup, if fake mode is active, `Application.run` logs one line at `ERROR` level (visible in all log aggregations without opt-in) naming the integrations currently in fake mode, and the `/health` endpoint exposes `integrations.mode=fake|hybrid|real` and `integrations.fakes=[...]`. This lets a deploy-check assert that no production instance is ever in fake mode.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Plain environment variable alone (`NEOHASKELL_INTEGRATIONS=fake`) | Rejected | Invisible in process listings; easy to leak via `/proc/*/environ` into copied deployment configs. |
| Plain CLI flag alone (no env-var gate) | Rejected | Single-surface misconfiguration can silently disable real integrations in production. Previously considered; reversed after security review. |
| Per-integration wiring in `Application` builder (`Application.withFake @Sendgrid`) | Rejected | Forces test suites and test binaries to diverge from production binaries at compile time. Kills the "same binary, different flag" promise. |
| Separate `Application.test` builder | Rejected | Duplicates `Application` surface area. Every new `with*` helper would need a test-mode twin. |
| CLI flag gated by env-var opt-in | **Chosen** | Same binary, dual-surface gate. Deployment config owns the opt-in (env var); per-test-run decides the mode (flag). Visible in process listings and CI logs; composable with the existing declarative config DSL (ADR-0021). |

**Wire-up-time dispatch, not per-call selection.** Selection is resolved **once, at startup** in `Application.run`, before the command/event loop begins accepting traffic. For each `Integration` instance the framework constructs a *pre-bound closure* — a function of type `request -> Task IntegrationError (Response request)` — that already routes to the chosen implementation:

```haskell
-- Wire-up (runs once, at startup, inside Application.run)
mkDispatcher ::
  forall request.
  (Integration request) =>
  Selection ->
  (request -> Task IntegrationError (Response request))
mkDispatcher selection = case selection of
  Real -> runReal
  Fake -> fakeOrFixture @request
  Hybrid fakes
    | isFake @request fakes -> fakeOrFixture @request
    | otherwise -> runReal
{-# INLINE mkDispatcher #-}
```

At request time, outbound dispatch calls the pre-bound closure directly. There is no per-call `case` over `Selection`, no per-call `Map.lookup`, no per-call env-var read. The dispatcher shim is `{-# INLINE #-}` so the closure call inlines into the call site. This keeps the production (`--integrations=real`) hot path at one direct function call per emit, preserving the 50k req/s budget.

### 4. Fixture Storage and Lookup

Fixtures live under `<projectRoot>/tests/fixtures/<integration-name>/<hash>.json` where:

- `<projectRoot>` is the absolute path resolved from the `Application` config at wire-up time (not `getCurrentDirectory`). The framework refuses to resolve fixture paths against an unset project root — there is no implicit fallback to `cwd`.
- `<integration-name>` is produced by the `NameOf` type family already used by commands and queries (e.g. `SendEmail`). Because `NameOf` is computed at the type level by the framework, it is never user-controlled. The framework additionally validates that the rendered name matches `[A-Za-z0-9_]+`; any other characters cause fixture loading to fail at startup with a clear error (this catches typos in custom `NameOf` instances, not just malicious input).
- `<hash>` is the hex-encoded (full 64 character, lower-case) SHA-256 of the request serialised to canonical JSON (RFC 8785 / JCS — see §12). Hex-only means the hash never contributes path-dangerous characters.

**Path assembly and validation**. The final path is built with `System.FilePath`'s `</>` combinator and then passed through `System.Directory.makeAbsolute`. Before any read or write, the framework asserts that the canonicalised absolute path is a descendant of `<projectRoot>/tests/fixtures/` (prefix check on the resolved absolute path). A mismatch — including attempts like `..`, symlinks pointing outside the root, or Unicode homoglyphs — causes the lookup to be treated as a miss (read path) or the record to fail with an error (write path). This makes fixture-path construction safe even if a future change accidentally plumbs user-controlled text into `<integration-name>` (e.g. via the `--fake=NAME` CLI argument); the validation rejects the value at the boundary rather than trusting callers.

Each file holds the request (for human readability and grep) and the response:

```json
{
  "request": {
    "to": "jess@example.com",
    "templateId": "welcome-v2",
    "variables": [["name", "Jess"]]
  },
  "response": {
    "messageId": "msg_abc123",
    "status": "queued"
  }
}
```

Lookup order when `--integrations=fake` is active:

1. Compute the canonical-JSON SHA-256 of the request.
2. If `tests/fixtures/<integration-name>/<hash>.json` exists, decode its `response` field as `Response request` and return it.
3. Otherwise, invoke the instance's `runFake` (default: Arbitrary).

No declarative matcher DSL. A fixture matches one specific request and nothing else. To share a response across many requests, override `runFake` with hand-written logic — that is the established escape hatch.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Hash-keyed files (SHA-256 of canonical JSON) | **Chosen** | Deterministic, grep-friendly, no manifest to maintain. Drop in a file, it works. Canonical JSON normalises field order so hand-edits stay stable. |
| Named fixture files with a manifest mapping scenario-name → request-matcher | Rejected | Introduces a matcher DSL for no gain in the common case (record-and-replay). Adds a manifest file that must stay in sync with fixture files. |
| A single fixture JSONL per integration | Rejected | Harder to diff, harder to hand-edit, harder to share across branches without conflicts. |

### 5. Fixture Recorder

The recorder is a library helper — not a new tool — to avoid blocking this ADR on the unfinished `cli/` executable.

```haskell
module Test.Integration.Fixture
  ( record
  , promote
  , RedactionRule (..)
  , defaultRedactionRules
  ) where

record ::
  forall request.
  (Integration request, ToJSON request) =>
  Array RedactionRule ->
  Array request ->
  Task IntegrationError ()

promote :: forall request. (Integration request) => Array Text -> Task IntegrationError ()
```

A test file invokes `TestIntegration.Fixture.record @SendEmail defaultRedactionRules [req1, req2]`. When the environment variable `NEOHASKELL_RECORD_FIXTURES=1` is set, the helper:

1. Calls `runReal` on each request (sandbox credentials must be present in the environment).
2. Applies the supplied `RedactionRule`s to both request and response (see below).
3. Runs the **entropy / secret scan** over the redacted payload — recognising patterns like `sk_`, `pk_`, `Bearer `, `whsec_`, `rk_`, JWT shapes (`eyJ...`), 32+ character hex tokens, Base64-encoded blobs longer than 48 characters, and any string whose Shannon entropy exceeds a threshold. A match causes `record` to abort before writing unless the specific rule is explicitly acknowledged via `RedactionRule.allow`.
4. Serialises the (redacted) request and response into the fixture file layout.
5. Writes to `<projectRoot>/tests/fixtures/local/<integration-name>/<hash>.json` by default. `tests/fixtures/local/` is added to `.gitignore` by the default `neohaskell new` template and is **never committed**. Committed fixtures live at `<projectRoot>/tests/fixtures/<integration-name>/<hash>.json`; files move from `local/` to the committed tree only via the explicit `promote` step.

**Promoting fixtures.** `TestIntegration.Fixture.promote @SendEmail ["msg_abc123", "msg_xyz789"]` moves selected local recordings (identified by hash prefix or response key) into the committed tree, after re-running the redaction and entropy scans and requiring the environment variable `NEOHASKELL_PROMOTE_FIXTURES=1`. The two-step record/promote flow ensures that the moment of commit is a deliberate human action — no test run can implicitly ship a sandbox response containing real PII to the shared repository. Nick reviews the local recording, confirms it is safe, then promotes.

**Redaction hooks.** `RedactionRule` encodes transformations the author wants applied to recorded traffic: `redactJsonPath "$.customer.email" "<redacted-email>"`, `redactHeader "Authorization" "<redacted>"`, `redactMatching regex "<redacted>"`, and so on. `defaultRedactionRules` includes rules for common identifier and secret shapes (email addresses, phone numbers, signed-URL query params). Authors add integration-specific rules in the test file.

When `NEOHASKELL_RECORD_FIXTURES` is unset, `record` is a no-op. This lets the recorder stay embedded in the test suite without hitting the network on every test run. `record` and `promote` live under `Test.Integration.Fixture` — a test-only module, not re-exported from production entry points — so production code cannot invoke them.

The long-term path is a `neohaskell integration record <test>` subcommand in `cli/`. That is explicit future work — out of scope here.

### 6. Contract Tests — Schema Drift Detection

For each `Integration` instance, the framework auto-generates a single property test with three assertions:

1. **Fake output parses as Response.** Generate `N = 100` arbitrary requests, call `runFake`, assert each response round-trips through `Json.encode` (which dispatches through `toEncoding`, avoiding an intermediate `Value` tree) and `Json.decode` cleanly.
2. **Real output parses as Response.** When `NEOHASKELL_CONTRACT_SANDBOX=1` is set and sandbox credentials are available, generate arbitrary requests, call `runReal`, assert each response parses.
3. **Structural compatibility.** Both fake and real outputs must satisfy the same `ToSchema` shape derived from `Response request`.

Request and response types should define `toEncoding` (via `genericToEncoding defaultOptions`) so the round-trip and the canonical-JSON hashing path both avoid the quadratic `Value`-tree construction cost. This is an instance-level concern but must be called out in the integration authoring guide so Jess doesn't silently land `toJSON`-only instances that halve throughput in fixture-lookup mode.

Contract tests live in a test-suite target, not the production executable. Its QuickCheck dependency is consequently linked only into test binaries, keeping the production binary free of the test-framework surface.

Value-level equivalence is **not** asserted — the real Sendgrid API will not return `messageId = "xyz"` deterministically. Drift is detected at the schema level: if Sendgrid adds a required field to the response and the fake doesn't produce it, the fake's output fails `FromJSON` and the test fails.

When drift is detected, the integration author's fix path:

1. Update the `Response` record to match the new real shape.
2. The Arbitrary instance regenerates automatically (Generic derivation).
3. If `runFake` was overridden, update it.
4. Re-record affected fixtures with the recorder helper.

Contract tests run on every PR. The sandbox-hitting assertion (#2) runs only when the env var is set — typical CI has this on, typical local runs have it off.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Value-level equivalence (fake must return exactly what real returns) | Rejected | Real APIs are non-deterministic (message IDs, timestamps). Test flakes dominate the signal. |
| Golden-file capture of real responses, diffed structurally on every run | Rejected | Conflates fixture management with drift detection. Fixtures are project-owned; contract tests are framework-owned. |
| Schema-level drift detection via `ToSchema` | **Chosen** | Cheap, deterministic, catches the failure mode this ADR is meant to mitigate. Leverages existing ADR-0013 schema infrastructure. |

### 7. Property-Based Integration Tests

The same harness that powers contract tests is exposed as a user-facing helper, shipped under the `Test.Integration.Property` module so it cannot be imported from production code:

```haskell
module Test.Integration.Property (fakeProperty) where

fakeProperty ::
  forall request.
  (Integration request, QuickCheck.Arbitrary request, Show request) =>
  (request -> Response request -> QuickCheck.Property) ->
  QuickCheck.Property
fakeProperty check = QuickCheck.property \req -> QuickCheck.ioProperty do
  response <- Task.runOrThrow (runFake req)
  Task.yield (check req response)
```

The `Show request` constraint remains (so QuickCheck can print the shrunk counter-example). There is deliberately **no `Show (Response request)` constraint**: responses may contain `Redacted` fields whose `Show` instance prints `<redacted>`, and requiring `Show` at the property-helper boundary would force Nick to derive `Show` on response types containing secrets — exactly the red line. Counter-example output prints the request and a truncated hash of the response; authors who want richer failure diagnostics can destructure fields inside their property body and pass only the public-safe values back to QuickCheck.

The name is `fakeProperty` (not `property`) to make it unambiguous that the invariant is checked against `runFake`, not `runReal`. Jess uses it:

```haskell
test_sendEmailInvariant :: TestTree
test_sendEmailInvariant =
  Test.quickCheck "SendEmail response has a non-empty messageId" do
    TestIntegration.fakeProperty @SendEmail \_req response ->
      response.messageId /= ""
```

Stateful property testing (`quickcheck-state-machine`-style) is out of scope. The helper runs each generated request in isolation against `runFake`.

### 8. Module Placement

Production modules (linked into every NeoHaskell application binary):

```text
core/service/Service/Integration/
  Adapter.hs       -- Integration, InboundIntegration typeclasses (no Arbitrary superclass)
  Canonical.hs     -- First-party RFC 8785 canonical-JSON encoder + SHA-256 hasher (see §12)
  Fixture.hs       -- Fixture lookup and path validation (read-only in production)
  Selection.hs     -- CLI flag + env-var gate parsing, wiring selection, dispatcher shim
  Debug.hs         -- IntegrationDebug structured-log carrier with Redacted fields (see §11)
```

Test-only modules (linked only into `test-suite` targets, shipped with nhcore but never into production binaries):

```text
core/service/Test/Integration/
  Fixture.hs       -- Fixture.record and Fixture.promote (write helpers)
  Contract.hs      -- Auto-generated contract-test harness
  Property.hs      -- User-facing property-test helper (fakeProperty)
  Simulate.hs      -- simulate :: Trigger inbound -> Task IntegrationError ()
```

The `Test.Integration` namespace sits inside the `nhcore` package but in a separate cabal library stanza (`library testing`, exposed modules start with `Test.Integration`). Application `test-suite` targets add `nhcore:testing` to `build-depends`; production `executable` targets do not. This enforces at compile time that `simulate`, `record`, `promote`, and `fakeProperty` cannot be called from production code paths.

`Integration.hs` (the existing top-level entry point) re-exports **only the production-safe surface**: `Integration (..)`, `InboundIntegration (..)`, `IntegrationError (..)`. It does **not** re-export `simulate`, `record`, `promote`, or `fakeProperty` — those are reachable only via `Test.Integration.*`.

Follows the precedent set by ADR-0049's `Service/OutboundIntegration/` layout — one directory for a concept with multiple sub-modules — extended with a parallel `Test/Integration/` tree for test-only helpers.

### 9. Supersession of Prior ADRs

- [ADR-0008](0008-integration-pattern.md): the `ToAction` typeclass, `Integration.outbound`, and the imperative `toAction` body are replaced by the `Integration` typeclass of this ADR. The per-entity dispatch model and `Integration.batch` / `Integration.none` API remain untouched.
- [ADR-0049](0049-outboundintegration-typeclass-dispatch-generation.md): the `OutboundIntegration` typeclass and the `outboundIntegration` TH macro remain. The change is to the adapter layer below: handlers now emit typed `Integration request` values instead of opaque `Action` values. `handleEvent` returns `Integration.Outbound` as before, but the actions inside it are declarative `Integration` dispatches.
- ADR-0008's inbound section is superseded by `InboundIntegration` in this ADR. The `Integration.Inbound` worker record is replaced by the `InboundIntegration` typeclass with symmetric `runReal` / `runFake`.

Backwards compatibility is not a priority. The testbed and existing integration packages are updated in the same change set as the framework code.

### 10. Public API Surface

Production modules (re-exported from `Integration` and `Core`):

```haskell
module Service.Integration.Adapter (
  Integration (..),
  InboundIntegration (..),
  IntegrationError (..),
) where

class Integration request where
  type Response request :: Type
  runReal :: request -> Task IntegrationError (Response request)
  runFake :: request -> Task IntegrationError (Response request)
  default runFake ::
    (QuickCheck.Arbitrary (Response request)) =>
    request ->
    Task IntegrationError (Response request)

class InboundIntegration inbound where
  type Trigger inbound :: Type
  runReal :: (Trigger inbound -> Task IntegrationError ()) -> Task IntegrationError Void
  runFake :: InboundHandle (Trigger inbound)
  default runFake ::
    (QuickCheck.Arbitrary (Trigger inbound)) =>
    InboundHandle (Trigger inbound)
```

Test-only modules (re-exported from `Test.Integration` only, inside the `nhcore:testing` library stanza):

```haskell
module Test.Integration.Simulate (simulate) where

simulate ::
  forall inbound.
  (InboundIntegration inbound) =>
  Trigger inbound ->
  Task IntegrationError ()

module Test.Integration.Fixture (record, promote, RedactionRule (..), defaultRedactionRules) where

record ::
  forall request.
  (Integration request, ToJSON request) =>
  Array RedactionRule ->
  Array request ->
  Task IntegrationError ()

promote ::
  forall request.
  (Integration request) =>
  Array Text ->
  Task IntegrationError ()

module Test.Integration.Property (fakeProperty) where

fakeProperty ::
  forall request.
  (Integration request, QuickCheck.Arbitrary request, Show request) =>
  (request -> Response request -> QuickCheck.Property) ->
  QuickCheck.Property
```

Production code imports `Integration` (top-level) or `Core` and gets only the typeclass surface. Test code imports `Test.Integration` and gets the simulation, recording, and property helpers.

### 11. Error Discipline

`IntegrationError` payloads are **public-safe text only**. No request bodies, no headers, no URLs containing tokens, no environment-variable values, no rendered response bodies. The contract is enforced at the type level by narrowing the payload constructors:

```haskell
data IntegrationError
  = TransportFailure Text     -- e.g. "connection reset"
  | AuthenticationFailure     -- no payload; the cause is secret by nature
  | PermanentFailure Text     -- e.g. "sendgrid rejected: invalid email syntax"
  | TransientFailure Text     -- e.g. "sendgrid returned 503"
  | ValidationFailure Text    -- e.g. "templateId missing"
  deriving (Eq, Show)
```

`AuthenticationFailure` carries no payload deliberately — there is no safe free-form text for "your API key was rejected" that cannot also contain the rejected key or a portion of it. The `Text` payloads on other constructors are contractually public-safe; reviewers reject instances that pass request bodies, response bodies, or secrets into them.

Sensitive diagnostic context travels through a parallel `IntegrationDebug` structured log, which carries `Redacted` fields and is only emitted to the application's structured logger at `Debug` level:

```haskell
module Service.Integration.Debug (IntegrationDebug (..), log) where

data IntegrationDebug = IntegrationDebug
  { integrationName :: Text
  , requestHash :: Text             -- hex SHA-256, not request body
  , statusCode :: Maybe Int
  , authHeader :: Redacted Text
  , responseSnippet :: Redacted Text
  }
  deriving (Generic)

-- no ToJSON instance; the logger renders via a Redacted-aware formatter
log :: IntegrationDebug -> Task never ()
```

`requestHash` is the same canonical-JSON SHA-256 used for fixture lookup, so a log line cross-references the fixture file in a safe, token-free way.

**Contract-test failure output**. When a contract-test assertion fails, the default QuickCheck shrink/print path would render the full request and response. The framework intercepts that path: if `Response request` contains a `Redacted` field, or if the response size exceeds a threshold, the failure output prints only the request hash and the failing assertion, pointing the author at the fixture file for inspection. For non-`Redacted` responses, the full request/response is printed as today, because that is the debuggability payoff of the tests.

### 12. Canonical JSON — RFC 8785 First-Party Implementation

Fixture hashing requires a deterministic canonical form. The Haskell ecosystem has no well-maintained RFC 8785 / JCS implementation (`canonical-json` on Hackage implements an older scheme used by the cabal signature format, not JCS). Rather than depend on an unmaintained library or build our own bespoke canonical form, nhcore ships a first-party RFC 8785 encoder:

```haskell
module Service.Integration.Canonical (encode, hash) where

encode :: (ToEncoding request) => request -> ByteString
hash :: (ToEncoding request) => request -> Text   -- lowercase hex SHA-256 of encode
```

Implementation constraints:

- Routes through `toEncoding` (not `toJSON`) to avoid the intermediate `Value` tree.
- Object keys are UTF-16-sorted per RFC 8785 §3.2.3 (not bytewise UTF-8-sorted).
- Numbers are serialised per ECMA-262 §7.1.12.1 (IEEE 754 + "shortest round-trippable" formatting).
- Rejects `NaN` and `±Infinity` at encode time with a `ValidationFailure`.
- String escape rules follow RFC 8785 §3.2.2.2 (minimal escaping, control-char passthrough as `\uXXXX`).

The implementation is backed by the full official RFC 8785 test-vector suite, committed to `core/service/test/Canonical/Vectors/`. The hashing algorithm and canonical form are versioned: `Canonical.version :: Int` is exposed so a future bug-fix release can announce a framework-level version bump and known-broken fixture hashes. Pinning the canonicaliser to a first-party module means a platform-differing output bug is a framework bug, not a "which JCS library are you on?" coordination problem.

The `Canonical` module is production-linked (needed for fixture *lookup* in fake mode, and used by `Integration.Debug` for request hashes), but its public surface is the two functions above — no extension points that could be misused to influence the hash.

## Consequences

### Positive

- **Fake-by-default is free.** Any integration whose response type derives `Generic` and `Arbitrary` (routine for plain records) gets a usable fake with zero extra code from Nick.
- **Production dispatch links no test framework.** `Arbitrary` is a constrained-default requirement, not a superclass constraint. A production build whose integrations all override `runFake` (or whose response types omit `deriving Arbitrary`) links zero QuickCheck code into the outbound dispatch path. Test binaries pay the `Arbitrary` cost; production binaries do not.
- **One binary, two deployment surfaces.** Test mode is a CLI flag gated by an environment variable, not a compile-time split. The binary that runs in production is the binary Jess tests with — but a misconfigured CLI flag alone cannot silently disable real integrations in production (see §3).
- **Secret-bearing integrations cannot accidentally leak.** The `Redacted` requirement on secret fields means response types containing secrets cannot derive `Show`/`Arbitrary`/`ToJSON` in one line, which forces the author to define `runFake` explicitly — ruling out an Arbitrary-generated response that contains a fake-looking "real" token in CI logs.
- **Drift is caught automatically.** Silent divergence — the failure mode ADR-0008 and ADR-0049 ignored entirely — is now detected by a framework-generated property test on every PR.
- **Fixture layout is greppable and safe.** SHA-256-keyed JSON files with embedded request bodies let a developer grep `tests/fixtures/` for any string; paths are prefix-validated against the project root so traversal is impossible.
- **Recorded fixtures never ship secrets.** Recordings default to a gitignored `tests/fixtures/local/` directory, pass through an entropy / known-prefix scan, and require an explicit `promote` step to land in the committed tree.
- **Inbound and outbound have the same shape.** `Test.Integration.simulate` is the inbound analogue of "call the fake" — tests look symmetric, and `simulate` is unreachable from production module paths.
- **Property tests are one helper call.** The same harness that validates contracts is available to application authors for domain invariants via `Test.Integration.fakeProperty`.

### Negative

- **Response types wanting the default fake must have `Arbitrary`.** For custom records this is a one-liner via `Generic`, but exotic types (opaque identifiers, ADTs with business invariants) require Nick to hand-write an `Arbitrary` instance — or override `runFake`.
- **Hash-keyed fixtures are not hand-authorable from scratch.** A human cannot sit down and write a fixture without first generating the canonical-JSON hash of a request. In practice authors record-and-edit, but the ergonomics are worse than a named-scenario file.
- **Secret-bearing integrations cost more to fake.** Because `Redacted` fields block the Arbitrary default, authors of Stripe-like integrations must write `runFake` explicitly. This is deliberate (prevents accidental secret serialisation) but is additional work relative to the "pure record, pure response" happy path.
- **Breaks ADR-0008 and ADR-0049's adapter layer.** Every integration package and testbed example that today defines `ToAction` or calls `Integration.outbound` on a config record must be rewritten. Scope is manageable (few dozen call sites) but not trivial.
- **Stateful fakes have no framework scaffolding.** Nick who writes a payment state machine rolls his own `IORef`/`TVar` storage. Different stateful integrations will diverge in how they model state. Deliberate — a framework state-machine DSL is premature — but it does mean test-code ergonomics vary per integration.
- **Two-library cabal split adds build complexity.** Splitting `Test.Integration.*` into a separate `nhcore:testing` library stanza means test-suite targets must list `nhcore:testing` in addition to `nhcore`. Mitigated by updating the `neohaskell new` template.
- **First-party RFC 8785 encoder is code nhcore now owns.** No external library drops in — we maintain the JCS implementation and its conformance tests in-tree. Weighed against the zero maintained alternatives in the Haskell ecosystem, this is a net positive.

### Risks

1. **Schema drift detection misses semantic drift.** Sendgrid could change the meaning of `status = "queued"` without changing the response shape. The framework won't catch it. This is inherent to schema-level checking and is explicitly the tradeoff chosen in §6.
2. **Sandbox-hitting contract tests introduce flakes.** When `NEOHASKELL_CONTRACT_SANDBOX=1` is on, network instability can fail PR builds unrelated to the change. The mitigation is that this env var is opt-in, typically run on a scheduled job rather than per-PR.
3. **Fixture file churn on canonical-JSON changes.** If `Service.Integration.Canonical` gains a bug fix that changes byte-level output, existing fixture hashes no longer match. Mitigation: `Canonical.version` is published; a version bump invalidates fixtures explicitly, and the migration path is a re-record pass.
4. **Arbitrary-generated data can violate integration invariants.** Generated `SendEmail.to` might not be a valid email; the real API would reject it but `runFake` happily returns a generated response. Mitigation: `runFake` is a stub, not a simulator — tests asserting real-API validation behaviour must override `runFake` or use fixtures.
5. **`NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=1` could leak into production environments.** A deployment copied from staging to production might retain the env var. Mitigation: startup logs an `ERROR`-level line naming active fakes; `/health` exposes `integrations.mode`; deploy checks can enforce the value. This is defence-in-depth — the env var gate is still strictly better than a CLI flag alone.
6. **Contract-test output redaction is a heuristic.** The rule "if the `Response` type has a `Redacted` field, print only the hash" is coarse. A response containing both a secret and a non-secret field will have its non-secret field hidden in failure output too, which hurts debuggability. Mitigation: authors can shrink/print explicit fields inside the property body when the response type is known.

### Mitigations

- **Arbitrary cost**: ship `Generic`-derived defaults in framework code, document the one-liner for custom types, provide `deriving Arbitrary` in the style guide.
- **Fixture authoring ergonomics**: the recorder handles the common case. For hand-authoring, documentation shows the pattern: write the request as a Haskell literal in a scratch test, print the canonical JSON and its hash, save the file.
- **Migration**: ADR-0008 and ADR-0049's adapter call sites are updated in the same PR series that lands this ADR. The testbed serves as the reference migration.
- **Stateful fake patterns**: a follow-up ADR may introduce a state-machine helper module once two or three stateful integrations have shipped and their shared shape is clear. Until then, Nick writes the `IORef`/`TVar` plumbing directly.
- **Canonical JSON conformance**: the RFC 8785 test-vector suite is committed under `core/service/test/Canonical/Vectors/` and is gating on every PR. Any change to canonical output that doesn't also bump `Canonical.version` is a CI failure.
- **Test-only library boundary**: `nhcore:testing` is documented as test-suite-only. The `neohaskell new` project template wires `nhcore:testing` into the test-suite stanza by default so authors never need to think about the split.

## Future Work

- `neohaskell integration record <test>` subcommand in `cli/` once the CLI executable lands.
- A state-machine helper library for stateful fakes, once usage patterns are established across multiple integrations.
- Time-travel / replay of production traffic into the fixture store.
- Observability for fake-vs-real traffic in hybrid mode.

## References

- [ADR-0008: Integration Pattern](0008-integration-pattern.md) — superseded adapter layer (`ToAction`, `Integration.Inbound` worker).
- [ADR-0013: Automatic Schema Generation](0013-automatic-schema-generation.md) — `ToSchema` infrastructure reused by contract tests.
- [ADR-0021: Declarative Config DSL](0021-declarative-config-dsl.md) — selection flag parsing integrates here.
- [ADR-0049: OutboundIntegration Typeclass with Typed Event Dispatch](0049-outboundintegration-typeclass-dispatch-generation.md) — outer dispatch layer whose handlers produce `Integration request` values.
- [core/service/Integration.hs](../../core/service/Integration.hs) — current top-level re-exports, updated by this ADR.
- [core/service/Service/Integration/Types.hs](../../core/service/Service/Integration/Types.hs) — `OutboundRunner` type-erased runner, adapted to dispatch via the new typeclass.
- [core/service/Service/OutboundIntegration/Core.hs](../../core/service/Service/OutboundIntegration/Core.hs) — `OutboundIntegration` typeclass (unchanged).
- [RFC 8785: JSON Canonicalization Scheme (JCS)](https://datatracker.ietf.org/doc/html/rfc8785) — canonical JSON used for fixture hashing.
