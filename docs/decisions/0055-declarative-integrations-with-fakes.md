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

An `Integration` is a request type, a response type, and a `runReal` function. `runFake` has a default implementation that calls `QuickCheck.generate arbitrary @Response`. Nick overrides `runFake` only when the default is insufficient.

This supersedes the `ToAction` typeclass from [ADR-0008](0008-integration-pattern.md) and replaces the raw `runReal`-equivalent body that today lives inside `OutboundIntegration` handlers from [ADR-0049](0049-outboundintegration-typeclass-dispatch-generation.md).

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Two typeclass instances (`ToAction (Real Cfg)`, `ToAction (Fake Cfg)`) selected by a wrapper | Rejected | Forces Nick to write two instances even in the common zero-state case. Wrapper types pollute every call site. Cannot auto-derive the fake. |
| A single `Adapter req resp` record with `runReal`/`runFake` function fields | Rejected | Records cannot carry a default — Nick must always supply both. No hook for the framework to inject Arbitrary-based fallback. Value-level dispatch prevents per-instance fixture lookup without runtime reflection. |
| A typeclass with `runReal` as a method and `runFake` as a method with a default implementation | **Chosen** | Zero-ceremony default (`runFake` falls through to Arbitrary). Override is trivial (define `runFake` on the instance). Framework can inject fixture lookup and contract-test generation via type-class machinery already used for `Command` and `OutboundIntegration`. |

```haskell
module Service.Integration.Adapter (Integration (..)) where

import Core
import Task qualified
import Test.QuickCheck qualified as QuickCheck

class (Arbitrary (Response request)) => Integration request where
  type Response request :: Type

  runReal :: request -> Task IntegrationError (Response request)

  runFake :: request -> Task IntegrationError (Response request)
  runFake _ = do
    generated <- Task.fromIO (QuickCheck.generate QuickCheck.arbitrary)
    Task.yield generated
```

**What Nick writes for the zero-state case:**

```haskell
data SendEmail = SendEmail
  { to :: !Text
  , templateId :: !Text
  , variables :: !(Array (Text, Text))
  }
  deriving (Generic, Arbitrary, ToJSON, FromJSON, Show)

data SendEmailResponse = SendEmailResponse
  { messageId :: !Text
  , status :: !Text
  }
  deriving (Generic, Arbitrary, ToJSON, FromJSON, Show)

instance Integration SendEmail where
  type Response SendEmail = SendEmailResponse
  runReal req = do
    apiKey <- Environment.require "SENDGRID_API_KEY"
    Http.post "https://api.sendgrid.com/v3/mail/send"
      |> Http.bearer apiKey
      |> Http.jsonBody req
      |> Http.send
      |> Task.mapError toIntegrationError
```

No `runFake` needed — default kicks in.

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
class (Arbitrary (Trigger inbound)) => InboundIntegration inbound where
  type Trigger inbound :: Type

  runReal :: (Trigger inbound -> Task IntegrationError ()) -> Task IntegrationError Void

  runFake :: InboundHandle (Trigger inbound)
  runFake = Inbound.controllableHandle
```

In test mode, `Integration.simulate @StripeWebhook payload` fires the emitter synchronously. The default `runFake` returns a framework-provided handle that exposes a push channel for tests. Nick overrides only when the inbound needs its own autonomous behaviour in smoke tests (e.g. a fake file watcher that periodically emits synthetic files).

### 3. Selection via `Application.run`

Selection happens in the default CLI surface of `Application.run`. No Haskell source change is required to flip from real to fake.

| Flag | Meaning |
|------|---------|
| `--integrations=real` | Default. All integrations call `runReal`. |
| `--integrations=fake` | All integrations call `runFake` (with fixture fallback). |
| `--integrations=hybrid --fake=Sendgrid --fake=Stripe` | Named integrations use `runFake`, the rest use `runReal`. Useful for staging. |

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Environment variable (`NEOHASKELL_INTEGRATIONS=fake`) | Rejected | Invisible in process listings; easy to forget in shell scripts; inconsistent with NeoHaskell's declarative CLI story. |
| Per-integration wiring in `Application` builder (`Application.withFake @Sendgrid`) | Rejected | Forces test suites and test binaries to diverge from production binaries at compile time. Kills the "same binary, different flag" promise. |
| Separate `Application.test` builder | Rejected | Duplicates `Application` surface area. Every new `with*` helper would need a test-mode twin. |
| CLI flag parsed by `Application.run` | **Chosen** | Same binary, different flag. Visible in process listings and CI logs. Composable with the existing declarative config DSL (ADR-0021). |

The flag is read by the runtime's wiring layer before dispatch is constructed. Each `Integration` instance is wrapped in a dispatcher shim that consults the selection and routes to `runReal`, `runFake`, or the fixture store.

### 4. Fixture Storage and Lookup

Fixtures live under `tests/fixtures/<integration-name>/<hash>.json` where:

- `<integration-name>` is `NameOf request` (e.g. `SendEmail`, matching the existing `NameOf` type family used by commands and queries).
- `<hash>` is the SHA-256 of the request serialised to canonical JSON (RFC 8785 / JCS: sorted keys, no insignificant whitespace).

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
module Service.Integration.Fixture
  ( record
  ) where

record :: forall request. (Integration request, ToJSON request) => Array request -> Task IntegrationError ()
```

A test file invokes `Integration.Fixture.record @SendEmail [req1, req2]`. When the environment variable `NEOHASKELL_RECORD_FIXTURES=1` is set, the helper:

1. Calls `runReal` on each request (sandbox credentials must be present in the environment).
2. Serialises request and response into the fixture file layout.
3. Writes to `tests/fixtures/<integration-name>/<hash>.json`, overwriting any existing fixture for that hash.

When the variable is unset, `record` is a no-op. This lets the recorder stay embedded in the test suite without hitting the network on every test run.

The long-term path is a `neohaskell integration record <test>` subcommand in `cli/`. That is explicit future work — out of scope here.

### 6. Contract Tests — Schema Drift Detection

For each `Integration` instance, the framework auto-generates a single property test with three assertions:

1. **Fake output parses as Response.** Generate `N = 100` arbitrary requests, call `runFake`, assert each response round-trips through `FromJSON . ToJSON` cleanly.
2. **Real output parses as Response.** When `NEOHASKELL_CONTRACT_SANDBOX=1` is set and sandbox credentials are available, generate arbitrary requests, call `runReal`, assert each response parses.
3. **Structural compatibility.** Both fake and real outputs must satisfy the same `ToSchema` shape derived from `Response request`.

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

The same harness that powers contract tests is exposed as a user-facing helper:

```haskell
module Service.Integration.Property (property) where

property ::
  forall request.
  (Integration request, Show request, Show (Response request)) =>
  (request -> Response request -> QuickCheck.Property) ->
  QuickCheck.Property
property check = QuickCheck.property \req -> QuickCheck.ioProperty do
  response <- Task.runOrThrow (runFake req)
  Task.yield (check req response)
```

Jess uses it:

```haskell
test_sendEmailInvariant :: TestTree
test_sendEmailInvariant =
  Test.quickCheck "SendEmail response has a non-empty messageId" do
    Integration.property @SendEmail \_req response ->
      response.messageId /= ""
```

Stateful property testing (`quickcheck-state-machine`-style) is out of scope. The helper runs each generated request in isolation against `runFake`.

### 8. Module Placement

```text
core/service/Service/Integration/
  Adapter.hs       -- Integration, InboundIntegration typeclasses
  Fixture.hs       -- Fixture lookup, hash computation, Fixture.record recorder
  Contract.hs      -- Auto-generated contract-test harness
  Property.hs      -- User-facing property-test helper
  Selection.hs     -- CLI flag parsing, wiring selection, dispatcher shim
```

`Integration.hs` (the existing top-level entry point) re-exports `Integration (..)`, `InboundIntegration (..)`, `simulate`, and `Fixture.record` so Jess has one import path.

Follows the precedent set by ADR-0049's `Service/OutboundIntegration/` layout — one directory for a concept with multiple sub-modules.

### 9. Supersession of Prior ADRs

- [ADR-0008](0008-integration-pattern.md): the `ToAction` typeclass, `Integration.outbound`, and the imperative `toAction` body are replaced by the `Integration` typeclass of this ADR. The per-entity dispatch model and `Integration.batch` / `Integration.none` API remain untouched.
- [ADR-0049](0049-outboundintegration-typeclass-dispatch-generation.md): the `OutboundIntegration` typeclass and the `outboundIntegration` TH macro remain. The change is to the adapter layer below: handlers now emit typed `Integration request` values instead of opaque `Action` values. `handleEvent` returns `Integration.Outbound` as before, but the actions inside it are declarative `Integration` dispatches.
- ADR-0008's inbound section is superseded by `InboundIntegration` in this ADR. The `Integration.Inbound` worker record is replaced by the `InboundIntegration` typeclass with symmetric `runReal` / `runFake`.

Backwards compatibility is not a priority. The testbed and existing integration packages are updated in the same change set as the framework code.

### 10. Public API Surface

```haskell
module Service.Integration.Adapter (
  Integration (..),
  InboundIntegration (..),
  IntegrationError (..),
  simulate,
) where

class (Arbitrary (Response request)) => Integration request where
  type Response request :: Type
  runReal :: request -> Task IntegrationError (Response request)
  runFake :: request -> Task IntegrationError (Response request)

class (Arbitrary (Trigger inbound)) => InboundIntegration inbound where
  type Trigger inbound :: Type
  runReal :: (Trigger inbound -> Task IntegrationError ()) -> Task IntegrationError Void
  runFake :: InboundHandle (Trigger inbound)

simulate :: forall inbound. (InboundIntegration inbound) => Trigger inbound -> Task IntegrationError ()

module Service.Integration.Fixture (record) where

record :: forall request. (Integration request, ToJSON request) => Array request -> Task IntegrationError ()

module Service.Integration.Property (property) where

property ::
  (Integration request, Show request, Show (Response request)) =>
  (request -> Response request -> QuickCheck.Property) ->
  QuickCheck.Property
```

Re-exported from `Integration` (top-level) and `Core`.

## Consequences

### Positive

- **Fake-by-default is free.** Any integration whose response type derives `Generic` and `Arbitrary` (routine for plain records) gets a usable fake with zero extra code from Nick.
- **One binary, one codepath.** Test mode is a CLI flag, not a compile-time split. The binary that runs in production is the binary Jess tests with.
- **Drift is caught automatically.** Silent divergence — the failure mode ADR-0008 and ADR-0049 ignored entirely — is now detected by a framework-generated property test on every PR.
- **Fixture layout is greppable.** SHA-256-keyed JSON files with embedded request bodies let a developer grep `tests/fixtures/` for any string and find the relevant scenario.
- **Inbound and outbound have the same shape.** `simulate` is the inbound analogue of "call the fake" — tests look symmetric.
- **Property tests are one helper call.** The same harness that validates contracts is available to application authors for domain invariants.

### Negative

- **Every response type must have `Arbitrary`.** For custom records this is a one-liner via `Generic`, but exotic types (opaque identifiers, ADTs with business invariants) require Nick to hand-write an Arbitrary instance. This is the cost of zero-ceremony defaults.
- **Hash-keyed fixtures are not hand-authorable from scratch.** A human cannot sit down and write a fixture without first generating the canonical-JSON hash of a request. In practice authors record-and-edit, but the ergonomics are worse than a named-scenario file.
- **Breaks ADR-0008 and ADR-0049's adapter layer.** Every integration package and testbed example that today defines `ToAction` or calls `Integration.outbound` on a config record must be rewritten. Scope is manageable (few dozen call sites) but not trivial.
- **Stateful fakes have no framework scaffolding.** Nick who writes a payment state machine rolls his own `IORef`/`TVar` storage. Different stateful integrations will diverge in how they model state. Deliberate — a framework state-machine DSL is premature — but it does mean test-code ergonomics vary per integration.

### Risks

1. **Schema drift detection misses semantic drift.** Sendgrid could change the meaning of `status = "queued"` without changing the response shape. The framework won't catch it. This is inherent to schema-level checking and is explicitly the tradeoff chosen in Decision 6.
2. **Sandbox-hitting contract tests introduce flakes.** When `NEOHASKELL_CONTRACT_SANDBOX=1` is on, network instability can fail PR builds unrelated to the change. The mitigation is that this env var is opt-in, typically run on a scheduled job rather than per-PR.
3. **Fixture file churn on canonical-JSON changes.** If the RFC 8785 canonicaliser is changed (library update, bug fix), existing fixture hashes no longer match. The mitigation is to pin the canonicaliser library version and treat hash-scheme changes as a breaking framework release.
4. **Arbitrary-generated data can violate integration invariants.** Generated `SendEmail.to` might not be a valid email; the real API would reject it but `runFake` happily returns a generated response. The mitigation is that `runFake` is a stub, not a simulator — tests asserting real-API validation behaviour must override `runFake` or use fixtures.

### Mitigations

- **Arbitrary cost**: ship `Generic`-derived defaults in framework code, document the one-liner for custom types, provide `deriving Arbitrary` in the style guide.
- **Fixture authoring ergonomics**: the recorder handles the common case. For hand-authoring, documentation shows the pattern: write the request as a Haskell literal in a scratch test, print the canonical JSON and its hash, save the file.
- **Migration**: ADR-0008 and ADR-0049's adapter call sites are updated in the same PR series that lands this ADR. The testbed serves as the reference migration.
- **Stateful fake patterns**: a follow-up ADR may introduce a state-machine helper module once two or three stateful integrations have shipped and their shared shape is clear. Until then, Nick writes the `IORef`/`TVar` plumbing directly.

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
