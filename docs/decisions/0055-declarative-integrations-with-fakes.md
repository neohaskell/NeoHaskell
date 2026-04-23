# ADR-0055: Real/Fake Parity for Outbound Integrations

## Status

Proposed

## Context

### Current State

NeoHaskell has two ADRs governing outbound integrations:

- [ADR-0008](0008-integration-pattern.md) defined an imperative `ToAction` typeclass: Nick writes `toAction :: config -> Action` where `Action` wraps a `Task IntegrationError (Maybe CommandPayload)`. HTTP calls, retries, and authentication live inside the `ToAction` instance body.
- [ADR-0049](0049-outboundintegration-typeclass-dispatch-generation.md) layered an `OutboundIntegration` typeclass on top for typed event dispatch, but left `ToAction` untouched as the adapter mechanism.

Neither layer says anything about testing. Today a NeoHaskell application that depends on Sendgrid, Stripe, or any other third-party API has three unattractive options:

1. **Run against a real sandbox.** Requires credentials in CI, non-determinism, network flakes, rate limits.
2. **Hand-build a mock per project.** Every application reinvents the same fake.
3. **Skip integration tests entirely.** The majority path in practice.

There is no framework-level affordance for swapping in a fake at wire-up time and no way to boot the binary in a "no real network calls" mode.

### Use Cases

- **Jess writes a unit test for a command handler that emits an outbound Sendgrid email.** She does not have Sendgrid credentials and does not want the test to hit the network.
- **Nick boots the application binary in `--integrations=fake` mode for a local dev or acceptance smoke test.** Every outbound integration returns a plausible-looking response without leaving the process.
- **A team runs end-to-end acceptance tests against the same binary they ship**, with fake outbound calls so the tests are deterministic and offline.

### Design Goals

1. **Real/fake parity is a contract, not a convention.** Every `Integration` instance ships both `runReal` (the production logic) and `runFake` (a mock implementation for testing). The typeclass enforces it: an instance without both does not compile.
2. **Zero-ceremony default fakes.** If Nick does nothing special, `runFake` is auto-derived from the response type via QuickCheck's `Arbitrary`. Stateless cases are free.
3. **Override path for stateful fakes.** When a state machine (payment lifecycle, order lifecycle) needs to be modelled, Nick overrides the default with hand-written code.
4. **Same binary, two modes.** Test mode is a CLI flag. The binary that runs in production is the binary tests run.
5. **Production dispatch links no test framework.** A production build whose integrations all override `runFake` (or whose response types omit `deriving Arbitrary`) links zero QuickCheck symbols on the dispatch path.

### Out of Scope

The first draft of this ADR also proposed a fixture recorder, an entropy/secret scanner, an auto-generated contract-test harness, an inbound-symmetry typeclass, a property-test helper, and a redaction-aware debug log carrier. These were cut after review:

- **Contract testing, fixtures, recorder.** Premature. Real integration verification is done manually against the actual third-party today; we can add framework-level tooling once a concrete pain point appears.
- **Entropy scan, redaction rules.** Belongs in the per-integration library (`integration-stripe`, `integration-sendgrid`) where the response shape is known, not in the framework.
- **Inbound symmetry.** Inbound integrations stay on the [ADR-0008](0008-integration-pattern.md) worker pattern for now. A separate ADR can address inbound test-mode if it becomes a constraint.
- **Property helper / `simulate`.** Tests can call `runFake` directly; a 30-line wrapper does not justify an additional cabal sublibrary.

This ADR is intentionally narrow: a typeclass with `runFake` + `runReal`, and a CLI flag that picks one at startup.

### GitHub Issue

No tracking issue exists yet.

## Decision

### 1. The Real/Fake Parity Contract

Every `Integration` instance ships **both** `runReal` and `runFake`. The typeclass enforces it: omit either method and the instance does not compile. `runReal` is the production logic (HTTP call, retry, auth); `runFake` is the mock implementation tests run against.

`runFake` has a *constrained default* that falls back to `QuickCheck.generate arbitrary @Response`. `Arbitrary` is **not** a superclass constraint of `Integration` — the default is only required when Nick does not override `runFake`. Production builds whose integrations all override `runFake` (or whose response types omit `deriving Arbitrary`) link zero QuickCheck code into the dispatch path.

This supersedes the `ToAction` typeclass from [ADR-0008](0008-integration-pattern.md) and replaces the raw `runReal`-equivalent body that today lives inside `OutboundIntegration` handlers from [ADR-0049](0049-outboundintegration-typeclass-dispatch-generation.md).

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Two typeclass instances (`ToAction (Real Cfg)`, `ToAction (Fake Cfg)`) selected by a wrapper | Rejected | Forces Nick to write two instances even in the common zero-state case. Wrapper types pollute every call site. Cannot auto-derive the fake. |
| A single `Adapter req resp` record with `runReal`/`runFake` function fields | Rejected | Records cannot carry a default — Nick must always supply both. No hook for the framework to inject Arbitrary-based fallback. |
| `Arbitrary` as a **superclass constraint** on `Integration` | Rejected | Every production binary would link `QuickCheck` transitively via the class dictionary. Arbitrary is a test concern; the production dispatch path must not depend on it. |
| A typeclass with `runReal` as a method and `runFake` as a method with a **constrained default** (`default runFake :: (Arbitrary (Response request)) => ...`) | **Chosen** | Zero-ceremony default when Nick wants it. Override is trivial. The constraint lives on the *default method body*, not the class head, so production builds that override `runFake` link no QuickCheck code. |

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

**What Nick writes for the zero-state case:**

```haskell
data SendEmailConfig = SendEmailConfig
  { apiKey :: Redacted Text
  }
  deriving (Generic)

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
        Task.throw (ValidationFailure "unknown intent")
```

The state store (`PaymentFakeState`) is the integration author's concern — whatever `IORef`, `TVar`, or pure-structure approach fits. The framework does not prescribe a state-machine DSL.

### 2. Selection via `Application.run` — CLI Flag

Selection happens in the default CLI surface of `Application.run`. No Haskell source change is required to flip from real to fake. The framework adds three flags to every `Application.run` binary:

| Flag | Meaning |
|------|---------|
| *(no flag)* | Default. All integrations call `runReal`. |
| `--integrations=real` | Explicit form of the default. All integrations call `runReal`. |
| `--integrations=fake` | All integrations call `runFake`. |
| `--integrations=hybrid --fake=Sendgrid --fake=Stripe` | Named integrations call `runFake`, the rest call `runReal`. Useful for staging or for tests that want most integrations live but one stubbed. |

**Why a CLI flag is the security mechanism.** The flag is the *only* switch — there is no environment-variable gate, no compile-time split, no separate test builder. The CLI flag is what makes this safe:

- **Visible.** `--integrations=fake` shows up in `ps`, in container orchestration UIs, in CI logs, in audit pipelines. An operator or a security review can grep argv for it; an env var with the same effect would be invisible without inspecting `/proc/<pid>/environ`.
- **Deliberate.** A CLI flag is set by the launcher (a test runner, a `docker run` command, a developer's shell) at the moment of invocation. It does not get inherited from a parent process the way an env var does, and it does not survive a `cp` of a deployment manifest the way an env var does.
- **Single source of truth.** One mechanism to audit, one mechanism to revoke. Two-mechanism gates encourage the "I'll set the env var globally and forget" anti-pattern that defeats the gate.
- **Production deploys simply do not pass the flag.** Real deployments invoke the binary with no `--integrations` flag (or with `--integrations=real`); the default is the safe choice. A deploy-check asserts on argv.

On startup, if fake mode is active, `Application.run` logs one line at `ERROR` level (visible in all log aggregations without opt-in) naming the integrations currently in fake mode, and the `/health` endpoint exposes `integrations.mode=fake|hybrid|real` and `integrations.fakes=[...]`. This lets a deploy-check assert that no production instance is ever in fake mode, even if argv inspection is unavailable.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Plain environment variable (`NEOHASKELL_INTEGRATIONS=fake`) | Rejected | Invisible in process listings; inherited by child processes; easy to leak via copied deployment configs and dotenv files. |
| Per-integration wiring in `Application` builder (`Application.withFake @Sendgrid`) | Rejected | Forces test suites and test binaries to diverge from production binaries at compile time. Kills the "same binary, different flag" promise. |
| Separate `Application.test` builder | Rejected | Duplicates `Application` surface area. Every new `with*` helper would need a test-mode twin. |
| CLI flag + environment-variable gate (the prior draft of this ADR) | Rejected | Two-mechanism gates split the audit surface and encourage the anti-pattern of setting the env var globally. The CLI flag alone — visible, deliberate, per-invocation — is the simpler audit boundary. |
| CLI flag, no env gate | **Chosen** | Same binary, single visible audit surface. Production deploys pass `--integrations=real` (or nothing); test invocations pass `--integrations=fake`. Deploy-checks verify on argv and `/health`. |

**Wire-up-time dispatch, not per-call selection.** Selection is resolved **once, at startup** in `Application.run`, before the command/event loop begins accepting traffic. For each `Integration` instance the framework constructs a *pre-bound closure* — a function of type `request -> Task IntegrationError (Response request)` — that already routes to the chosen implementation, registered in a `TypeRep`-keyed map:

```haskell
-- Wire-up (runs once, at startup, inside Application.run)
mkDispatcher ::
  forall request.
  (Integration request) =>
  Selection ->
  (request -> Task IntegrationError (Response request))
mkDispatcher selection = case selection of
  Real -> runReal
  Fake -> runFake
  Hybrid fakes
    | isFake @request fakes -> runFake
    | otherwise -> runReal
{-# INLINE mkDispatcher #-}
```

At request time, outbound dispatch looks up the pre-bound closure in the `DispatchRegistry` and calls it directly. There is no per-call `case` over `Selection`, no per-call env-var read. The `emit` shim is `{-# INLINE #-}` so the closure call inlines into the call site after specialisation. This keeps the production hot path at one direct function call per emit, preserving the 50k req/s budget.

### 3. Module Placement

All modules live in production (linked into every NeoHaskell application binary):

```text
core/service/Service/Integration/
  Adapter.hs            -- Integration typeclass (no Arbitrary superclass)
  IntegrationError.hs   -- Public-safe error variants
  Selection.hs          -- CLI flag parsing (real / fake / hybrid)
  DispatchRegistry.hs   -- TypeRep-keyed pre-bound closure registry
  ShimEmit.hs           -- emit hot-path shim (registry lookup + call)
```

There is no `nhcore:testing` sublibrary in this ADR. Tests call `runFake` directly when they want fake-mode behaviour. The CLI selection (§2) is the production-side mechanism; per-test mocking happens via plain function calls.

### 4. Supersession of Prior ADRs

- [ADR-0008](0008-integration-pattern.md): the `ToAction` typeclass, `Integration.outbound`, and the imperative `toAction` body are replaced by the `Integration` typeclass of this ADR. The per-entity dispatch model and `Integration.batch` / `Integration.none` API remain untouched. **The inbound section of ADR-0008 is unaffected** — inbound integrations continue to use the existing worker pattern; this ADR is outbound-only.
- [ADR-0049](0049-outboundintegration-typeclass-dispatch-generation.md): the `OutboundIntegration` typeclass and the `outboundIntegration` TH macro remain. The change is to the adapter layer below: handlers now emit typed `Integration request` values instead of opaque `Action` values.

Backwards compatibility is not a priority. The testbed and existing integration packages are updated in the same change set as the framework code.

### 5. Public API Surface

```haskell
module Service.Integration.Adapter (Integration (..)) where

class Integration request where
  type Response request :: Type
  runReal :: request -> Task IntegrationError (Response request)
  runFake :: request -> Task IntegrationError (Response request)
  default runFake ::
    (QuickCheck.Arbitrary (Response request)) =>
    request ->
    Task IntegrationError (Response request)


module Service.Integration.IntegrationError (IntegrationError (..)) where

data IntegrationError
  = TransportFailure Text     -- e.g. "connection reset"
  | AuthenticationFailure     -- no payload; the cause is secret by nature
  | PermanentFailure Text     -- e.g. "sendgrid rejected: invalid email syntax"
  | TransientFailure Text     -- e.g. "sendgrid returned 503"
  | ValidationFailure Text    -- e.g. "templateId missing"
  deriving (Eq, Show)


module Service.Integration.Selection (Selection (..), parseSelection) where

data Selection = Real | Fake | Hybrid (Array Text)

parseSelection :: Task Text Selection
```

The `DispatchRegistry` and `ShimEmit` modules are framework-internal: application authors call `Integration.emit` (which delegates to the shim), not the registry or shim directly.

Production code imports `Integration` (top-level) or `Core` and gets the typeclass surface plus `emit`.

### 6. Error Discipline

`IntegrationError` payloads are **public-safe text only**. No request bodies, no headers, no URLs containing tokens, no environment-variable values, no rendered response bodies. The contract is enforced by reviewers: any instance that passes a request body, response body, or secret into one of the `Text` payloads is rejected.

`AuthenticationFailure` carries no payload deliberately — there is no safe free-form text for "your API key was rejected" that cannot also contain the rejected key or a portion of it.

Sensitive diagnostic context is the integration author's concern. The framework provides no debug log carrier in this ADR — per-integration libraries can log at `Debug` level using `Redacted` types from existing infrastructure ([Redacted.hs:46](../../core/core/Redacted.hs)).

## Consequences

### Positive

- **Fake-by-default is free.** Any integration whose response type derives `Generic` and `Arbitrary` (routine for plain records) gets a usable fake with zero extra code.
- **Production dispatch links no test framework.** `Arbitrary` is a constrained-default requirement, not a superclass constraint. A production build whose integrations all override `runFake` (or whose response types omit `deriving Arbitrary`) links zero QuickCheck code into the outbound dispatch path.
- **One binary, two modes.** Test mode is a CLI flag, not a compile-time split. The binary that runs in production is the binary Jess tests with.
- **Visible audit surface.** `--integrations=fake` shows up in `ps`, in container UIs, in CI logs. A deploy-check that asserts on argv catches every misconfiguration without needing to inspect process environments.
- **Small surface.** Five modules, one typeclass, one CLI flag. Easy to read end-to-end; easy to delete if the design proves wrong.

### Negative

- **Response types wanting the default fake must have `Arbitrary`.** For custom records this is a one-liner via `Generic`, but exotic types (opaque identifiers, ADTs with business invariants) require Nick to hand-write an `Arbitrary` instance — or override `runFake`.
- **No drift detection.** When the real API response shape changes, fake-based tests pass while production breaks. This was the gap the original draft tried to close with contract testing; cutting it leaves the gap open. Mitigation: integration authors are expected to verify against the real third-party manually before each release. This is the deliberate trade-off of the smaller scope.
- **No fixture story.** Tests cannot exercise specific real-world response shapes without overriding `runFake` per scenario. For most stateless integrations this is fine; for state-machine integrations the override pattern is the only path.
- **Breaks ADR-0008 and ADR-0049's adapter layer.** Every integration package and testbed example that today defines `ToAction` or calls `Integration.outbound` on a config record must be rewritten. Scope is manageable (few dozen call sites) but not trivial.
- **Stateful fakes have no framework scaffolding.** Different stateful integrations will diverge in how they model state. Deliberate — a framework state-machine DSL is premature.

### Risks

1. **Arbitrary-generated data can violate integration invariants.** Generated `SendEmail.to` might not be a valid email; the real API would reject it but `runFake` happily returns a generated response. Mitigation: `runFake` is a stub, not a simulator — tests asserting real-API validation behaviour must override `runFake`.
2. **A `--integrations=fake` flag could land in a production deployment manifest.** A Helm chart, systemd unit, or Kubernetes spec copied from staging might retain the flag. Mitigation: startup logs an `ERROR`-level line naming active fakes; `/health` exposes `integrations.mode`; deploy checks assert on argv (a single grep) and on `/health`. Removing the env-var gate accepts that argv-based audit is the line of defence; the gain is one mechanism to audit instead of two.
3. **Fakes silently drift from real APIs.** Without contract testing, a fake that returns a field the real API removed will keep passing tests. Mitigation is operational: integration authors maintain their own real-vs-fake parity discipline. If this becomes a recurring pain we add a follow-up ADR for contract testing.

### Mitigations

- **Arbitrary cost**: ship `Generic`-derived defaults in framework code, document the one-liner for custom types, provide `deriving Arbitrary` in the style guide.
- **Migration**: ADR-0008 and ADR-0049's adapter call sites are updated in the same PR series that lands this ADR. The testbed serves as the reference migration.
- **Stateful fake patterns**: a follow-up ADR may introduce a state-machine helper module once two or three stateful integrations have shipped and their shared shape is clear.

## Future Work

- **Inbound test-mode**: a follow-up ADR covering inbound symmetry (`InboundIntegration` typeclass, `simulate`).
- **Contract testing**: revisit if integration authors report drift causing production incidents.
- **Fixture recording / replay**: revisit if recording real sandbox traffic becomes a recurring need.
- **State-machine helper library** for stateful fakes, once usage patterns are established across multiple integrations.
- **Observability for fake-vs-real traffic** in hybrid mode.

## References

- [ADR-0008: Integration Pattern](0008-integration-pattern.md) — superseded outbound adapter layer (`ToAction`); inbound section unaffected.
- [ADR-0021: Declarative Config DSL](0021-declarative-config-dsl.md) — selection flag parsing integrates here.
- [ADR-0049: OutboundIntegration Typeclass with Typed Event Dispatch](0049-outboundintegration-typeclass-dispatch-generation.md) — outer dispatch layer whose handlers produce `Integration request` values.
- [core/service/Integration.hs](../../core/service/Integration.hs) — current top-level re-exports, updated by this ADR.
- [core/service/Service/OutboundIntegration/Core.hs](../../core/service/Service/OutboundIntegration/Core.hs) — `OutboundIntegration` typeclass (unchanged).
