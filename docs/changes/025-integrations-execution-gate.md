# Change 025: Execute the complete integrations suite in normal gates

Run the shipped integration tests in Linux CI, the macOS test matrix and the local all-tests command. Keep Oura refresh-failure tests offline by using an owned loopback token endpoint, without changing their existing callback and AuthenticationError assertions.

```yaml spec
issue: adhoc:integrations-execution-gate
kind: bug
touches: [ci-cd, dev-pipeline, outbound-integrations]
breaking: false
new-dependency: true
new-capability: false
new-extension-point: false
```

## Contract delta

```diff signatures
```

No application API change. `.agents/skills/neohaskell-test/scripts/run-full-suite --strict` executes `cabal test all`, checks every declared test component, and runs the maintained Hurl testbed. A failed or missing `nhintegrations-test` result must fail this command. Core and service suites remain included. Linux `test-integrations` must execute the whole suite, retain its detailed report, and participate in `ci-gate` needs/results. The macOS matrix must execute the same suite with detailed output and trigger for integrations/workflow changes. No selected-video substitute, quarantine, xfail or skipped-green baseline.

Oura SyncAll tests retain a stored refresh token and resource Unauthorized failures. An owned bracketed HTTP fixture binds only127.0.0.1 on an ephemeral port, accepts POST /oauth/token and returns400 JSON invalid_grant. Both existing error examples must observe exactly one refresh POST; successful examples must observe zero. No provider validation or production OAuth behavior changes. Test-only WAI/Warp/http-types dependencies already exist in the pinned repository closure; no new flake input or package upgrade. Counter and form observations stay local and never log synthetic credential values. Fixture error parsing must be checked through the actual pinned OAuth client.

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Actual local runner fails when the integration suite fails | `script:.agents/skills/neohaskell-test/tests/test_test_runtime.py#FullSuiteTests.test_integrations_failure_is_not_skipped` | unit | none |
| C2 | Actual local runner executes one whole-project Cabal invocation and preserves service/Hurl execution | `script:.agents/skills/neohaskell-test/tests/test_test_runtime.py#FullSuiteTests.test_whole_integrations_suite_is_executed` | unit | none |
| C3 | Linux gate requires the complete integration job and retains its execution report | `script:.agents/skills/neohaskell-test/tests/test_ci_verification.py#CiVerificationTests.test_linux_suite_is_required` | unit | none |
| C4 | macOS matrix and change triggers include integrations | `script:.agents/skills/neohaskell-test/tests/test_ci_verification.py#CiVerificationTests.test_macos_matrix_runs_whole_suite` | unit | none |
| C5 | Failed local refresh invokes the existing error callback | `hspec:nhintegrations-test:integrations/test/Integration/Oura/SyncAllSpec.hs#invokes onError callback when fetch fails and onError is Just` | integration | http:real |
| C6 | Failed local refresh preserves the existing AuthenticationError expectation | `hspec:nhintegrations-test:integrations/test/Integration/Oura/SyncAllSpec.hs#throws error when fetch fails and onError is Nothing` | integration | http:real |

C1 originally reproduced the omission against the unchanged main `7bd77c` runner. Owned offline stubs returned 37 for integrations, but `scripts/run-all-tests.sh` returned 0. That evidence and its former `scripts/test-integrations-gate-red` driver remain historical, not proof about the current runner. The earlier 621-example, 2-failure integration baseline also remains historical evidence. It is not a new PR-bound receipt and was not rerun against the external token URL. C5/C6 preserve the accepted local HTTP execution, callback assertions, AuthenticationError assertion, and exact POST counts. No fabricated fixture failure is used as historical red.

Current main retired `./dev`, the old runner, and the `criteria-runtime` report consumer. This change does not restore them. The Linux suite report remains a diagnostic artifact, not input to the retired concatenation gate. The existing `checks.yml` tooling job discovers the test skill's Python tests and propagates their failures.

Wiring negative controls must reject missing job/needs/result/report, wrong-suite substitution and narrowed selectors. Native full candidate `nhintegrations-test` execution is mandatory, separately from script wiring checks. Real CI at actual HEAD, `run-full-suite --strict` with owned PostgreSQL and Hurl, and testbed execution remain required before ready. OAuth failures remain unresolved while PR917 API approval is pending. No Auth or HTTP behavior change is authorized by this tooling migration.

## User impact

No application migration. Contributors and CI will see integration failures that were previously omitted. Test-only loopback fixtures replace external network access. Required-check configuration remains maintainer-owned; this change does not claim to modify branch protection. No discretionary subprocess/audio API or policy changes are included.

## ADR

[ADR-0082: Own local OAuth refresh-failure test endpoints](../decisions/0082-local-oauth-test-endpoints.md) records the test-only dependency additions and fixture lifecycle.

## Current verification ownership

- Extend `.agents/skills/neohaskell-test/tests/test_test_runtime.py` with actual `run-full-suite` execution under temporary offline command fixtures. Keep the existing full-suite runner unchanged.
- Extend `.agents/skills/neohaskell-test/tests/test_ci_verification.py` with Linux and macOS wiring controls and actual aggregate-shell failure tests. Verify the existing `checks.yml` discovery step executes both test files and rejects either failure.
- Preserve the bracketed loopback fixture and stored refresh token in `integrations/test/Integration/Oura/SyncAllSpec.hs`. Preserve its test-only dependencies in `integrations/nhintegrations.cabal`.
- Do not write other worktrees, Core product code, host PostgreSQL, or the Linux guest. Native suites belong to the coordinating agent's separate verification run.
