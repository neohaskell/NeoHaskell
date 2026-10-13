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

No application API change. `scripts/run-all-tests.sh` must execute full `nhintegrations-test` and propagate failure, preserving the existing core/service/Hurl gates. Linux `test-integrations` must execute the whole suite, retain its detailed report, and participate in `ci-gate` needs/results. The macOS matrix must execute the same suite and trigger for integrations/workflow changes. No selected-video substitute, quarantine, xfail or skipped-green baseline.

Oura SyncAll tests retain a stored refresh token and resource Unauthorized failures. An owned bracketed HTTP fixture binds only127.0.0.1 on an ephemeral port, accepts POST /oauth/token and returns400 JSON invalid_grant. Both existing error examples must observe exactly one refresh POST; successful examples must observe zero. No provider validation or production OAuth behavior changes. Test-only WAI/Warp/http-types dependencies already exist in the pinned repository closure; no new flake input or package upgrade. Counter and form observations stay local and never log synthetic credential values. Fixture error parsing must be checked through the actual pinned OAuth client.

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Actual local runner fails when the integration suite fails | `script:scripts/test-integrations-gate-red#` | unit | none |
| C2 | Actual local runner executes exactly one whole integration suite and preserves service/Hurl execution | `script:scripts/test-integrations-gate#AllTestsRunner.test_whole_integrations_suite_is_executed` | unit | none |
| C3 | Linux gate requires the complete integration job and retains its execution report | `script:scripts/test-integrations-gate#IntegrationWorkflows.test_linux_suite_is_required` | unit | none |
| C4 | macOS matrix and change triggers include integrations | `script:scripts/test-integrations-gate#IntegrationWorkflows.test_macos_matrix_runs_whole_suite` | unit | none |
| C5 | Failed local refresh invokes the existing error callback | `hspec:nhintegrations-test:integrations/test/Integration/Oura/SyncAllSpec.hs#invokes onError callback when fetch fails and onError is Just` | integration | http:real |
| C6 | Failed local refresh preserves the existing AuthenticationError expectation | `hspec:nhintegrations-test:integrations/test/Integration/Oura/SyncAllSpec.hs#throws error when fetch fails and onError is Nothing` | integration | http:real |

C1 is a new genuine reproduction against unchanged main7bd77c runner: all commands are owned offline stubs, integrations returns37, and the actual runner incorrectly returns0. It does not execute Haskell tests or PostgreSQL. The earlier621-example/2-failure integration baseline is retained as historical evidence, not a new PR-bound receipt and not rerun against the external token URL. C5/C6 initially identify existing assertions; local HTTP execution and POST-count assertions land only after design review. No fabricated fixture failure is used as historical red.

Wiring negative controls must reject missing job/needs/result/report, wrong-suite substitution and narrowed selectors. Native full candidate nhintegrations-test execution is mandatory after fixture correction, separately from script wiring checks. Real CI at actual HEAD, full test-all with owned PG/Hurl, and testbed remain required before ready.

## User impact

No application migration. Contributors and CI will see integration failures that were previously omitted. Test-only loopback fixtures replace external network access. Required-check configuration remains maintainer-owned; this change does not claim to modify branch protection. No discretionary subprocess/audio API or policy changes are included.

## ADR

[ADR-0082: Own local OAuth refresh-failure test endpoints](../decisions/0082-local-oauth-test-endpoints.md) records the test-only dependency additions and fixture lifecycle.

## Localized plan

- Copy-adapt scripts/test-codemap-empty-changes for scripts/test-integrations-gate; real scripts/run-all-tests.sh execution under temporary offline command stubs.
- Copy-adapt scripts/workflow-check job_blocks, strip_comments and existing mutation patterns for wiring discrimination; register the new regressions in its self-test path.
- Copy-adapt core/test/Auth/OAuth2/TokenRefreshSpec.hs real local HTTP boundary, but replace its fixed-port/sleep lifecycle with Warp bracketed loopback ephemeral-port helper.
- Edit only .github/workflows/test.yml, .github/workflows/test-macos.yml, scripts/run-all-tests.sh, scripts/workflow-check, scripts/test-integrations-gate, integrations/test/Integration/Oura/SyncAllSpec.hs, integrations/nhintegrations.cabal and the spec/ADR/test-surface/release records.
- No writes to other worktrees, Core product code, host PostgreSQL or the stopped owned VM. Native cached macOS toolchain only; no new compiler downloads.
