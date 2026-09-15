# Change 012: Install native release-test prerequisites

Unblock the 0.10.0 native builds by installing Nix in each native runner before
Neo CLI tests run. The first publication attempt failed because its native
jobs install Rust but omit Nix, which an existing CLI prerequisite test needs.

```yaml spec
issue: adhoc:release-native-nix
kind: bug
touches: [ci-cd, dev-pipeline, governance-docs]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

Use the same pinned Nix setup action already used by the seed and consumer
jobs, before testing/building fresh native assets. Preserve the reuse guard,
four native targets, test expectations, publication gates and frozen revision.
This repairs the runner environment; it does not change the product revision,
release notes, version, date, ledger or reserved release attempt.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Every fresh native matrix job installs Nix before its test/build command, and reused assets skip setup | `script:scripts/releases/test_release.py#workflow` | unit | none |
| C2 | A later runner-setup commit retries the existing attempt using its original frozen release revision and unchanged ledger | `script:scripts/releases/test_release.py#bootstrap` | unit | none |

## User impact

The first 0.10.0 release can retry on correctly provisioned runners. Applications
and their migration instructions are unchanged. No tests are weakened or
skipped. An observed executable-fixture `Text file busy` failure remains a
separate possible transient issue to verify on retry.

## ADR

Not required — restores an existing build prerequisite in the native matrix.
