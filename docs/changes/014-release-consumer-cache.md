# Change 014: Use the project caches during release consumer checks

Complete the approved Nix runner setup correction for the generated-app release
check. Its first run ignored the project's binary cache configuration, exhausted
the fallback GitHub cache quota and timed out rebuilding dependencies.

```yaml spec
issue: adhoc:release-consumer-cache
kind: bug
touches: [ci-cd, dev-pipeline, governance-docs]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

Configure the release consumer's existing Nix installer with
`accept-flake-config = true`, matching the existing Neo CI consumer job.
This permits the frozen project's declared caches and signing keys. Keep this
setup inside the main-only consumer job, after its frozen-revision checkout,
and retain the asset-reuse guard. Add no credentials, cache-write permissions,
new cache hosts or keys. Preserve native builds, tests, timeouts and publication
gates. Change no product source, version, release notes, revision or ledger.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | The main-only consumer job accepts the frozen project's Nix configuration before building, under the existing reuse guard, matching the Neo CI consumer setup | `script:scripts/releases/test_release.py#workflow` | unit | none |
| C2 | A later runner configuration commit retries the original frozen release revision and unchanged ledger | `script:scripts/releases/test_release.py#bootstrap` | unit | none |

## User impact

Release validation can use the project's existing binary caches. Applications
need no migration; 0.10.0 retains its prepared source and contents. No new account
or paid API key is required. All existing release tests remain mandatory.

## ADR

Not required — restores the existing consumer CI's Nix configuration in the
release runner. No package dependency, flake input or public API change.
