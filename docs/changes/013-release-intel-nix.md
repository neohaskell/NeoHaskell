# Change 013: Install Nix on Intel macOS release runners

Finish the approved native-runner Nix correction so 0.10.0 can build on all
four supported targets. The retry of #877 exposed that Determinate Nix no
longer ships its installer for Intel macOS.

```yaml spec
issue: adhoc:release-intel-nix
kind: bug
touches: [ci-cd, dev-pipeline, governance-docs]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

Use the commit-pinned Cachix Nix installer v31.11.1 only for fresh Intel macOS
native builds. Keep the existing Determinate installer on the other three
targets. Both setup steps retain the asset-reuse guard and run before native
tests. Preserve all four runners, existing test expectations and publication
gates. Change no product source, release revision, version, notes or ledger.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Fresh Intel macOS builds select the supported pinned installer; other targets retain Determinate; reused builds skip both | `script:scripts/releases/test_release.py#workflow` | unit | none |
| C2 | A later runner correction retries the same frozen release revision and ledger | `script:scripts/releases/test_release.py#bootstrap` | unit | none |

## User impact

This is runner provisioning only. Applications need no migration; the release
remains 0.10.0 from the already prepared revision. The fix does not retire Intel
Mac support or bypass any tests.

## ADR

Not required — no package build dependency, flake input or public contract
changes. The additional action is CI runner provisioning. Its pinned upstream
test matrix includes `macos-15-intel` / `x86_64-darwin`.

Sources: [Determinate support notice](https://github.com/DeterminateSystems/nix-src/issues/224),
[Cachix pinned runner matrix](https://github.com/cachix/install-nix-action/blob/13d8dd58da0234aa297dedd986986ccb8e7f3e24/.github/workflows/test.yml).
