# Change 016: Reuse compiled Haskell components across CI runners

Build the existing Cabal components through pinned haskell.nix, transfer their
runtime closures, and execute the tests on consuming runners. Evaluate one
library extraction only after the component foundation is verified. The detailed
plan, measurement protocol and progress are maintained in PR #899 under the
requester's explicit direct-execution exception; this contract file satisfies
existing CI validation and does not initialize skill or pipeline orchestration.

```yaml spec
issue: adhoc:cached-nix-components
kind: refactor
touches: [ci-cd, dev-pipeline, testbed]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

No public Haskell API change. Existing component outputs remain available.
`ci-components` adds a reference bundle with minimal runtime tools. Consumers
fetch a producer's exact closure without evaluating a flake or compiling. Failed,
empty or absent test execution fails; the aggregate CI gate propagates failures.
The full Cabal workspace, developer shell, independent Rust output, doctest and
Cabal codemap generation remain available. Testbed scripts accept an explicit
`NHTESTBED_BINARY` and fail if it is unusable, preserving their default Cabal path.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Exact revision binding, transfer policy, real disposable executable failures and empty summaries fail closed | `script:scripts/tests/test_nix_components.py#Components` | unit | none |

Full real-store, real-PostgreSQL/Hurl, hosted Linux/macOS and seven-case extraction
acceptance evidence is tracked explicitly in PR #899; these unit fixtures alone
are not a substitute. No extraction is claimed by this initial foundation.

## User impact

CI can reuse compiled project outputs while tests still run against their real
fixtures. Ordinary consumers and local Cabal development need no migration.

## ADR

Not required for the initial foundation: existing haskell.nix/Cachix mechanism,
no new dependency, public API, capability or extension point. Record an extraction
decision if the measured pilot justifies a new package boundary.
