# Validation evidence

The source base is `deacffb43babc44cc12855f0897d7259901b3c33`, the merged revision of [PR #934](https://github.com/neohaskell/NeoHaskell/pull/934). The local source and tool checks ran on macOS. Production Haskell was unchanged. These checks establish lint behavior and documentation integrity, not runtime cancellation or storage behavior.

## Execute the real lint contract

```sh
HLINT=/path/to/hlint-3.10/hlint python3 .agents/skills/neohaskell-code-guidelines/tests/hlint_contract.py -v
python3 .agents/skills/neohaskell-code-guidelines/scripts/check.py --hlint /path/to/hlint-3.10/hlint
```

[The fixture transcript](validation/hlint-contract.txt) records 18 passing tests. The tests execute HLint and the gate CLI. They cover qualified and unqualified raw calls, new calls under existing imports, duplicate counts, modified uses, stale debt, safe pruning, line shifts, report isolation, thread inspection, error translation in prefix and pipeline forms, legitimate recovery, ordinary suggestions, parse failures, suppression behavior, and Haskell and Python workspace references.

The final gate reports 529 Haskell files and 726 source files checked. It retains 142 exact debt occurrences across 133 identities. Those occurrences comprise 24 raw imports, 117 raw operation uses, and one workspace reference. The [inventory](lint-debt.json) contains every identity and count. The [gate transcript](validation/gate.txt) records the command result.

Before the change, the pinned HLint 3.10 command `hlint core integrations testbed --json` returned `[]` with exit code 0. The available Nix HLint 3.8 returned 116 different hints. That difference is why the gate requires the CI version. The original suggestion-failure contract remains covered by an actual `Use head` rejection fixture.

A separate local check extracted HLint into a system temporary directory and ran the full gate through that executable. It passed. CI now installs under `runner.temp`, so HLint's bundled Haskell examples do not enter repository source discovery. This local check does not claim execution of GitHub's Linux runner.

## Execute ordinary owned-test discovery

An independent coordinating agent ran the exact owned Python discovery loop from [checks.yml](../../../../.github/workflows/checks.yml). [Its transcript](validation/python-discovery.txt) records 14 discovered suites and 146 passing tests with exit code 0. The explicit HLint suite is outside that dependency-free discovery pattern and runs in the lint job with its required executable. Both paths fail when their required checks fail.

This evidence covers the Python discovery step. It does not claim execution of the tooling job's separate Node, release-engine, or worktree-audit steps.

## Validate authored resources

```sh
python3 .agents/skills/validate-skills/scripts/check.py --repo .
python3 .agents/skills/neohaskell-release/scripts/semantic-release check --base origin/main
git diff --check
```

The [resource-check transcript](validation/resources.txt) records these commands and the native validators. The skill validator reported zero errors. The release fragment check passed with `impact: none`. The whitespace check passed. The available skill-creator native validator also passed for the naming, code-guidelines, best-practices, and no-comments skills. Its PyYAML dependency was installed in a local temporary virtual environment.

## Review the accepted findings

Two independent Astra High reviewers examined the diff. This follows the user's explicit model choice and provides no model diversity. Both found that generic Python discovery would run the HLint tests without installing HLint. The suite now has an explicit lint-job entrypoint. Reviewer A also demonstrated a legitimate non-Task suppression regression and missing pipeline forms in the new translation hint. Both now have acceptance or rejection fixtures.

The comment reviewer recommended deleting two duplicated migration-policy comments. Both were removed. No semantic directives or production comments changed. The coordinating review also found the repository-local binary installation problem and verified the old suggestion policy. Both changed the final implementation.

The independent trail audit had access to artifacts and source, but not a complete conversation transcript. Historical test outputs had been overwritten as fixtures grew. The trail therefore marks the earlier 16-test evidence as superseded by the retained final 18-test transcript. The earlier comment-deletion evidence pointer is also superseded by the reviewed configuration diff. This is a bounded audit, not a claim that every historical action was reconstructed.

A [fresh final verifier](validation/final-review.txt) independently reproduced the 18-test and full-gate results after the corrections. The verifier confirmed the current configuration. The exact two transient comment deletions remain historically unverified because their intermediate snapshot was not retained.
