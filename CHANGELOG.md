# Changelog

Entries are **generated from contract-delta specs** (`docs/changes/*.md`) by
`./dev changelog` — do not hand-write them; regenerate instead. A change is
**breaking** iff its spec's `diff signatures` delta removes or changes a
signature line; a breaking entry carries a mandatory migration note (from the
spec's `## User impact`). CI gate: `changelog --check` in `.github/workflows/checks.yml`.

**Release promotion:** at release time, rename the `## [Unreleased]` heading to
`## [X.Y.Z] — YYYY-MM-DD` and add a fresh empty `## [Unreleased]` above it; a
breaking entry in the section forces a major/minor bump per semver. (No release
has been cut yet — everything accrues under Unreleased until the first tag.)

## [Unreleased]

### 002-task-control-flow-dialect-rules — Change 002: Enforce Task control-flow dialect — `|> discard`, `Task.when`, `Task.unless`

Not breaking. No public signature or wire-format change — the migrated `if …
pass` blocks and their `Task.when`/`Task.unless` replacements are behaviourally
identical (`Task.when c a` runs `a` iff `c`, `Task.unless c a` runs `a` iff not
`c`, each otherwise doing nothing — exactly like the `if`/`pass` forms). New Task
code is nudged toward the dialect idioms at edit
time (rule 1) and at `./dev lint`/CI (rules 2–3). Existing non-dialect parser
and `Q`-monad code is deliberately preserved via added-lines grandfathering and
a scoped ignore. Testbed: no acceptance-test change — this is a source-dialect
and tooling change with no HTTP-observable behaviour.

### 001-fileupload-dedup-blob-existence-check — Change 001: Verify the blob still exists before returning a dedup match on file upload

Not breaking. No signature or wire-format change; `UploadResponse` still omits
`blobKey` from JSON. Behavior only changes on the failure path: an upload that
previously returned a reference to a missing blob (poisoning the content hash
forever) now re-stores the content and returns a valid reference. The re-stored
bytes are the caller's own uploaded content, matched by the same owner-scoped
content hash, so there is no cross-owner exposure. Testbed: no acceptance-test
change — blob loss cannot be induced over HTTP; covered at the integration
level.

Side effect of making the reproduction executable: `ContentDedupSpec` is listed
in the cabal `other-modules` but was never registered in
`core/test-service/Main.hs`, so its dedup coverage compiled but never ran. This
change registers it, so the regression tests **and** the existing dedup suite
now execute.
