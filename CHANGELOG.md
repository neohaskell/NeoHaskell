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
