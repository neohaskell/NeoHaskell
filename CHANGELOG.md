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

### 004-crypto-hmac-sign-verify — Change 004: Add Crypto module with HMAC-SHA256 signWith/verifyWith

None breaking. New public module `Crypto` and new `Bytes.getRandom`
primitive (secure random bytes, mirroring `Int.getRandom`); no existing
signatures change. The `Bytes` newtype now lives in the hidden internal
module `Bytes.Internal` purely to break an import cycle; it is not
importable by applications. The public `Bytes` API is unchanged (`Bytes
(..)` is re-exported as before). `Auth.OAuth2.StateToken` keeps its private
`HmacKey` for now — migrating it onto `Crypto.HmacKey` is a possible
follow-up refactor, deliberately out of scope here. Signature wire format
is lowercase hex (the common webhook header convention, e.g. GitHub/Stripe
style); `verifyWith` is case-insensitive on input.

API delta:

- `+ Crypto: data HmacKey`
- `+ Crypto: hmacKeyFromText :: Text -> Result Text HmacKey`
- `+ Crypto: hmacKeyFromBytes :: Bytes -> Result Text HmacKey`
- `+ Crypto: generateHmacKey :: Task err HmacKey`
- `+ Crypto: signWith :: HmacKey -> Bytes -> Text`
- `+ Crypto: verifyWith :: HmacKey -> Text -> Bytes -> Bool`
- `+ Bytes: getRandom :: Int -> Task w Bytes`

### 003-maintainer-codemap-regeneration — Change 003: Maintainer-triggered codemap regeneration onto a contributor PR

Not breaking. No public signature or wire-format change. New capability for
maintainers only: a manually-dispatched workflow on `main`. Contributors see
their PR branch receive one `chore: regenerate codemap` fast-forward commit after
Nick approves the protected environment; a no-op (codemap already current) leaves
the branch untouched and the run succeeds. Every unsupported or unsafe condition
(maintainer edits disabled, org-owned fork, metadata race, symlink under
`codemap/`, out-of-allowlist manifest/diff, non-fast-forward) fails with an
actionable Actions summary and mutates nothing — **no fallback PR is ever
created**. Testbed: no acceptance-test change — this is CI/tooling with no
HTTP-observable behavior. One-time maintainer setup is **mandatory and
load-bearing**: the `codemap-publish` Environment with **required reviewer Nick**
AND **deployment branches = `main` only**, plus the `CODEMAP_PUBLISH_TOKEN` secret
— a maintainer classic `public_repo` PAT (broad public-repo blast radius
documented; dedicated bot identity recommended; expiry ≤90d; revoke-on-exposure)
— documented in ADR-0070 and the workflow header. Without any of these the
workflow fails closed at `publish`.

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
