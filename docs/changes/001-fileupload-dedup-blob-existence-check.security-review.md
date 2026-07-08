# Security design review: Verify the blob still exists before returning a dedup match on file upload

Spec: docs/changes/001-fileupload-dedup-blob-existence-check.md | Capabilities: file-upload (security-sensitive) | Date: 2026-07-08

## Design under review

Add a non-exported helper `ensureBlobPresent :: BlobStore -> BlobKey -> Bytes -> Task Text Unit`
called before every "return an existing `FileRef`" site in `handleUploadImpl` /
`normalUploadFlow`. It checks `blobStore.exists`; if the blob is absent — or its
presence cannot be confirmed (the check errored) — it re-stores the caller's
content under the **existing** blob key (`BlobStore.store` overwrites
idempotently) and returns the unchanged `FileRef`/`blobKey`. No state-store write,
no new key, no signature change. (Candidate B — fall through to a fresh upload —
was rejected: it mints a new key, hits the `(owner_hash, content_hash)` partial
unique index on Postgres, and the existing 23505 cleanup path deletes the fresh
blob and re-returns the stale ref.)

## Findings

| # | Checklist | Finding | Grounding | Verdict |
|---|-----------|---------|-----------|---------|
| 1 | S1 (Tampering) | Heal writes bytes under a blob key. Could an attacker write attacker-chosen bytes to a key? No — the key comes from an **owner-scoped** dedup match (`findByContentHash … WHERE owner_hash = $1 AND content_hash = $2`, `Postgres.hs:692`; in-memory requires `ownerHash ==`), and the bytes are **SHA-256-pinned** to the matched `contentHash` (`computeContentHash content` must equal the row's hash to reach the branch). Writing different bytes needs a second preimage. | kept | informational (no vector) |
| 2 | S1 (Info-disclosure) | Does the heal expose another owner's content? No — the re-stored bytes are the caller's own uploaded `content`; the match is owner-scoped. `ownerHash` is server-derived (JWT `claims.sub` or the literal `"anonymous"`, `Transport/Web.hs:843`), not client-controlled. | kept | informational (no vector) |
| 3 | S6 (Data access) | Candidate A performs **zero** SQL / state-store writes — it only reads `getState` and calls `BlobStore` ops. This strictly *reduces* the DB attack surface vs candidate B (which would exercise the insert + 23505 path). | kept | informational (security plus) |
| 4 | S8 (Error surface) | The heal's store-failure path must not leak internals (a `BlobStoreError` `StorageError` can carry a filesystem path). Design returns a **generic** client message (`"Failed to restore missing file content. Please retry."`) and logs the heal server-side (`Log.warn`), matching the existing generic-message idiom in `normalUploadFlow`. | kept | advisory (addressed in design) |
| 5 | S2 (Parse-don't-validate) | Helper takes `BlobKey` (already a validated newtype; `Local.validateBlobKey` rejects `..`/`/`/leading-`/` on every store) + `Bytes`. No raw-`Text` boundary introduced; `handleUploadImpl` signature unchanged. | kept | informational (no gap) |
| 6 | S3 / S4 / S5 / S7 | No secrets, no secret comparison, no new randomness, no new query/command/authorization surface, no new dependency (`new-dependency: false`). `blobKey` is an internal random UUID already omitted from JSON. | kept | informational (nothing found) |
| 7 | S8 / DoS | exists-*error* → treat-as-missing → re-store: under a degraded backend that errors on `exists`, every dedup hit re-writes the blob (≤ `maxFileSizeBytes`, default 10 MB). | demoted (failed Q1: bounded by upload rate × max size, and only while storage is already unhealthy; re-store is idempotent) | informational (future: a backend with expensive writes should distinguish `NotFound` from transient error) |
| 8 | S5 | Anonymous mode (`auth` disabled) gives all uploads `ownerHash = "anonymous"`, so dedup/heal is shared across anonymous clients. | demoted (failed Q2/Q4: pre-existing property of the dedup match itself, already flagged at `Transport/Web.hs:842`; not introduced by this change) | informational |

**Blockers:** 0 — proceed to implementation. No plan amendment required for security.

## Notes carried to implementation

- Keep the heal's client-facing error **generic**; log detail server-side only (finding 4).
- Apply `ensureBlobPresent` at **all four** existing-ref return sites (the two dedup
  branches *and* the two 23505 race-retry branches in `normalUploadFlow`), so no code
  path returns a dangling ref — the race-retry path is the same defect class as #713
  (surfaced by the design-alternative review lens).
- `handleDownloadImpl`'s `File blob is missing from storage` remains a detect-only
  backstop for any ref that still slips through; out of scope to change here.

## Provenance

A 5-lens adversarial design verification (postgres-constraint, concurrency-race,
security, test-validity, design-alternative) was run against the real source before
this record; all five returned **design-sound**. This review incorporates its
security lens (findings 1–3, 7–8) and its completeness finding (the race-retry path,
in Notes).
