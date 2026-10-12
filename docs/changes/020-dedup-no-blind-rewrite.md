# Change 020: Stop dedup rewrites when the blob existence check returns an error

Change 001 made file-upload deduplication self-healing: when a duplicate
upload matches an existing `FileRef`, `ensureBlobPresent` checks that the blob
is still in the `BlobStore` and re-stores it if it is gone. One branch of that
helper goes too far: when the **existence check itself errors**, it logs a
warning, treats the blob as missing and re-stores it. On a remote object store
(S3, proposed in change 017), a failed existence check can therefore trigger
an unnecessary overwrite and transfer on each retry. The maintainer's review
of the S3 work asked to "adjust it so that it doesn't rewrite stuff". This
change keeps the self-heal for a *confirmed* missing blob and fails the request
when the existence check returns `Err`. This does not change the Local
backend's existence-test semantics: `Ok False` still selects self-healing,
including when that backend reports an inaccessible path as absent.

```yaml spec
issue: adhoc:dedup-no-blind-rewrite
kind: bug
touches: [file-upload]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

Internal-only fix inside the non-exported helper `ensureBlobPresent` in
`Service.FileUpload.Web`. No public signature changes.

```diff signatures
```

### Behavior

| `exists` result | Before (change 001) | After |
|---|---|---|
| `Ok True` | return existing ref | unchanged |
| `Ok False` | `Log.warn`, re-`store`, return ref | unchanged |
| `Err e` | `Log.warn`, treat as missing, re-`store` | `Log.critical` with the real error, **no store call**, request fails with the generic message `"Failed to verify stored file content. Please retry."` (same S8 convention as the existing re-store failure path: detail in the server log only) |

The 23505 concurrent-race retry branches in `normalUploadFlow` are untouched;
they call the same helper and inherit the new rule. That path still writes
(and then best-effort deletes) its own provisional blob before the retry
lookup reaches the helper; the guarantee here is only that the deduplicated
existing blob is not rewritten when its existence check returns `Err`.
A successful check describes presence at check time; it cannot guarantee that
another actor will not remove the blob before or after the upload returns.

### Scoped principles and regression checks

- Branch on the backend result, not an inferred filesystem condition: C1
  checks returned `Err`; C2 preserves `Ok False` healing for both file states;
  C3 preserves `Ok True` identity and zero stores.
- Keep client errors exact and generic: C1 checks the complete message and
  original bytes. Backend details stay in the server log at error level via
  `Log.critical`.
- Count writes by blob identity in the conflict path: C4 forces first lookup
  miss, an injected `23505` state update, retry lookup hit and winner `exists`
  error. It checks one provisional store and successful deletion of that same
  key, absence after deletion, winner exclusion, unchanged winner bytes and
  C1's exact error. This is real filesystem I/O with an injected conflict,
  not proof of a PostgreSQL race. Cleanup remains best-effort if deletion fails.

No Local error-classification change, locking redesign or permanent blob
presence guarantee is included.

## Criteria

`kind: bug`: C1 is the reproduction. It replaces the test
`dedup self-heals when the existence check errors` from change 001 (C4 there),
whose expectation encoded the behavior this change removes. **Rewording an
existing expectation requires maintainer approval** (AGENTS.md
non-negotiable; `scripts/guards/expectation-guard.py` flags it): the
maintainer must create `.agents/allow-expectation-edits` locally or apply the
`expectations-approved` label on the PR. Change 001's C4 row and the attested
match in `docs/changes/test-surfaces.json` are updated to the new test name so
`./dev spec-check` stays consistent.

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | When `exists` errors, the upload fails with exactly the generic message `"Failed to verify stored file content. Please retry."` (no backend error leaked), `store` is never called, and the original blob is untouched | `hspec:nhcore-test-service:core/test/Service/FileUpload/ContentDedupSpec.hs#dedup fails closed when the existence check errors` | integration | filesystem:real |
| C2 | Confirmed-missing blobs still self-heal (Pending and Confirmed matches) | `hspec:nhcore-test-service:core/test/Service/FileUpload/ContentDedupSpec.hs#dedup self-heals a missing Pending blob on re-upload`<br>`hspec:nhcore-test-service:core/test/Service/FileUpload/ContentDedupSpec.hs#dedup self-heals a missing Confirmed blob on re-upload` | integration | filesystem:real |
| C3 | Healthy path preserved: a present blob returns the existing `FileRef`/`blobKey` without a store call | `hspec:nhcore-test-service:core/test/Service/FileUpload/ContentDedupSpec.hs#duplicate upload of a present blob makes no store call` | integration | filesystem:real |
| C4 | Injected conflict retry fails with C1’s exact error after successfully deleting only its provisional blob; two lookups, one store/delete at the same non-winner key, winner bytes unchanged | `hspec:nhcore-test-service:core/test/Service/FileUpload/ContentDedupSpec.hs#dedup conflict retry cleans only its provisional blob when winner check errors` | integration | filesystem:real |

## User impact

None breaking. A duplicate upload whose blob-store existence check fails now
returns an error ("Failed to verify stored file content. Please retry.")
instead of silently rewriting the blob. The client retries; the operator sees
the real backend error at error level via `Log.critical` in the server log.
Uploads whose existence check reports present or missing behave exactly as
before. Local backend existence-test semantics are unchanged.

## ADR

Not required — no trigger (breaking / new-dependency / new-capability /
new-extension-point all false). The rule is the standard fail-closed one:
do not rewrite the existing key when its existence check returns `Err`.

### Continuation approval and historical red evidence

The first published change020 commit contains spec, test and implementation
together. That history does not show a red-only commit; it does not establish
that a private red receipt never existed. The maintainer must verify a genuine
receipt or explicitly decide how to accept a clearly labelled after-the-fact
reproduction. Such a reproduction is new evidence, not historical red-first
compliance, and must not rewrite published history to simulate it. The current
fixed head is not a new red reproduction. Existing `expectations-approved`
authorization is limited to the previously approved expectation change, not a
new Gate 1 continuation or final Gate 2. No additional behavior or expectation
changes are authorized by this proposal.
