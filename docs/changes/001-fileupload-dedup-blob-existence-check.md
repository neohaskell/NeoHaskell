# Change 001: Verify the blob still exists before returning a dedup match on file upload

File-upload deduplication currently returns an existing `FileRef` on an
owner+content-hash match **without checking that the blob is still present** in
the `BlobStore`. If the blob is ever lost (backend switch, ephemeral storage
dir, retention/GC, manual deletion), every subsequent upload of the same
content keeps short-circuiting to a `FileRef` whose blob is gone, and every
downstream read fails permanently with `File blob is missing from storage`.
This change makes dedup self-healing: before returning a match, verify the blob
exists and, if it does not, re-store the content under the same blob key so the
returned reference is never dangling.

```yaml spec
issue: issue#713                # issue#NNN or adhoc:<slug>
kind: bug                       # feature | bug | refactor
touches: [file-upload]          # capability IDs from codemap/capabilities.yaml (closed list)
breaking: false                 # MUST be true if the contract delta has any `-` line
new-dependency: false           # any new build-depends / flake input
new-capability: false           # this change adds a row to codemap/capabilities.yaml
new-extension-point: false      # this change adds a row to codemap/extension-points.yaml
```

## Contract delta

Internal-only fix. The public API surface of `Service.FileUpload.Web` is
unchanged — `handleUploadImpl` keeps its signature; the self-heal lives in a
new non-exported helper. No `codemap/signatures/` line is added or removed, so
the promised diff is empty.

```diff signatures
```

## Criteria

`kind: bug` → C1 is the failing reproduction test, committed red in the draft
PR. The behavior crosses the filesystem boundary (a real `BlobStore.Local`
losing a blob), so the reproductions are `integration`.

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | Re-uploading content whose deduplicated **Pending** blob was lost returns a `FileRef` whose blob is present again (self-heal), not a dangling ref | `Service.FileUpload.ContentDedupSpec` "dedup self-heals a missing Pending blob on re-upload" | integration |
| C2 | Same self-heal when the dedup match is a **Confirmed** file | `Service.FileUpload.ContentDedupSpec` "dedup self-heals a missing Confirmed blob on re-upload" | integration |
| C3 | Self-heal preserves dedup identity — same `FileRef` and same `blobKey` as the original — and the re-stored blob retrieves the exact uploaded content | `Service.FileUpload.ContentDedupSpec` "dedup self-heal preserves FileRef/blobKey and restores exact content" | integration |
| C4 | When the existence check itself errors, dedup still heals (re-stores) rather than returning a possibly-dangling ref | `Service.FileUpload.ContentDedupSpec` "dedup self-heals when the existence check errors" | integration |
| C5 | Healthy path preserved — when the blob is present, a duplicate upload still returns the existing `FileRef`/`blobKey` unchanged | `Service.FileUpload.ContentDedupSpec` "duplicate upload returns same blobKey as original" | integration |

## User impact

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

## ADR

Not required — no trigger (breaking / new-dependency / new-capability /
new-extension-point all false). The design choice (re-store under the existing
blob key rather than falling through to a fresh upload, which would collide with
the owner+content-hash unique constraint and re-return the stale ref) is
recorded here and in the implementation comment, not as an ADR.
