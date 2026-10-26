---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

The new S3 BlobStore primitive supports path-style signed object operations.
Malformed endpoint and header-bound configuration is rejected before requests.
Local wire tests verify signatures independently, including wrong-secret controls;
they do not certify deployments on every S3-compatible provider.

`Application.withFileUploadUsing` selects S3 through a config-aware backend
factory. Upload limits are validated before backend creation. Custom storage
ignores the Local directory and startup errors never fall back to disk.
Existing `Application.withFileUpload` callers keep Local storage unchanged.

To verify locally, run `.agents/skills/tdd/scripts/test-match "BlobStore.S3" nhcore-test-service`.
The selection checks signing against the published AWS examples and exercises
the backend against an in-process fake S3 server, including an upload/download
through a running Application. Startup selection and config loading are covered
by the additional focused commands in `docs/changes/017-s3-blob-store.md`.
