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

The promised public Application wiring remains release-blocking pending a separate
config-aware API decision. Current `Application.withFileUpload` does not accept a
BlobStore and still selects Local; do not use the previously documented snippet.
This repair does not remove the Application promise or change Local behavior.

To verify locally, run `.agents/skills/tdd/scripts/test-match "BlobStore.S3" nhcore-test-service`.
The selection checks signing against the published AWS examples and exercises
the backend against an in-process fake S3 server. A passing run does not resolve
the Application release blocker.
