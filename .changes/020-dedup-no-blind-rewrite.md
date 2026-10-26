---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

A duplicate file upload no longer rewrites the existing stored blob when the blob store's existence check returns an error. The upload instead fails with "Failed to verify stored file content. Please retry." Backend details are logged at error level via `Log.critical`. A check that reports the blob present still reuses it; a check that reports it missing still restores the uploaded content. This does not change how the Local backend reports existence-check results.

A concurrent upload that loses a state-store conflict may already have written its own provisional blob. It attempts to delete that provisional blob; it does not rewrite the winner's existing blob when the winner's existence check returns an error. This check does not prevent another actor from removing a blob later.

No code or data changes are needed in applications. To verify locally, run `./.agents/skills/tdd/scripts/test-match "ContentDedup" nhcore-test-service` and confirm `dedup fails closed when the existence check errors` passes.
