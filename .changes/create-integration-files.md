---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

Integrations can save generated PDFs and other documents as owned files with `fileAccess.createFile`. Pass the owner captured by an authorized command, a stable document job key, and the document bytes. The returned `FileRef` works with the existing authenticated download route and has no temporary-upload expiry.

Retrying a completed job with the same owner, key, and metadata returns the same reference and preserves its first completed bytes. An unfinished job or a missing stored file requires the original bytes. Applications must explicitly reconcile abandoned jobs because file creation and the command that records its reference are separate operations.

Existing applications configured with `withFileUpload` keep their setup. To use the feature, create the file inside `Integration.action` and save its reference through a completion command. Verify that the owner can download the document and a retried job returns the same reference.
