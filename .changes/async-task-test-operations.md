---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

Two `AsyncTask` operations help you test cleanup code. `AsyncTask.injectRuntimeFailure` raises a failure with your message inside a running task. It returns after delivery, so a test can check cleanup after an outside failure. It does not wait for cleanup to finish.

`AsyncTask.threadsLeftRunningBy` runs a task and reports threads that started while it ran and are still running when checked. This is a momentary snapshot. It also includes threads started by unrelated concurrent work.

Both operations are meant for tests. Existing apps need no changes.
