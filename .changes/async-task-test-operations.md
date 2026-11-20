---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

Two `AsyncTask` operations help you test cleanup code. `AsyncTask.injectRuntimeFailure` raises a failure with your message inside a running task, so a test can check what the task cleans up when it fails from outside. `AsyncTask.threadsLeftRunningBy` runs a task and lists the threads it started that are still running, so a test can prove an operation leaves no worker behind. Both report a momentary snapshot and are meant for tests. Existing apps need no changes.
