---
group: platform
component: Framework
impact: none
category: Fixed
---

## Summary

Correct the release note for the two `AsyncTask` cleanup test helpers. Only
`threadsLeftRunningBy` reports a snapshot, which can include threads started by
unrelated concurrent work. `injectRuntimeFailure` returns after exception delivery
without waiting for cleanup to finish. This documentation correction changes no
application behavior or API and requires no application changes.
