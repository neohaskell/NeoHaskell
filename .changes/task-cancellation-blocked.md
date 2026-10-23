---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

`Task.withCancellationBlocked` lets a cleanup task finish an ownership handoff before cancellation can interrupt it, including while it waits. Use it only when the wait and cleanup can finish. A stalled task in this operation cannot be cancelled. `AsyncTask.isWaitingForExceptionDelivery` exposes a momentary status snapshot for cancellation tests.
