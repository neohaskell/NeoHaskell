---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

`Subprocess.runWithTimeout` now drains stdout and stderr together, so a child that writes more than a pipe can hold does not stall until its timeout. The function still returns complete output, including output larger than one MiB; there is no new capture limit or API change.

Timeouts and cancellation now close captured pipes and stop and reap the direct child, including a child that ignores TERM on macOS or Linux. Cleanup allows a one-second TERM grace before forced termination. This does not terminate descendants or provide a sandbox. No application changes are required. Existing callers can verify the fix by capturing large output and by checking that timed-out or cancelled direct children no longer remain running.
