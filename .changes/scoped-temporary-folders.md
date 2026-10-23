---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

`Directory.withTemporaryFolder` gives a task a private, unique folder under the system temporary directory. It attempts to remove the folder when the task finishes, fails, or is cancelled. Use it for files that should last only as long as one operation. Existing apps need no changes.
