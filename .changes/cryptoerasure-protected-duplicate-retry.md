---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

The PostgreSQL event store now rejects a retry that reuses a protected event's ID with a different payload, even when the retry omits the protected value. It also rejects the rest of that retry batch. Existing protected data and its erasure state stay intact. Applications need no code change for normal event writes.
