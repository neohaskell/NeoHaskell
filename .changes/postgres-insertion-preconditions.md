---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

The PostgreSQL, Simple and in-memory event stores now agree on when a write
is refused. Before this fix, some writes slipped through and could damage an
entity's history:

- On PostgreSQL, a write that says "add after event 5" when the stream only
  has events up to 3 was saved at position 6. That left a gap in the stream
  that could never be filled.
- On PostgreSQL, a write that expects the entity to exist was saved even when
  the entity had no events yet, which created it by accident.
- On every store, a write that says "add after event -1" or another negative
  position was saved.

These writes now fail with `ConsistencyCheckFailed`, and nothing is saved.
A write that says "add after event N" now succeeds only when N is the last
event in the stream, on every store. Commands that use `acceptExisting` or
`acceptAfter` get the usual conflict handling instead of a silently wrong
history. On PostgreSQL, sending the exact same events again after a
successful write still succeeds without saving them twice.

You do not need to change your app. Streams that already have gaps are not
repaired.
