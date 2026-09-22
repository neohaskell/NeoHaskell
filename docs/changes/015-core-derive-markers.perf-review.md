# Performance design review: Core concept derivation helpers

Spec: docs/changes/015-core-derive-markers.md | Date: 2026-09-16

| # | Checklist | Finding | Grounding | Verdict |
|---|-----------|---------|-----------|---------|
| 1 | P1 | Helpers execute at compile time. Generated entity methods delegate to application functions as handwritten instances do. | No new runtime dispatch boundary. | informational |
| 2 | P2, P3, P5, P6 | No runtime algorithm, accumulator, lock or container changes. | Demoted: no exercised new hot path. | informational |
| 3 | P4 | Entity codecs retain Generic JSON defaults and preserve explicit codecs. | Same serialization mechanism as existing entities. | informational |
| 4 | P7 | No speed claim; no new runtime benchmark budget. | Compile-time API consistency is the goal. | informational |

**Blockers:** 0. No performance-related plan amendment.
