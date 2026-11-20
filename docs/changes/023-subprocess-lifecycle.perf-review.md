# Performance design review: Preserve subprocess output and owned lifecycle
Spec: docs/changes/023-subprocess-lifecycle.md | Capabilities: system | Date: 2026-10-13

| # | Checklist | Finding | Grounding | Verdict |
|---|-----------|---------|-----------|---------|
| 1 | P1,P2 | External process execution is not command/event persistence hot-path work; no pragma/specialization changes. | failed Q1 hot-path budget relevance | informational |
| 2 | P3 | Imported lazy String from hGetContents is not forced by global Strict. Strict hGetContents' must finish in each reader thread; scope async tasks across exceptions. | kept: reproducing pipe-capacity deadlock | advisory: implementation requirement |
| 3 | P5,P6 | Two concurrent drains remove circular pipe waits; full captured output necessarily retains O(output) memory. No new cap or global lock. | kept: actual large output paths | advisory/residual |
| 4 | P7 | Existing real pipe fixtures prove termination/full-output behavior, not throughput speedup. No benchmark improvement claim. | kept | informational |

**Blockers:** 0 — preserve full output and validate lifecycle regressions; no throughput claim. One-second TERM grace is cleanup policy, not an extraction-timeout default change.
