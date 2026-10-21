### Trace forensics

**You own the diagnosis from the artifact.**

Distinct from **Runtime forensics**, which instruments the live process. Here the capture already exists. The artifact is a fixed dataset, read it, don't re-run it. Keep tooling generic: a DevTools or trace parser for cpuprofile and `.json.gz`, a text editor for a spindump, your heap tooling for a heapsnapshot. Preserve the capture and put derived tables, queries and findings under repository-root `.agents-state/trace-forensics/<run>/`.

1. Identify the format and load it with the right tool. Include GHC profiles, eventlogs and compiler timing/splice captures; distinguish compiler/Template Haskell work from application runtime. Parse large artifacts in a subagent (the **principle-guard-the-context-window** skill) and keep the reduced finding in the main thread. Use the available roles and delegation facilities in `AGENTS.md`.
2. Transform the raw artifact into a form you can query. Dump the artifact into SQLite, using rows for its actual samples, frames, nodes, events, aggregates or compiler phases; preserve units, relationships and references to original records. Reach the queryable shape before you read.
3. Narrow to the cause. Query for the frames that hold the most time and walk the call tree to the hot path. For a leak, follow the retainer chain to a GC root only when the captured heap graph supports it; aggregates and retainer sets alone do not provide complete paths. For a spindump, find the thread stuck on-CPU or blocked and its wait reason. For GHC, distinguish CPU time, allocation and live residency; interpret only recorded eventlog data, and do not treat generated splice text as generation timing.
4. Attribute to source. Map the hot frame or cost centre to file, symbol and line using metadata matching the captured revision/build, accounting for inlining and lazy evaluation. Resolve missing symbols, or state the mapping limit without inventing source locations.
5. If you have a paired capture, check comparable workload, build/RTS settings and capture windows before diffing it. Report observed differences separately from causal interpretation: pairing alone does not confirm the cause. Mark an unestablished mechanism as the strongest hypothesis the artifact supports, while retaining directly observed facts.
6. Hand back a cited diagnosis, no fix unless asked. Route to Bug fix or Perf issue once the cause is known. Throughput checkpoint stays one line: `throughput checkpoint: n/a, read-only forensics`.

**Reply:** the artifact and format, the reduced finding, the source location, the artifact paths, and what a paired comparison supports or leaves uncertain.
