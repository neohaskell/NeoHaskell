### Perf issue

**You own the measurement story. Plan, review, verify the numbers.** Tie every fix to a measurement, don't read source instead of measuring.

1. Capture a baseline diagnostic trace via the matching control skill or named runtime/compiler driver. Measure the affected framework flow through its promised completion boundary, demanding pure results and executing Tasks/decisions where relevant; use representative consumer builds for compilation or Template Haskell costs. Keep diagnostic/profiling runs separate from reported timing samples. Vet the baseline, and each later number, with **benchmark-checklist**, matching compiler/dependencies, build/RTS settings, cache state, workload and successful-work/error counts.
Use [Framework benchmark](../../skills/framework-benchmark/SKILL.md) for repeatable baseline and treatment sampling when its protocol fits the selected workload.

2. `how` to ground hypotheses. Don't claim a perf ceiling without running it first.
   Try the performance mantras in order, cheapest first:
   1. Don't do it. Stop work whose result nothing uses rather than cheapening it.
   2. Do it, but don't do it again.
   3. Do it less.
   4. Do it later.
   5. Do it when they're not looking.
   6. Do it concurrently.
   7. Do it cheaper.

   When an earlier mantra meets the target, stop.
3. Plan the fix from the trace. Use `architect` first for a meaningful change to a contract, representation, module/service boundary, state owner, concurrency or generation strategy. Delegate implementation through available roles in `AGENTS.md`, within actual capacity. Review the diff. Capture a post-fix trace.
   Apply the **sequence-verifiable-units** principle skill, verifying each attempt before trying the next.
4. Parse and compare the artifacts in SQLite. Use supported readers for actual JSON, GHC profiles/eventlogs or compiler-phase reports; retain units, relationships and references to original records rather than inventing samples from aggregates. Keep raw captures, derived tables, queries and results under repository-root `.agents-state/perf-issue/<run>/`. "Inconclusive" or wrong-surface is not a pass. Flag it.
5. Cite the measurement in the PR.
6. Run **Opening a PR**.

Apply [Design for Throughput](../../skills/principle-design-for-throughput/SKILL.md) and [Security by Construction](../../skills/principle-security-by-construction/SKILL.md): preserve authority, ordering, durability, cancellation, observable outcomes and the same completion boundary while optimizing. The hypothetical throughput design target adds no full-target load-test gate; empirical claims still need appropriate measurements.

For sustained improvement against a metric rather than a one-off fix, use the Hillclimb playbook (`.agents/workflow/playbooks/hillclimb.md`).

**Reply:** baseline number, post-fix number, delta, artifact path.
