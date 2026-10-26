---
name: benchmark-checklist
description: "Vet a perf measurement (limiter, tuning, limits, errors, repeatability, relevance, and whether the work happened) before you report or act on it. Use when you benchmark or report a measured speedup or regression, including complete framework flows and compilation of projects that consume the framework."
---

# Benchmark checklist

Use this when you produce a performance number: a PR's before and after, a regression claim, a hillclimb harness, or a library or config choice. Answer each question with evidence from a run, not a guess about the code.

For a quick ballpark the user asked for, one run is enough. Still check questions 4 and 7, and say that it is one run. Skip the rest unless that run looks wrong. A choice between options is never a ballpark.

## Before you run anything

- Write down the claim you expect to make, in the words you would ship ("export is 30% faster at p50 on the 60k-row dataset"). The questions test that sentence.
- Read the measurement script. Note what it times, counts, and ignores.
- Check host load and available core count with tools supported by the execution environment. If the machine is busy, find out what is running. If you cannot stop it, interleave the sides so both see the same noise, and say so in the report.

## Complete framework flows

When the claim or change crosses subsystem boundaries, benchmark the complete relevant flow alongside focused stage measurements. Choose the workloads and completion boundaries that answer the claim.

- **Command execution.** Exercise the real path through authorization/context, entity fetch or replay, decision interpretation, event encoding and storage append to the command result. State the entrypoint and timed boundary. Count successful commands, rejections, errors and retries separately.
- **Decider execution.** Measure decision logic and interpretation where their costs matter, including required capabilities and consumption of emitted events. A constructed decision description or deferred Task is not completed decision execution.
- **Storage and downstream visibility.** Use the relevant backend and its declared completion/durability settings. Choose representative event-history lengths, payloads, batches, cache/snapshot states and contention. Verify event/state outcomes. State whether completion means command acceptance, storage confirmation or subscriber/projection visibility; wait for the claimed boundary before stopping timing. Include transport or downstream integration work when the claim covers it.

## Compilation is a performance surface

Framework changes can change the time and memory required to compile a consuming project. Benchmark relevant downstream compilation as well as the framework build. Choose scenarios appropriate to the claim.

- **Representative consumer.** Compile a fixture using actual framework declarations, Template Haskell splices and Generic-derived instances. Exercise the generated APIs/instances. Vary type, field, constructor, splice or module counts when investigating scaling costs.
- **Distinct build scenarios.** Report clean consumer compilation, incremental recompilation after a defined edit, and unchanged rebuilds separately where relevant. Include edits to generated declarations or shared types when their downstream invalidation is part of the claim. State whether dependency/framework builds, downloads, typechecking, code generation and linking are included.
- **Controlled build state and actual work.** Prepare equivalent dependencies for each framework revision and restore the same sources, edit and artifact/cache conditions for each sample. Record what is cached. Retain evidence that the expected modules/splices were processed, expected declarations were produced and compilation succeeded. Name no-op rebuild measurements explicitly.
- **Costs and attribution.** Measure elapsed time and, where relevant, CPU and peak memory across the compiler and relevant splice processes. Use separate compiler-phase diagnostics to investigate splice execution, deriving/instance resolution, typechecking, optimization and code generation. Generic-derived code can also do runtime work; measure that behavior when relevant rather than treating every generic operation as compile-time execution.

## The questions

1. **Why not double?** Name the limiter. Profile in a run you do not report, because profilers and tracers slow the work down. Use supported host CPU/I/O tools and relevant Haskell profiling or runtime statistics, including allocation and garbage-collection costs where they explain the limiter, then map the hot spot to source. Verify which diagnostics the actual executable supports; keep diagnostic/profiling runs separate from reported timings. Watch the load generator too: if it saturates first, you measured the load generator. If a change did not move the number, the limiter explains why, so find it before you call the change useless.
2. **Was it tuned?** Run every side the way production runs it: release builds, production flags and env, batching and transaction settings, connection pools, caches as warm or cold as production sees them, and the same versions and data. For Haskell comparisons, record the compiler and dependency versions, build/optimization mode, and effective runtime settings, including parallelism and garbage-collection settings where relevant. Account for executable defaults and environment overrides; do not compare an unoptimized development build with an optimized build as if the implementation alone changed. If one side runs on defaults, you compared configurations, not implementations. A limiter that is a setting (a commit per row, a debug build, a missing index) means that side is untuned. Tune it and measure again before you pick a winner. If you cannot tune it, do not pick a winner from that run. When the user is choosing what to adopt, narrowing the claim to the code as it ships today does not fix this, because they adopt the option, not today's settings.
3. **Did it break limits?** Do the arithmetic. Compare bytes per second with disk and network bandwidth, operations per second times cost per operation with your cores, and the time saved with the time the changed piece took. Removing a piece that takes 10% of the run can make the run at most about 11% faster. A result past a limit measured something other than the work, such as a cache, a no-op, or a bug.
4. **Did it error?** Count failures and non-success responses, and check that outputs are correct, not just present. Rejections are often fast, and timeouts and retries are slow. If the script does not count errors, add the count.
5. **Does it reproduce?** Run each side at least 5 times, alternating (A, B, A, B, and so on) so warmup, lazy initialization, caches, and drift favor neither. Report the median and range. A gap smaller than the run-to-run variation is no measurable difference. When the call is close, use a rank-sum test or the harness's own statistics.
6. **Does it matter?** Next to any micro result, measure the relevant framework operation, language/compiler path or toolchain workflow the caller waits on, with realistic data sizes and concurrency, and report the micro result as a share of the whole. A helper that takes 1% of a request can make the request at most 1% faster, however fast the helper gets.
7. **Did it even happen?** Confirm the work ran inside the timed region: the request reached the server, the rows were written, the bytes were read, and the code used the result. For Haskell, demand the part of a pure result required by the measured contract inside the timed region; timing only construction or an outer constructor can leave the intended work unevaluated. Actually execute deferred `Task` effects and consume the required stream output. Match evaluation depth to the real consumer rather than forcing every result fully. Check for work shared across samples, cached in setup, or optimized away, and distinguish setup from the work the claim includes. Deferred work and timeouts can produce numbers for work that never happened. For compilation claims, confirm the intended compiler/splice work happened and the build succeeded; report cache state and included build phases.

## Report

- Lead with the verdict: faster, slower, no measurable difference, or inconclusive.
- Give the number with its unit, the run count, the range, and the limiter. For example, "p50 41 ms → 33 ms, median of 7 runs per side, range 32 to 35 ms after, bound by JSON parsing on one core."
- Call the verdict inconclusive when you claim a difference but cannot name the limiter, when a side ran untuned, or when you could not check questions 4 and 7. Name the gap.
- Keep a PR body to one primary number, per the **Opening a PR** playbook. Put the runs, range, and limiter evidence in a linked artifact or notes file.

Use [Framework benchmark](../framework-benchmark/SKILL.md) for matched samples and retained raw evidence. Its coarse suite-duration alarm includes startup and build costs. Use a workload-specific driver for runtime throughput or consumer compilation claims.

## Other perf material

- Vet the **Perf issue** playbook's baseline before the playbook plans from it, and every number after that.
- Vet the **Hillclimb** playbook's harness before it is frozen. The frozen harness then prints error and work counts, so each keep-or-revert checks questions 4 and 7 for free.
