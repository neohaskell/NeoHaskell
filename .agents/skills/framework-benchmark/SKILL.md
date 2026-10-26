---
name: framework-benchmark
description: Run matched baseline and treatment measurements for NeoHaskell framework flows, compiled test suites and consumer compilation, retaining workload counts, inputs and raw samples.
---

# Measure a framework change

Read [the benchmark checklist](../benchmark-checklist/SKILL.md) before selecting a workload or reporting a result. Measure the affected command, decider, storage or downstream completion boundary. For derivation or generated API changes, include representative consumer compilation.

Write a protocol using [the runner contract](references/protocol.md). Run `python3 .agents/skills/framework-benchmark/scripts/compare.py <protocol.json>`. The runner alternates baseline and treatment, keeps raw logs and records input hashes. It reports medians and ranges, without deciding whether the difference is meaningful.

Use a workload driver that independently checks the resulting state and emits completed work and error counts. Read that driver before measuring. Use a separate diagnostic run to identify the limiter. Compiled suite duration includes command startup and any build work. It is not request latency or framework throughput.

For compilation, separate clean, incremental and unchanged builds. Put each sample's cache preparation in the protocol's `prepare` command. Name the modules or splice work the sample must perform. An unchanged-build scenario may deliberately perform no compiler work. Keep dependencies, flags, edits and cache state equivalent across sides.

For scheduled suite-duration checks, run `python3 .agents/skills/framework-benchmark/scripts/bench.py --strict`. The [suite registry](references/nightly-suites.json) owns selections, prerequisites, timeouts and optional calibrated budgets. A null budget records calibration data. It establishes no performance threshold.

Keep the protocol and outputs under repository-root `.agents-state/framework-benchmark/`. Report the actual completion boundary, errors, successful work, settings, samples, range and limiter. Follow the checklist's rules for an inconclusive result. The hypothetical throughput design target is not a required load-test size.
