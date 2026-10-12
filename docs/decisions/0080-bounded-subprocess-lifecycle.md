# ADR-0080: Bound captured subprocess output and require owned-child cleanup

## Status

Proposed. No maintainer approval or implementation is recorded.

## Context

`Subprocess.runWithTimeout` is used by PDF and video integrations. Lazy pipe reads can prevent a child from exiting; cancellation can bypass child cleanup. Strict reads alone do not bound retained output.

## Decision proposed for approval

Keep the existing signature. Concurrently drain both pipes with bounded buffers. Retain at most 1,048,576 raw bytes per stream; after overflow, continue draining without retaining additional data and return `ProcessError` if the child otherwise completes. Timeout returns `TimeoutError`; cancellation remains cancellation after cleanup. No partial completion is reported as success. UTF-8 decoding follows the existing text API for retained bytes; invalid encoding produces a typed process error, not an uncaught synchronous exception.

Own the direct child, its pipe handles, and readers until cleaned. On timeout, cancellation, or reader failure, request termination, allow one second grace, then force termination if still alive and reap. Close all owned handles and cancel/join remaining readers. Normal completion also joins readers and closes handles. A descendant keeping a pipe open must not make timeout cleanup wait for EOF forever. This does not promise descendant termination or a universal bound on kernel-level uninterruptible waits. Unix force-kill behavior needs supported macOS/Linux implementation and tests; other platform support must not be asserted without evidence.

The 1 MiB per-stream cap and 1 second grace are new proposed constants, not existing behavior. The behavioral limit is breaking even though signatures remain unchanged. Existing PDF callers and downstream applications must be evaluated. Alternative: add an explicit bounded API and migrate video without narrowing the existing API. Maintainer must choose before implementation.

## Consequences

Expected retained payload is at most 2 MiB plus fixed read/decode buffers, not unbounded child output. UTF-8 decoding can allocate additional bounded Text memory; this is not a total RSS guarantee. Above-cap callers must handle `ProcessError` or adopt a separately approved file/streaming path. No silent truncation, retry, global process group kill, or framework-wide process redesign is included.

## Verification

Change 023 C1–C8 provide executable real pipe, proposed overflow, and direct-child timeout/cancellation checks. Reader failure, reader-task/handle cleanup and PDF-impact checks remain explicit incomplete coverage, not inferred from PID disappearance. Its C1 must be committed red with a current pipeline receipt. Security and performance review are required explicitly for this resource-owning boundary, even though the current `system` capability lacks risk tags. Review records do not substitute for executable checks.
