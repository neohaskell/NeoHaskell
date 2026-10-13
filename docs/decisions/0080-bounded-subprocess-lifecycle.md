# ADR-0080: Preserve subprocess output while owning its lifecycle

## Status

Proposed implementation under NickSeagull's [Gate1 existing-guarantee approval](https://github.com/neohaskell/NeoHaskell/pull/911#issuecomment-6150731937); final review remains pending.

## Context

Lazy pipe reads can block a noisy child, and cancellation can bypass child cleanup. The full-output API is already used by PDF and video integrations.

## Decision

Preserve public signatures, complete stdout/stderr and normal/nonzero exit codes. Drain both streams strictly and concurrently. No new output cap, truncation or breaking API. Declare the already-resolved `unix` package as a conditional non-Windows direct dependency for public signalProcess/sigKILL; Windows keeps native process termination. No custom FFI, shell kill or unstable process-internal API. The earlier one-MiB rejection proposal is withdrawn in favor of explicit above-one-MiB preservation tests, following the maintainer's decision.

Own the direct child and readers across timeout, synchronous failure and asynchronous cancellation. Mask resource acquisition, scope readers, apply the deadline to drains plus wait, terminate/reap on abnormal exit, close handles and join readers. POSIX TERM receives one-second cleanup grace before direct SIGKILL; Windows process-library termination is already forced. No descendant termination or universal kernel-uninterruptible bound is promised.

## Consequences

Full output uses memory proportional to output. Bounded retention requires a separate explicit future contract. Security and resource review precedes implementation; local security records never enter the PR. Change023 owns executable real-pipe and PID-lifecycle regression coverage; missing reader/handle observations are not implied by PID disappearance.
