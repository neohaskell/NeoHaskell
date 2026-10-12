# Change 023: Drain subprocess pipes and clean up the owned child

This proposal prevents a noisy child process from timing out merely because its output pipe fills, and prevents cancellation from leaving that child running. It changes only `Subprocess.runWithTimeout`; `run`, `open`, and `openInherit` remain outside this slice.

**Pre-approval draft:** no implementation or approval. C1 is the minimal executable repro. The additional verification obligations below remain mandatory. Each needs an executable proving-test locator and boundary attestation before final spec approval; the executable table covers real pipes, capture-policy boundaries and direct-child lifecycle, but is not complete Gate 1 readiness or proof of reader-task cleanup. No historical approval is claimed.

```yaml spec
issue: adhoc:subprocess-lifecycle
kind: bug
touches: [system]
breaking: true
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

```diff signatures
```

Existing signature remains `Subprocess.runWithTimeout :: Int -> Text -> Array Text -> Path -> Task Error Completion`. Behavioral compatibility changes are deliberate and need approval: retain at most 1,048,576 raw bytes separately for stdout and stderr, continue strict concurrent draining past the cap, and return `ProcessError` for overflow rather than a silently truncated completion. Timeout wins if the deadline occurs; cancellation propagates after cleanup. Within-bound output/nonzero exit keep `Completion` semantics. Invalid retained text encoding and synchronous reader failures become typed `ProcessError`, not uncaught exceptions.

Timeout includes pipe draining as well as process wait. The child and readers are owned through every exit. Failure/timeout/cancel requests direct-child termination; after 1 second grace force termination, reap, close handles, and join/cancel readers. Successful completion closes handles and joins readers too. Do not wait forever for EOF held by a descendant. Descendant termination and an OS sandbox are not promised. Uninterruptible kernel waits are not universally time-bounded.

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Real stderr exceeding pipe capacity completes with exit 7, not timeout | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#drains real stderr before waiting for process exit` | integration | subprocess:real |
| C2 | Normal and nonzero exits return complete output on both streams | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#captures complete normal and nonzero child output` | integration | subprocess:real |
| C3 | Simultaneous pipe writers exceed pipe capacity without timeout | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#drains simultaneous real pipes beyond pipe capacity` | integration | subprocess:real |
| C4 | Proposed one-MiB policy: exact-cap output remains complete (awaiting policy choice) | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#captures exactly the proposed one MiB per stream` | integration | subprocess:real |
| C5 | Proposed one-MiB policy: over-cap output returns ProcessError, not timeout or partial success (awaiting policy choice) | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#rejects output above the proposed one MiB capture cap` | integration | subprocess:real |
| C6 | Timeout leaves the readiness-observed direct child absent from process table | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#reaps an observed direct child after timeout` | integration | subprocess:real |
| C7 | TERM-resistant direct child is absent after bounded timeout cleanup | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#reaps an observed TERM-resistant child after timeout` | integration | subprocess:real |
| C8 | Cancellation propagates and leaves readiness-observed direct child absent | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#propagates cancellation and reaps the observed direct child` | integration | subprocess:real |

### Fixture controls and remaining coverage boundary

C1 remains the exact original red reproduction. C2–C5 run two simultaneous Python pipe writers with full-write loops. C4/C5 are executable proposed-policy tests, not evidence that the one-MiB default has been approved. They must change only with the recorded policy decision, not merely to turn green.

C6–C8 create a private synthetic workspace. The child writes its own PID only after installing its TERM policy; a separate control requires that PID to be live and its command to contain this workspace before the lifecycle action. After completion/cancellation the observer requires no process-table entry, rejecting zombies as well as live survivors. This is direct-child observation, not process-tree cleanup. A finally cleanup sends SIGKILL only after PID identity validation, waits until the child is absent or a non-executing zombie, and removes only its own workspace; its own failure is red, never ignored. That separate controller cannot reap the Haskell parent's child. Cleanup records zombie versus absent; the test assertion still requires absence and rejects zombies. A post-REPL observation verifies no executing fixture remains. The TERM-resistant test has a four-second test budget (one-second process timeout plus proposed one-second grace and observation margin), not a new production deadline.

Reader-task join/handle-close observation, deterministic reader I/O failure and a descendant-held pipe are **not proved** by PID disappearance. The public API exposes neither reader handles nor threads; a narrow approved internal test seam or separate lifecycle observation method remains necessary before approving/accepting those claims. Do not substitute a fake reader or infer thread cleanup from a returned result. PDF impact coverage also remains mandatory after policy choice.

Review principle: draining and retained-output limits are separate obligations (C1–C5). Ownership lasts through cancellation (C6–C8). Real process controls distinguish failure from absent/malformed fixtures. A passing structural locator check does not make the still-unobserved reader guarantees complete.

## User impact

Breaking behavioral limit, no signature change. Under the proposed policy, applications collecting over 1 MiB on either stream would receive `ProcessError`; PDF text extraction uses this API and is explicitly affected. Approve this policy or choose an additive bounded API first. Migration/release notes must identify the real caller behavior and must not invent a replacement API. Existing extraction timeout defaults do not change. Limits of 1,048,576 bytes and 1 second are proposed new constants. No constants have changed yet.

## ADR

[ADR-0080](../decisions/0080-bounded-subprocess-lifecycle.md) is Proposed and part of the approval request.

## Localized implementation and verification plan

Files: `core/system/Subprocess.hs` (`runWithTimeout`); `core/test/SubprocessSpec.hs`; `core/test-core/Main.hs`; `core/nhcore.cabal`; `docs/changes/test-surfaces.json`; generated `codemap/**`; separate Framework release fragment. No lower API export added. Test skeleton copy-adapts `core/test/TaskSpec.hs`.

Uses resolved in generated signatures: `Subprocess.runWithTimeout`, `Task.asResult`, `Text.length`; existing Task/Test conventions. `./dev who-calls runWithTimeout Subprocess` had no HIE index; emitted bounded fallback confirms PDF `extractText`/`pdfinfo` and video `realRunner` callers. Run PDF impact tests and the full integrations suite, not only video.

After explicit Gate 1: spec plan plus explicit security/performance review before product code (current system tag is not evidence of low risk). Real synthetic fixtures only, macOS and Linux receipts. One watcher, check/test repair loop, at most two repairs. No blanket async catch; no process-tree claim. Runtime attestations for the additional real regression checks must be added and executed before implementation acceptance; no passing result is claimed now.
