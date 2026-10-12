# Change 023: Drain subprocess pipes and clean up the owned child

This proposal prevents a noisy child process from timing out merely because its output pipe fills, and prevents cancellation from leaving that child running. It changes only `Subprocess.runWithTimeout`; `run`, `open`, and `openInherit` remain outside this slice.

**Pre-approval draft:** no implementation or approval. C1 is the minimal executable repro. The additional verification obligations below remain mandatory. Each needs an executable proving-test locator and boundary attestation before final spec approval; the C1-only table is a preliminary draft, not complete Gate 1 readiness or proof of the broader contract. No historical approval is claimed.

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

### Required regression coverage before final spec approval

Before final spec approval, add executable proving-test locators and real subprocess boundary attestations for these checks; establish honest pre-fix outcomes and require all approved checks green before implementation acceptance:

- Both pipes write concurrently beyond pipe capacity; below/exact capture cap returns full bytes and exit status.
- Above-cap output is discarded while draining and returns the approved typed error, never partial successful output.
- Normal completion, nonzero exit, timeout, cancellation and reader failure close owned handles and join readers.
- Timeout and cancellation terminate/reap the recorded direct child, including a TERM-resistant fixture; cancellation still propagates.
- A descendant-held pipe cannot block direct-child timeout cleanup forever; no descendant termination is promised.
- Creation/reader failure produces the specified typed error without resource leaks; PDF callers receive explicit impact coverage.

Review principle: draining and retained-output limits are separate obligations. Ownership lasts through cancellation. Tests observe real pipes and recorded owned PIDs; result values alone do not prove cleanup. Reader cleanup needs direct thread/lifecycle observation in a narrow internal test seam if the public API cannot expose it; do not add a public lifecycle framework just for tests. These are mandatory regression obligations, not substitutes for C1 or invented passing locators.

## User impact

Breaking behavioral limit, no signature change. Applications collecting over 1 MiB on either stream now receive `ProcessError`; PDF text extraction uses this API and is explicitly affected. Approve this policy or choose an additive bounded API first. Migration/release notes must identify the real caller behavior and must not invent a replacement API. Existing extraction timeout defaults do not change. Limits of 1,048,576 bytes and 1 second are proposed new constants. No constants have changed yet.

## ADR

[ADR-0080](../decisions/0080-bounded-subprocess-lifecycle.md) is Proposed and part of the approval request.

## Localized implementation and verification plan

Files: `core/system/Subprocess.hs` (`runWithTimeout`); `core/test/SubprocessSpec.hs`; `core/test-core/Main.hs`; `core/nhcore.cabal`; `docs/changes/test-surfaces.json`; generated `codemap/**`; separate Framework release fragment. No lower API export added. Test skeleton copy-adapts `core/test/TaskSpec.hs`.

Uses resolved in generated signatures: `Subprocess.runWithTimeout`, `Task.asResult`, `Text.length`; existing Task/Test conventions. `./dev who-calls runWithTimeout Subprocess` had no HIE index; emitted bounded fallback confirms PDF `extractText`/`pdfinfo` and video `realRunner` callers. Run PDF impact tests and the full integrations suite, not only video.

After explicit Gate 1: spec plan plus explicit security/performance review before product code (current system tag is not evidence of low risk). Real synthetic fixtures only, macOS and Linux receipts. One watcher, check/test repair loop, at most two repairs. No blanket async catch; no process-tree claim. Runtime attestations for the additional real regression checks must be added and executed before implementation acceptance; no passing result is claimed now.
