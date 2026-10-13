# Change 023: Drain subprocess pipes and clean up the owned child

This proposal prevents a noisy child process from timing out merely because its output pipe fills, and prevents cancellation from leaving that child running. It changes only `Subprocess.runWithTimeout`; `run`, `open`, and `openInherit` remain outside this slice.

NickSeagull approved repairs to existing guarantees on [October13](https://github.com/neohaskell/NeoHaskell/pull/911#issuecomment-6150731937), explicitly excluding a new output cap or breaking API. This spec reconciles that actual decision; final review/merge/release remain unapproved. The earlier C5 proposed rejection is replaced by complete-output preservation, not by weakening an existing product expectation.

```yaml spec
issue: adhoc:subprocess-lifecycle
kind: bug
touches: [system]
breaking: false
new-dependency: true
new-capability: false
new-extension-point: false
```

## Contract delta

```diff signatures
```

Existing signature remains `Subprocess.runWithTimeout :: Int -> Text -> Array Text -> Path -> Task Error Completion`. Preserve full stdout/stderr and normal/nonzero exit codes. Strict concurrent draining repairs the pipe deadlock; there is no capture-size rejection, silent truncation, or bounded-memory claim. Complete output retains memory proportional to output. Timeout stays `TimeoutError`; synchronous create/read failures are `ProcessError`; asynchronous cancellation propagates after cleanup. Preserve existing text decoding semantics.
Timeout includes pipe draining as well as process wait. The child and readers are owned through every exit. Failure/timeout/cancel requests direct-child termination; after 1 second grace force termination, reap, close handles, and join/cancel readers. Successful completion closes handles and joins readers too. Do not wait forever for EOF held by a descendant. Descendant termination and an OS sandbox are not promised. Uninterruptible kernel waits are not universally time-bounded.

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Real stderr exceeding pipe capacity completes with exit 7, not timeout | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#drains real stderr before waiting for process exit` | integration | subprocess:real |
| C2 | Normal and nonzero exits return complete output on both streams | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#captures complete normal and nonzero child output` | integration | subprocess:real |
| C3 | Simultaneous pipe writers exceed pipe capacity without timeout | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#drains simultaneous real pipes beyond pipe capacity` | integration | subprocess:real |
| C4 | Complete one-MiB output remains available without truncation | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#captures complete one MiB output per stream` | integration | subprocess:real |
| C5 | Output above one MiB remains complete with no new rejection limit | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#preserves complete output above one MiB without a new capture cap` | integration | subprocess:real |
| C6 | Timeout leaves the readiness-observed direct child absent from process table | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#reaps an observed direct child after timeout` | integration | subprocess:real |
| C7 | TERM-resistant direct child is absent after bounded timeout cleanup | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#reaps an observed TERM-resistant child after timeout` | integration | subprocess:real |
| C8 | Cancellation propagates and leaves readiness-observed direct child absent | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#propagates cancellation and reaps the observed direct child` | integration | subprocess:real |
| C9 | Descendant-held output pipes remain under deadline without killing descendant | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#times out when a descendant retains the output pipes` | integration | subprocess:real |
| C10 | Real UTF8 decoding failure returns ProcessError after direct-child cleanup | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#reaps the direct child after a real UTF8 pipe decoding failure` | integration | subprocess:real |
| C11 | Deadline still covers process wait after both streams reach EOF | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#times out after pipe EOF while the child still runs` | integration | subprocess:real |
| C12 | Cancellation during active two-stream output preserves async and reaps child | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#propagates cancellation while both pipes are backpressured` | integration | subprocess:real |
| C13 | Creation failure maps to typed ProcessError | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#returns a typed error when process creation fails` | integration | subprocess:real |
| C14 | Repeated cancellation during TERM grace still reaps the owned child and propagates async | `hspec:nhcore-test-core:core/test/SubprocessSpec.hs#reaps a TERM-resistant child despite repeated cancellation during cleanup` | integration | subprocess:real |

### Fixture controls and remaining coverage boundary

C1 remains the exact original red reproduction. C2–C5 require full output through simultaneous pipes. The former C5 unapproved cap proposal was reconciled to the maintainer's explicit existing-contract decision; no upstream captured-output guarantee is removed.

C6–C8 create a private synthetic workspace. The child writes its own PID only after installing its TERM policy; a separate control requires that PID to be live and its command to contain this workspace before the lifecycle action. After completion/cancellation the observer requires no process-table entry, rejecting zombies as well as live survivors. This is direct-child observation, not process-tree cleanup. A finally cleanup sends SIGKILL only after PID identity validation, waits until the child is absent or a non-executing zombie, and removes only its own workspace; its own failure is red, never ignored. That separate controller cannot reap the Haskell parent's child. Cleanup records zombie versus absent; the test assertion still requires absence and rejects zombies. A post-REPL observation verifies no executing fixture remains. The TERM-resistant test has a four-second test budget (one-second process timeout plus proposed one-second grace and observation margin), not a new production deadline.

C9 observes a separately owned live descendant after its parent is reaped and the pipe deadline returns; fixture cleanup kills that descendant without claiming production tree ownership. C10 forces a real invalid UTF8 byte followed by EOF while the child remains live; the peer reader must be cancelled and child reaped before ProcessError. C11 covers waiting after EOF. C9 separately requires both descendant writers to observe BrokenPipeError after timeout while the descendant remains alive, proving read-end closure rather than inferring it from PID disappearance. It checks newly created GHC thread identities have finished/died after scoped work completes. These observations use real pipes and runtime thread status, not a public test seam. PDF impact coverage remains mandatory before acceptance.

Review principle: concurrent draining preserves complete retained output (C1–C5). Ownership lasts through ordinary and repeated cancellation (C6–C8,C14). Real process controls distinguish failure from absent/malformed fixtures. A passing structural locator check does not make the still-unobserved reader guarantees complete.

## User impact

No API or capture-limit change; PDF callers keep full extracted text. Declare conditional direct `unix` dependency on non-Windows platforms for the existing resolved POSIX signal API; no new package version or lockfile change. Windows retains the native process termination API. The private one-second TERM grace allows escalation for an unresponsive direct child; it is cleanup time, not a changed extraction deadline. POSIX uses forced direct-child termination after grace; Windows uses the process library's native forced termination. No process-tree or sandbox guarantee. Full captured output is not memory-bounded; an opt-in retention policy is a separate future decision.

## ADR

[ADR-0080](../decisions/0080-bounded-subprocess-lifecycle.md) is Proposed and part of the approval request.

## Localized implementation and verification plan

Files: `core/system/Subprocess.hs` (`runWithTimeout`); `core/test/SubprocessSpec.hs`; `core/test-core/Main.hs`; `core/nhcore.cabal`; `docs/changes/test-surfaces.json`; generated `codemap/**`; separate Framework release fragment. No lower API export added. Test skeleton copy-adapts `core/test/TaskSpec.hs`.

Uses resolved in generated signatures: `Subprocess.runWithTimeout`, `Task.asResult`, `Text.length`; existing Task/Test conventions. `./dev who-calls runWithTimeout Subprocess` had no HIE index; emitted bounded fallback confirms PDF `extractText`/`pdfinfo` and video `realRunner` callers. Run PDF impact tests and the full integrations suite, not only video.

After explicit Gate 1: spec plan plus explicit security/performance review before product code (current system tag is not evidence of low risk). Real synthetic fixtures only, macOS and Linux receipts. One watcher, check/test repair loop, at most two repairs. No blanket async catch; no process-tree claim. Runtime attestations for the additional real regression checks must be added and executed before implementation acceptance; no passing result is claimed now.
