# Write readable service operations

## Keep the main operation shallow

State the data shape before implementing the flow. Name stable dependencies and configuration in a coherent record. Pass changing command or event data last. Aim for three or four ordinary arguments. Repeated `Nothing` arguments indicate a default configuration or a missing domain mode. Do not combine unrelated values merely to meet an argument count.

Make the service operation read from top to bottom at one level of abstraction. Extract a named responsibility when a branch combines validation, storage, retries, reporting, and cleanup. Keep meaningful domain decisions visible. Replace cases that only propagate the same Task error with ordinary Task sequencing. Use `Task.mapError` for error translation and `Task.recover` for recovery. Read their actual signatures before composing them. Keep explicit `Result` cases when inspecting a result is itself the operation.

`Service.CommandExecutor.Core.execute`, `executeNormalized`, and `executeInner` are review examples. Their current arguments and nested paths are migration targets, not approved templates. `CommandResultReceipt.Internal.reportIfRegistered` needs the same review. Preserve its atomic claim and asynchronous cancellation when flattening it.

## Put mechanisms in their owning core modules

Inspect maintained NeoHaskell APIs before importing a GHC or IO equivalent. Use `Var` for local mutable references. Use `AtomicVar.modifyWithResult` for an atomic claim, and `ConcurrentVar` for blocking coordination. A separate read and write does not become atomic because each call uses a wrapper. No `Ref` module exists in the inspected repository.

Keep raw exception manipulation in `Task` and `AsyncTask`. Service code composes Task operations. If a necessary mask, restoration, or exception-preserving cleanup operation is absent, implement and verify that operation in a separate core prerequisite PR. Then migrate the service. A `Task.fromIO` block that runs Tasks internally is evidence of a missing Task operation or a misplaced runner. Keep legitimate foreign IO adapters at their explicit boundaries.

`Task.asResultSafe` converts synchronous exceptions into text and propagates asynchronous exceptions. It cannot replace a contract that must rethrow the original exception. `Task.finally` provides cleanup on success, typed failure, and runtime exception. Its typed cleanup failures are discarded, while runtime cleanup exceptions can escape. Preserve those distinctions in any new operation.

Wrap required thread inspection in an owning concurrency or test-support API before using it elsewhere. Keep ownership policy in the service and exception mechanisms in core. Masking controls cancellation delivery. It does not provide mutual exclusion. Preserve an existing STM transaction when extracting its surrounding Task plumbing.

## Let types own outcomes and serialization

Use closed ADTs for internal failure labels, error categories, and outcomes. Put required payloads on their constructors. Convert to text only at the presentation or wire boundary. Avoid a record whose unrelated optional fields allow contradictory success and failure combinations. Retain `Task Text` only where free-form external diagnostics are the actual contract.

Derive serialization from the authoritative type when the resulting format matches the contract. A manual JSON object needs a demonstrated compatibility, redaction, omission, or protocol requirement. Document that reason at the codec owner and test valid encodings and rejected invalid combinations. Deriving alone does not prove semantic validity. Check constructors and decoding as well.

The dispatcher's `IntegrationRunnerOutcome` already distinguishes processed and failed results. Its text, failure-stage, and message helpers belong with that type when extraction reduces the reader's work. Its hand-built JSON needs a wire-contract review, not an automatic conversion that changes existing logs. Inspect the command executor's textual outcome and failure fields under the same rule.

## Give a module one useful responsibility

Group a datatype and its operations in a focused module when callers gain a useful contract. `Service.Integration.Dispatcher.IntegrationOutcome` with `IntegrationOutcome.text` is a proposed shape, not an existing API. Queue admission, worker ownership, and outcome formatting need separate review in the large dispatcher.

Apply SOLID in proportion to functional code. Keep one reason to change, preserve substitutable service contracts, and depend on the capabilities an operation uses. Existing records of functions already support dependency injection. Add a new interface or typeclass only when it removes a demonstrated coupling or encodes a useful contract. A module per helper increases the work needed to follow the flow.

A short module header may explain responsibility and normal usage. Follow the [comment exceptions](../../no-comments/references/comment-reviewer.md). Keep phase narration and temporary-workaround explanations out of source comments.

## Use scoped temporary test resources

Tracked application code and tests must not depend on `.agents-state`. That directory belongs to agent work and evidence. Use a NeoHaskell temporary-folder operation that scopes acquisition and cleanup across success, typed failure, and exceptions. Avoid a shared fixed `/tmp` path or a repository working directory.

At the inspected base, `Directory` has no scoped temporary-folder operation. Add it in the core prerequisite PR before migrating `StorageMetadataSpec`. Remove that test's exact lint debt entry with the migration. Do not invent an existing function name or substitute a raw filesystem call in each test.
