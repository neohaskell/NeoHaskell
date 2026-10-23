# Continue the production cleanup

Start from the current main revision and review this guidance PR before implementing the separate production stack. Use the user's requested GPT-6 Sol High for that stage. Do not merge either stage without the user's shipping instruction.

## Build the core operations first

Source inspection of [Task](../../../../core/core/Task.hs) found typed recovery, synchronous-exception conversion, and finalization. It found no mask-and-restore operation or exception-preserving observation and cleanup operation. `Task.asResultSafe` changes synchronous exceptions into text. It cannot replace dispatcher code that rethrows the original exception.

Inspect [AsyncTask](../../../../core/concurrency/AsyncTask.hs) with [dispatcher ownership handoff](../../../../core/service/Service/Integration/Dispatcher.hs). `runUnmasked` calls `asyncWithUnmask`, then runs the Task inside the supplied unmask function. Masked setup protects publication or cleanup of a worker. The child's callbacks must remain cancellable. Choose an understandable public name without losing that distinction. Preserve the two callback masking tests in [DispatcherSpec](../../../../core/test/Integration/DispatcherSpec.hs).

Add missing operations in a lower core PR. Define the typed-failure, synchronous-exception, asynchronous-cancellation, restoration, and cleanup contracts before implementation. Test the actual contracts. Keep service ownership policy in the service. Keep raw exception mechanisms in the Task and AsyncTask owners. Thread-status inspection currently serves deterministic concurrency tests. Give it an owning concurrency or test-support API with a stated diagnostic contract.

[Directory](../../../../core/system/Directory.hs) exports no temporary-directory acquisition or scoped cleanup operation at the inspected base. Add a scoped operation before migrating the storage test. Prove unique allocation and cleanup after success, typed failure, and runtime exception. Do not claim an existing API or replace the test path with another fixed directory.

## Refactor services after core is available

Refactor [CommandExecutor.Core](../../../../core/service/Service/CommandExecutor/Core.hs) into named domain steps with a coherent configuration record. Inspect callers before setting the final argument order. Replace local IORef operations with the suitable core state API. Flatten [reportIfRegistered](../../../../core/service/Service/CommandResultReceipt/Internal.hs) while preserving its atomic claim. `Var.get` followed by `Var.set` is not an atomic claim. [AtomicVar.modifyWithResult](../../../../core/concurrency/AtomicVar.hs) provides an existing atomic operation, but verify the complete receipt lifecycle before selecting it.

Separate outcome types and their operations from queue admission and worker ownership in [Dispatcher](../../../../core/service/Service/Integration/Dispatcher.hs) where that reduces reader load. Preserve STM ownership transitions. Convert internal labels into closed types. Derive serialization when it preserves the wire contract. Keep a documented custom codec when existing omission, redaction, or compatibility behavior requires it.

Migrate [StorageMetadataSpec](../../../../core/test/Service/CommandExecutor/StorageMetadataSpec.hs) to the scoped Directory operation. Delete its agent-workspace debt entry with the same PR. The workspace rule is not fully enforced until that entry is gone.

## Verify each boundary before the next PR

Use the repository's architecture, test, consumer-verification, regression-evidence, and release skills. Keep generated consumers and public API migrations in the owning PR. Execute cancellation, receipt, storage reopen, and JSON regression checks against the changed implementation. A rename or successful compilation does not prove those runtime contracts.

Run the [static gate](static-checks.md) and remove resolved exact debt with `--prune-debt`. The initial inventory also exposes older exception-boundary debt outside PR #934. Keep that scope explicit. Do not rewrite unrelated production modules merely to make this follow-up claim that the inventory is empty.
