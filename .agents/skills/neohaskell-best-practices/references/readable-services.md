# Write readable service operations

## Keep the main operation shallow

State the data shape before implementing the flow. Name stable dependencies and configuration in a coherent record. Pass changing command or event data last. Aim for three or four ordinary arguments. Repeated `Nothing` arguments indicate a default configuration or a missing domain mode. Do not combine unrelated values merely to meet an argument count.

Make the service operation read from top to bottom at one level of abstraction. Extract a named responsibility when a branch combines validation, storage, retries, reporting, and cleanup. Keep meaningful domain decisions visible. Sequence fallible tasks when a case only propagates the same error. Use the established error-mapping operation for translation and a recovery operation for an actual fallback. Keep explicit result cases when inspecting a result is itself the operation.

For an illustrative mail service, `MailDependencies` holds the queue and sender capabilities. `MailConfig` holds the retry policy. `sendMail dependencies config mail` takes the changing message last. Validation, delivery, and receipt recording are named steps. A receipt step must preserve its atomic claim and cancellation behavior when the surrounding flow becomes shallower.

## Put mechanisms in their owning core modules

Inspect maintained NeoHaskell APIs before importing a GHC or IO equivalent. Choose the core state API by its contract. Local mutation, an atomic claim, and blocking coordination need different guarantees. A separate read and write does not become atomic because each call uses a wrapper.

Keep raw exception manipulation in the owning core task and concurrency modules. Service code composes their operations. If a necessary mask, restoration, or exception-preserving cleanup operation is absent, implement and verify that operation in a separate core prerequisite PR. Then migrate the service. A task-to-IO adapter that runs tasks internally is evidence of a missing task operation or a misplaced runner. Keep legitimate foreign IO adapters at their explicit boundaries.

Distinguish typed failures, synchronous exceptions, and asynchronous cancellation in the core contract. Converting an exception into text cannot preserve its original identity for rethrowing. Define whether cleanup failures replace, accompany, or leave the original failure intact. Verify success, typed failure, runtime exception, and cancellation paths before using a cleanup operation.

In an illustrative worker handoff, the parent protects registration until it can publish the child or clean it up. The child's callback still needs to receive cancellation. The owning concurrency operation must state when cancellation is deferred and restored for each participant.

Wrap required thread inspection in an owning concurrency or test-support API before using it elsewhere. Keep ownership policy in the service and exception mechanisms in core. Masking controls cancellation delivery. It does not provide mutual exclusion. Preserve an existing STM transaction when extracting its surrounding Task plumbing.

## Let types own outcomes and serialization

Use closed ADTs for internal failure labels, error categories, and outcomes. Put required payloads on their constructors. Convert to text only at the presentation or wire boundary. Avoid a record whose unrelated optional fields allow contradictory success and failure combinations. Retain `Task Text` only where free-form external diagnostics are the actual contract.

Derive serialization from the authoritative type when the resulting format matches the contract. A manual JSON object needs a demonstrated compatibility, redaction, omission, or protocol requirement. Document that reason at the codec owner and test valid encodings and rejected invalid combinations. Deriving alone does not prove semantic validity. Check constructors and decoding as well.

An illustrative delivery model keeps failure details on the failed case:

```haskell
data DeliveryFailure = AddressRejected | RetryLimitReached

data DeliveryOutcome
	= Delivered
	| Failed DeliveryFailure
```

This shape cannot pair `Delivered` with a failure reason. Serialize from that same outcome type when its derived encoding meets the wire contract. A custom decoder must also reject a success value paired with a failure reason.

## Give a module one useful responsibility

Group a datatype and its operations in a focused module when callers gain a useful contract. In the illustrative delivery model, a `DeliveryOutcome` module owns the outcome type and its display conversion. Queue admission and worker ownership have different contracts. Split them when that helps a reader follow the service.

Apply SOLID in proportion to functional code. Keep one reason to change, preserve substitutable service contracts, and depend on the capabilities an operation uses. Existing records of functions already support dependency injection. Add a new interface or typeclass only when it removes a demonstrated coupling or encodes a useful contract. A module per helper increases the work needed to follow the flow.

A short module header may explain responsibility and normal usage. Follow the [comment exceptions](../../no-comments/references/comment-reviewer.md). Keep phase narration and temporary-workaround explanations out of source comments.

## Use scoped temporary test resources

Tracked application code and tests must not depend on `.agents-state`. That directory belongs to agent work and evidence. Use a core temporary-folder operation that scopes acquisition and cleanup across success, typed failure, and exceptions. Avoid a shared fixed `/tmp` path or a repository working directory.

For an illustrative storage test, allocate a unique folder, write a value, close and reopen storage, and assert that the value survives. Keep both storage sessions within the folder's lifetime. Prove cleanup after success and failure. If the core lacks the required scoped operation, add and verify it before migrating callers. Remove each resolved lint debt entry with its migration.
