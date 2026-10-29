# Protected-value erasure

An application can erase one protected value while retaining its event and read model. The event store removes that value's key. Later authorized reads render `Erased`; other protected references remain `Available`. Already delivered plaintext cannot be revoked.

## Create and identify a value

Create protected values with `Erasable.new` inside a command decision. Each new call gets a fresh opaque reference. Copies and `Erasable.map` keep the original reference. An authorized application query can use `Erasable.reference` to discover a reference for a request. The reference is metadata, not proof that the caller owns the value.

The PostgreSQL event store keeps the key apart from event ciphertext. Set `PostgresEventStore.erasableApplicationScope` to a stable, nonempty identifier before creating protected values that the application intends to erase. Outbound deletion fails when the scope is empty. The stored application scope, owning entity name, and stream ID must match the deletion target. The configured scope comes from the application, not from a Web request.

## Request and record erasure

The application owns the request command, authorization check, pending event, result command, and query. Check the authenticated caller against the loaded entity before accepting a request. A caller-supplied reference must match a reference in that entity. Record a pending request before emitting an outbound action.

Use `Integration.outbound` with `Integration.Erasable.Erase`. Its `target` is an `ErasureTarget` containing exactly one reference, the owning `EntityName`, and the owning `StreamId`. Its `onResult` and `onFailure` functions produce application commands. The action uses the same event-to-config and output-to-command flow as other outbound integrations. It works with or without external OAuth2 token storage.

`onResult` receives `ErasedNow` when a key is deleted, `AlreadyErased` for a known tombstone, and `UnknownReference` when no key record exists. Handle `UnknownReference` separately from a successful deletion. Scope denial, malformed key data, and storage failures reach `onFailure`. A returned result is not application completion until its command commits a completion event. A runner's `Processed` log is not proof of that event.

If deletion commits but the result command fails or the process stops, the application request stays pending. Expose an explicit authorized retry command. The retry can receive `AlreadyErased` and then record completion. The framework does not automatically replay a lost result or maintain a recovery ledger for the application.

Keys created before application scopes were stored remain readable, but scoped erasure refuses them. Establish each key's application ownership before a separately reviewed scope migration. Backup, WAL, replica, and restore retention remain deployment responsibilities.
