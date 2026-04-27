# ADR-0055: Deterministic UUID (v5) Support in Decision Monad

## Status

Accepted (merged 2026-04-27)

## Context

### Current State

The `Decision` monad provides `generateUuid` which uses UUID v4 (random) internally via the `genUuid` capability in `DecisionContext`. While suitable for most entity IDs, certain domain patterns require **deterministic identifiers** derived from stable inputs (e.g., mapping an external OAuth `sub` claim or a unique email to a system-internal UUID).

Currently, NeoHaskell lacks a first-class way to perform this mapping within a command handler's `decide` function. Pushing this responsibility to the caller or pre-computing IDs outside the monad breaks the encapsulation of the decision logic and forces the framework user to handle ID mapping manually.

### Use Cases

- **External Identity Mapping**: Jess is integrating an OAuth provider. She needs to derive a stable `UserId` from the provider's text identifier. Using `generateDeterministicUuid` ensures the same provider ID always maps to the same internal UUID.
- **Idempotent Imports**: When importing data from a legacy system, deterministic IDs allow re-running the import without creating duplicate entities, as the IDs derived from legacy keys will collide intentionally.
- **Stable References**: Creating reproducible IDs for singleton entities or well-known system resources based on their name.

### Design Goals

1. **Discoverability**: Authors working in the `Decision` monad should find deterministic ID generation next to random generation.
2. **Standard Compliance**: Follow RFC 4122 §4.3 (UUID v5) using SHA-1 hashing.
3. **Ergonomics**: Provide a pipe-friendly API that integrates naturally with `do`-notation in command handlers.
4. **Safety**: Explicitly document the reproducibility risks of deterministic IDs to prevent misuse with sensitive data.

### GitHub Issue

- [#596: feat(Decider): add deterministic UUID generation (UUID v5) to Decision monad](https://github.com/neohaskell/NeoHaskell/issues/596)

## Decision

### 1. API Architecture: Monadic vs. Pure

We will implement the deterministic UUID generation as a function within the `Decision` monad.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| **Monadic**: `generateDeterministicUuid :: Uuid -> Text -> Decision Uuid` | **Chosen** | High discoverability. Command handler authors find it next to `generateUuid`. Aligns with the monad as the "effects API" for commands. |
| **Pure**: `Uuid.fromNamespace :: Uuid -> Text -> Uuid` | Rejected | Low discoverability. Requires knowledge of utility APIs outside the command handler context. Pushes framework users to search documentation for common patterns. |

### 2. Implementation in `Decider`

The function will be added to the `Decider` module. It performs the hashing and wraps the result in `Return`.

```haskell
-- | Generate a deterministic UUID (v5) from a namespace and a name.
-- Same inputs always produce the same UUID.
-- See SECURITY WARNING in Uuid.generateV5.
generateDeterministicUuid :: Uuid -> Text -> Decision Uuid
generateDeterministicUuid namespace name =
  Uuid.generateV5 namespace name |> Return
```

Note: An `{-# INLINE generateDeterministicUuid #-}` pragma follows the function body to allow GHC to eliminate the `Return` wrapper during fusion.

### 3. Module Placement

| Change | File |
|--------|------|
| `generateDeterministicUuid` implementation | `core/service/Decider.hs` |
| `Data.UUID.V5` & `Data.Text.Encoding` imports | `core/service/Decider.hs` |
| New test suite | `core/test/DeciderSpec.hs` |
| Registration | `core/nhcore.cabal` |

## Consequences

### Positive

- **Natural Discovery**: Jess sees `generateDeterministicUuid` in autocomplete right next to `generateUuid`.
- **Consistency**: All ID generation logic for commands is cohesive within the `Decision` monad API.
- **Idempotency**: Simplifies the implementation of idempotent command handlers.

### Negative

- **Semantic Lie**: Wraps pure, deterministic logic in a monadic constructor (`Decision`), which may confuse users seeking the purest Haskell representation.
- **API Surface**: Adds one more function to the core `Decider` surface.

### Risks

- **Information Leakage**: Users might derive UUIDs from sensitive data (tokens/passwords) without realizing the IDs are reproducible.
- **SHA-1 Collision**: UUID v5 uses SHA-1. While acceptable for identification per RFC 4122, it is not suitable for high-security collision resistance against adversaries.

### Mitigations

- **Security Warning**: The docstring contains an explicit warning against using sensitive inputs.
- **Naming**: The inclusion of "Deterministic" in the name alerts the user to its behavior compared to `generateUuid`.

## References

- [RFC 4122: A Universally Unique IDentifier (UUID) URN Namespace](https://www.ietf.org/rfc/rfc4122.txt)
- [Data.UUID.V5 Documentation](https://hackage.haskell.org/package/uuid-1.3.15/docs/Data-UUID-V5.html)
- [ADR-0003: Command Abstraction and Flow](0003-command-abstraction-and-flow.md)
