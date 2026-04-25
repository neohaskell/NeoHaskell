# ADR-0055: UUID v5 Support in Decision Monad

## Status
Proposed

## Context
### Current State
NeoHaskell currently provides `generateUuid` (v4, random) in the `Decision` monad for generating unique identifiers in command handlers. 

### Use Cases
- Deriving stable entity IDs from external identifiers (OAuth user IDs, merchant IDs).
- Ensuring idempotent UUID generation within Decision monad operations.

### Design Goals
1. Provide a deterministic UUID generation method.
2. Maintain consistency with the existing `Decision` monad API.
3. Ensure discoverability for command handler authors.

### GitHub Issue
- [#596: feat(Decider): add deterministic UUID generation (UUID v5) to Decision monad](https://github.com/neohaskell/NeoHaskell/issues/596)

## Decision
### 1. API Architecture
| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Monadic: `generateDeterministicUuid :: Uuid -> Text -> Decision Uuid` | **Chosen** | High discoverability. Command handlers authors find it next to `generateUuid`. Aligns with the monad as the "effects API". |
| Pure: `Uuid.fromNamespace :: Uuid -> Text -> Uuid` | Rejected | Low discoverability. Requires knowledge of utility APIs outside the command handler context. |

### 2. Type Definitions
```haskell
-- No new types required, uses existing Uuid and Text.
```

### 3. Public API
```haskell
-- | Generate a deterministic UUID (v5) from a namespace and a name.
-- Same inputs always produce the same UUID.
--
-- SECURITY WARNING: Do NOT use this for sensitive data like passwords or tokens.
-- The deterministic nature means anyone with access to the inputs can reproduce the ID.
generateDeterministicUuid :: Uuid -> Text -> Decision Uuid
```

## Consequences
### Positive
- UUID v5 is discoverable by command handler authors without external documentation.
- Consistent API for all UUID generation in command handlers.

### Negative
- Introduces a monadic wrapper around pure, deterministic logic.

### Risks
- Minor API surface bloat in `Decider` module.

### Mitigations
- Document the deterministic nature clearly in the docstring.
- Explicitly warn against using sensitive data as input.

## Implementation Details
### Example Usage
```haskell
decide cmd entity _ctx = do
  -- Derive entity ID from external provider ID
  entityId <- generateDeterministicUuid oauthNamespace cmd.externalId
  acceptNew [EntityCreated { entityId = entityId }]
```

## References
- RFC 4122 §4.3 - UUID Version 5 (SHA-1).
- Note: UUID v5 uses SHA-1 for hashing. While RFC-compliant, it is intended for unique identification, not for cryptographic security where collision resistance against malicious actors is required.
- Data.UUID.V5 from the `uuid` package.
