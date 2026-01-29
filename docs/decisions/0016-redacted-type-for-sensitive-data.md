# ADR-0016: Redacted Type for Sensitive Data

## Status

Proposed

## Context

A security review of the OpenRouter integration identified that sensitive data can leak through `Show` instances into logs, error messages, and debug output. This poses compliance and security risks:

### Current State

1. **Manual redaction in `Auth.OAuth2.Types.hs`**: The OAuth2 module manually implements `Show` instances that redact sensitive values:
   ```haskell
   instance Show ClientSecret where
     show _ = "ClientSecret <REDACTED>"
   
   instance Show AccessToken where
     show _ = "AccessToken <REDACTED>"
   ```

2. **Unprotected user content in `Integration.OpenRouter.Message.hs`**: The `Message` type uses `deriving (Show)`, which exposes user-provided content:
   ```haskell
   data Message = Message
     { role :: Role
     , content :: Text  -- User messages visible in logs!
     }
     deriving (Show, Eq, Generic)
   ```

### Problems

1. **GDPR/Compliance Risk**: User messages sent to AI models may contain PII. Logging these messages violates data protection regulations.

2. **Security Exposure**: API keys, tokens, and passwords can appear in error messages, stack traces, or debug logs if wrapped in types that derive `Show`.

3. **Inconsistent Protection**: Each sensitive type requires a manual `Show` instance. Developers must remember to do this, and the pattern varies across modules (`<REDACTED>`, `***`, `<redacted>`).

4. **No Compiler Help**: Using `deriving (Show)` on a type containing sensitive fields compiles without warning.

### Design Goals

1. **Zero-effort for Jess**: Wrapping a value should be trivial
2. **Type-safe**: The compiler should help prevent accidental exposure
3. **Descriptive**: Redacted output should indicate WHAT was redacted (optionally)
4. **Explicit unwrapping**: Accessing the raw value requires conscious decision
5. **NeoHaskell style**: Pipe operator friendly, no Haskell-isms

## Decision

### 1. Type Name: `Redacted`

We choose `Redacted` over alternatives:

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| `Sensible` | Rejected | Clever pun on "sensitive" but violates Least Astonishment. Newcomers may think it means "reasonable." |
| `Secret` | Rejected | Conflicts with Haskell ecosystem (`vault`, `servant-auth`, `secret-sharing`). Also implies cryptographic protection we don't provide. |
| `Redacted` | **Chosen** | Self-documenting, familiar to security-conscious developers, no ecosystem conflicts, searchable. |

**Naming test results:**
- "What IS this?" → A value that shows as redacted in logs ✓
- "What would a TS dev call it?" → Familiar from logging/security contexts ✓
- "What if we had 3?" → `Redacted ApiKey`, `Redacted Text` — clear ✓
- "Conflicts?" → None in nhcore or common Haskell packages ✓

### 2. Module Location: `core/core/Redacted.hs`

`Redacted` is a fundamental type like `Maybe` or `Result`:
- Re-exported from `Core.hs` for universal availability
- Not security-specific (applies to PII, secrets, any loggable sensitive data)
- Creating a `Security` module would be over-engineering for a single type

### 3. Type Design

```haskell
-- | A value that shows as @<redacted>@ in Show output.
data Redacted value
  = Redacted value
  | RedactedLabeled Text value
```

Two constructors:
- `Redacted value`: Shows as `<redacted>` — most common case
- `RedactedLabeled label value`: Shows as `<redacted: label>` — for debugging complex structures

### 4. API Design

```haskell
-- Construction
wrap :: forall value. value -> Redacted value
labeled :: forall value. Text -> value -> Redacted value

-- Access
unwrap :: forall value. Redacted value -> value
```

**Accessor naming rationale:**
- `unwrap` matches existing nhcore patterns: `unwrapAccessToken`, `unwrapClientSecret`, `unwrapRefreshToken` in `Auth.OAuth2.Types`
- Neutral connotation (unlike `unsafe*` which implies danger, or `reveal` which is dramatic)
- Works well with pipes: `apiKey |> Redacted.unwrap |> sendToApi`

### 5. Instance Decisions

| Instance | Decision | Rationale |
|----------|----------|-----------|
| `Show` | **Yes** — displays `<redacted>` or `<redacted: label>` | Core purpose of the type |
| `Eq` | **No** | Comparing secrets can leak information via timing attacks. If needed, use `Redacted.unwrap a == Redacted.unwrap b` to make the security decision explicit. |
| `ToJSON` | **No** | Prevents accidental serialization to logs/APIs — the exact problem we're solving. If needed, serialize `Redacted.unwrap value` explicitly. |
| `FromJSON` | **Yes** | Safe because parsed values become protected immediately. Follows existing pattern in `Auth.OAuth2.Types`. |
| `Generic` | **No** | Would allow deriving `ToJSON`/`FromJSON`, defeating the safety purpose. |

### 6. Show Output Format

```haskell
instance Show (Redacted value) where
  show redacted =
    case redacted of
      Redacted _ -> "<redacted>"
      RedactedLabeled label _ -> "<redacted: " ++ Text.toLinkedList label ++ ">"
```

Format chosen for:
- Simplicity (type signature already shows what's inside)
- Consistency with existing `<REDACTED>` patterns in codebase
- No `Typeable` constraint required

### 7. Full Module Implementation

```haskell
-- | Wrapper type that redacts values in Show output.
--
-- Use 'Redacted' to prevent sensitive data from appearing in logs,
-- error messages, or anywhere that uses 'Show'.
--
-- == Quick Start
--
-- @
-- import Redacted (Redacted)
-- import Redacted qualified
--
-- -- Wrap sensitive values
-- let apiKey = Redacted.wrap "sk-xxx-secret"
-- let userMessage = Redacted.wrap userInput
--
-- -- Safe to log
-- log [fmt|Processing request with key: #{apiKey}|]
-- -- Output: Processing request with key: <redacted>
--
-- -- Explicit unwrapping required for use
-- sendToApi (Redacted.unwrap apiKey)
-- @
module Redacted
  ( Redacted
  , wrap
  , labeled
  , unwrap
  ) where
```

### 8. Core.hs Integration

Add to `Core.hs` re-exports:
```haskell
import Redacted as Reexported (Redacted)
```

## Consequences

### Positive

1. **Type-safe sensitive data handling**: Wrapping values in `Redacted` prevents accidental exposure through `Show`. The compiler enforces protection.

2. **Zero-effort for Jess**: Simple API — just `Redacted.wrap value` to protect, `Redacted.unwrap value` to access.

3. **GDPR/Compliance support**: User content, PII, and secrets can be safely logged without manual redaction at every log site.

4. **Consistent redaction format**: All sensitive data shows as `<redacted>` or `<redacted: label>`, replacing the inconsistent patterns (`***`, `<REDACTED>`, etc.) across the codebase.

5. **Explicit security decisions**: No `Eq` or `ToJSON` forces developers to consciously unwrap before comparing or serializing — making security trade-offs visible in code.

6. **Debugging support**: The `labeled` variant allows seeing WHAT is redacted in complex structures without exposing the actual value.

7. **NeoHaskell style compliant**: Uses qualified module design (`Redacted.wrap`, `Redacted.unwrap`), works with pipe operator.

### Negative

1. **Additional wrapping ceremony**: Fields that were previously `Text` become `Redacted Text`, requiring changes at construction and access sites.

2. **No `Eq` may surprise**: Developers expecting to compare `Redacted` values will get a compile error. The workaround (`unwrap` both) is intentional but requires explanation.

3. **Labeled variant adds complexity**: Two constructors instead of one. However, the simple `wrap` covers 90% of cases.

### Trade-offs

1. **No Typeable-based Show**: We chose `<redacted>` over `<redacted: ApiKey>` to avoid adding `Typeable` constraints everywhere. The `labeled` variant provides this when needed.

2. **Existing types unchanged**: `ClientSecret`, `AccessToken`, etc. in `Auth.OAuth2.Types` remain as-is. They're domain-specific types with validation and semantics beyond just redaction. `Redacted` is for wrapping arbitrary values.

3. **No runtime protection**: `Redacted` only affects `Show`. Code with access to `unwrap` can still misuse the value. This is a compile-time documentation/safety tool, not cryptographic protection.

### Migration Path

1. **New code**: Use `Redacted Text` for sensitive fields (user content, API keys, etc.)

2. **Existing sensitive types**: Keep `ClientSecret`, `AccessToken`, etc. unchanged — they already have manual redaction and domain-specific semantics.

3. **OpenRouter Message**: Change `content :: Text` to `content :: Redacted Text` to protect user messages.

4. **Integration Auth**: The `Http.Auth` type can optionally wrap credentials in `Redacted`, though environment variable expansion means the actual secrets aren't stored directly.

## References

- [Auth.OAuth2.Types.hs](../../core/auth/Auth/OAuth2/Types.hs) — Existing manual redaction patterns
- [Integration.OpenRouter.Message.hs](../../integrations/Integration/OpenRouter/Message.hs) — Example of unprotected user content
- [Integration.Http.Auth.hs](../../integrations/Integration/Http/Auth.hs) — Existing Auth redaction
- [ADR-0015: HTTP Outbound Integration](0015-http-outbound-integration.md) — Related security context
- GDPR Article 5(1)(f) — Integrity and confidentiality principle
- OWASP Logging Cheat Sheet — Sensitive data handling in logs
