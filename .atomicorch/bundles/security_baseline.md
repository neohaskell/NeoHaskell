---
id: security_baseline
target_max_tokens: 3000
---

# NeoHaskell Security Baseline

Every code change touching authentication, authorization, data handling, or external interfaces must be evaluated against this checklist. Security findings block merge.

---

## 7 Security Categories

### 1. Sensitive Data Handling

- [ ] No plaintext passwords, API keys, or tokens in source code or logs
- [ ] No sensitive fields in `Show` instances (use custom `Show` or `Redacted` wrapper)
- [ ] Secrets loaded from environment variables or secret store, never from config files committed to git
- [ ] PII fields annotated with `Sensitive` newtype wrapper
- [ ] Log output scrubbed of sensitive fields before emission

```haskell
-- CORRECT: Redact sensitive fields
newtype Password = Password Text
instance Show Password where
  show _ = "<redacted>"
```

### 2. Authorization

- [ ] Every endpoint or command handler checks authorization before processing
- [ ] Authorization checks happen on the server side — never trust client-supplied role claims
- [ ] Resource ownership validated: user can only access their own resources unless explicitly granted
- [ ] Admin operations gated behind explicit `AdminRole` check, not just authenticated user

### 3. Input Validation

- [ ] All external input (HTTP request bodies, query params, headers, event payloads) validated before use
- [ ] String length limits enforced (no unbounded Text inputs)
- [ ] Numeric range checks for IDs and quantities
- [ ] Enum/union values validated against known set (reject unknown constructors from JSON)
- [ ] File paths sanitized; no path traversal possible

### 4. Error Messages

- [ ] Error messages returned to clients do not leak internal structure (stack traces, module names, SQL)
- [ ] Distinguish user-facing errors from internal errors at the type level
- [ ] Validation errors provide enough context to fix the problem without exposing internals

```haskell
-- CORRECT: Separate user-facing from internal errors
data ApiError
  = ValidationFailed Text   -- safe to return to client
  | InternalError           -- never expose details

data InternalError
  = DatabaseError Text      -- log only, never return
  | ServiceTimeout          -- log only, never return
```

### 5. Cryptography

- [ ] No custom crypto — use `nhcore-crypto` wrappers only
- [ ] Password hashing uses Argon2id (never MD5, SHA1, SHA256 for passwords)
- [ ] JWT tokens signed with RS256 or EdDSA (never HS256 with shared secret)
- [ ] Tokens have explicit expiry; no infinite-lived tokens
- [ ] Cryptographic comparisons use constant-time equality

### 6. SQL and Data Store

- [ ] All queries use parameterized statements — no string interpolation in SQL
- [ ] No raw `[fmt|SELECT * FROM users WHERE id = {userId}|]` pattern in SQL context
- [ ] Migrations reviewed for data loss risk
- [ ] Eventstore entries validated against schema before persistence

### 7. Unsafe Haskell

- [ ] No `unsafePerformIO` outside of explicitly approved FFI wrappers
- [ ] No `unsafeCoerce` except in nhcore internals with documented justification
- [ ] No `Foreign.Ptr` usage in application code
- [ ] No `-XDangerous` extensions in application packages

---

## Red Lines

These findings automatically block merge (no exceptions without security team sign-off):

| Red Line | Reason |
|----------|--------|
| Plaintext secret in any committed file | Immediate credential rotation required |
| Authorization bypass (any endpoint) | Critical severity |
| SQL injection vector | Critical severity |
| `unsafePerformIO` in application code | Undefined behavior risk |
| MD5 or SHA1 for password hashing | Cryptographically broken |
| Unbounded input accepted from external source | DoS vector |

---

## Findings Output Template

When reporting security findings, use this YAML structure:

```yaml
findings:
  - id: SEC-001
    category: sensitive_data
    severity: critical  # critical | high | medium | low | info
    location: core/auth/Auth.hs:42
    description: |
      Password field included in derived Show instance.
      Logging a User value will expose the plaintext password.
    remediation: |
      Implement custom Show for Password that returns "<redacted>".
    blocking: true

  - id: SEC-002
    category: authorization
    severity: high
    location: core/service/Service/Command/UpdateProfile.hs:17
    description: |
      Command handler does not verify that the requesting user
      owns the profile being updated.
    remediation: |
      Add ownership check: require cmd.requesterId == profile.ownerId
      before processing the command.
    blocking: true
```

---

## Threat Model Summary

NeoHaskell services face these primary threat categories:

1. **Unauthenticated access** — endpoints exposed without auth
2. **Privilege escalation** — normal user performing admin actions
3. **Data exfiltration** — access to other users' data
4. **Injection** — SQL, command, or template injection via user input
5. **Credential theft** — exposed secrets, weak hashing, insecure token storage
6. **DoS** — unbounded inputs causing resource exhaustion
