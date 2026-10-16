# ADR-0082: Own local OAuth refresh-failure test endpoints

## Status

Proposed

## Context

SyncAll error tests inject Unauthorized resource results but retain a refresh token. The production refresh action therefore sends a real token request. Their external example.com token endpoint is not a mock and makes full-suite execution unreliable and dependent on external network access.

## Decision

Use a bracketed WAI/Warp server inside each SyncAll test. Bind only127.0.0.1 and let the OS assign the port. Return HTTP400 JSON invalid_grant for POST /oauth/token. Preserve stored refresh tokens and existing callback/AuthenticationError expectations; assert one observed refresh request in each unauthorized scenario and zero in success scenarios. Keep unsafeValidatedProvider confined to synthetic test configuration. Do not weaken production endpoint validation.

Add wai, warp and http-types to nhintegrations-test build-depends. These packages are already used by core tests in the pinned dependency closure. Do not add a flake input, external service, mutable download or production dependency. Validate the actual pinned helper signature before implementation.

## Consequences

The tests exercise real local HTTP refresh failures without a public endpoint. Bracketed server lifetime and readiness remove fixed-port collisions and sleep races. The test dependency declaration grows; production API and runtime dependencies do not change. Full integration-suite execution is still required, not replaced by fixture wiring checks.

## References

- [Change025](../changes/025-integrations-execution-gate.md)
- [ADR-0069: local security reviews](0069-security-reviews-are-local.md)
