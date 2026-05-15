# nhcore context for reviewers

Compiler flags and framework conventions that change what a review must (and must not) flag.

## Contents

- [Toolchain invocation](#toolchain-invocation)
- [Global compiler flags](#global-compiler-flags)
- [Custom Prelude](#custom-prelude)
- [Framework-provided defaults](#framework-provided-defaults)
- [Red-flag-but-already-handled list](#red-flag-but-already-handled-list)

## Toolchain invocation

Every Haskell-toolchain command (`cabal`, `hlint`, `ghc`, `hspec-discover`, etc.) must be invoked through the repo's nix dev shell. The canonical form is:

```
nix develop --command <command> <args...>
```

Examples used by the pipeline's leaves:

- `nix develop --command cabal build all`
- `nix develop --command cabal test --test-show-details=streaming`
- `nix develop --command cabal test`
- `nix develop --command hlint <files...>`

Bare `cabal ...` / `hlint ...` calls will fail outside the dev shell because the toolchain is pinned by `flake.nix`. System-level tools (`git`, `gh`, `python3`) are not wrapped — they live on the user's PATH and do not depend on the dev shell. Refusal: if `nix` is not on PATH, the leaf cannot proceed and must surface the missing binary rather than fall back to system `cabal`.

## Global compiler flags

- `Strict` / `StrictData` are on globally. All let-bindings, function args, and data-constructor fields are strict by default. Do not request `!` on fields; do not advise "use strict fields"; do not flag `foldl` as lazy (nhcore's `foldl` IS `foldl'`).
- `NoImplicitPrelude` is on. Every module imports from nhcore. Bare `Prelude.foldl`, raw `[]`, raw `String` cannot appear by accident — flag them when they do.
- `-funbox-strict-fields` is on. `{-# UNPACK #-}` on a strict field of a single-constructor type is redundant. Only request `UNPACK` for sum-type fields or sub-word coalescing per [perf §2](./performance-methodology.md#2-unpack-with-strict-fields).

## Custom Prelude

- `Text` (not `String`).
- `Array a` (not `[]`).
- `Result error value` (not `Either`).
- `Task err val` (not `IO`).
- `Result.ok` / `Task.yield` (not `pure` / `return`).
- `|>` for piping (not `$` / nesting).
- `[fmt|...|]` for interpolation (not `<>` / `++`).
- `do` + `let` for bindings (no `let..in`, no `where`).
- `case ... of` for pattern matching.

## Framework-provided defaults

- **Wrap `Integration.Http` by default.** If the integration's outbound shape is HTTP request/response over HTTPS (no WebSocket, no Server-Sent Events, no long-lived socket, no background daemon, no scheduled poller, no non-HTTP transport), the integration **MUST** wrap `Integration.Http` rather than hand-roll an HTTP client. This covers ~95% of outbound integrations (REST/JSON APIs, GraphQL POST-only, webhooks-out). The canonical layout — used by `Integration.Brevo`, `Integration.OpenRouter`, `Integration.Oura` — is four files:
    - `integrations/Integration/<Module>.hs` — re-export shell.
    - `integrations/Integration/<Module>/Request.hs` — Jess-facing `Request`, smart constructors, body/auth-shape types.
    - `integrations/Integration/<Module>/Response.hs` — `Response` and helper types with JSON instances.
    - `integrations/Integration/<Module>/Internal.hs` — `toHttpRequest :: ... -> Http.Request command`, response dispatch, and the `ToAction (Request command)` instance so `Integration.outbound brevoReq` works directly.

  Wrapping inherits `Integration.Http`'s retry, timeout, `Auth` redaction, env-var expansion (legacy), and `ToAction` machinery — re-implementing any of these in a new integration is a design-phase refusal. Departures from this default are allowed **only** when one of the following is concretely true and named in the design's `## Decision drivers`: the integration uses WebSocket / SSE / chunked-streaming response, holds a long-lived TCP/gRPC connection, runs a background polling daemon or scheduled timer, or speaks a non-HTTP wire protocol (raw TCP, UDP, IPC, file-system notify). A vague "for performance" or "for flexibility" is not a valid departure reason.

- `Redacted` wrapper for secret values; hand-written `Show` printing `<redacted>` is the established pattern for secret newtypes (e.g. `ClientSecret`, `AccessToken`, `RefreshToken`, `HmacKey`).
- **Config-sourced secrets, never env-var reads in integration code.** Every secret an integration needs (API keys, OAuth tokens, webhook signing secrets, bearers) is declared as a `Redacted Text` field in the user's project `Config.hs` via the `Config.field` / `Config.required` / `Config.envVar` / `Config.secret` DSL. The framework loads it once at startup, fails fast if missing, and exposes it via the implicit `?config` parameter. Integration code reads `?config.<fieldName>` and carries the value as `Redacted Text` end-to-end. **Integrations MUST NOT read environment variables themselves**, MUST NOT accept raw `Text` keys in their public API, and MUST NOT use the legacy `"${VAR}"` literal-expansion pattern (some older integrations like `Integration.OpenRouter` still do — those are tech debt awaiting migration, not a pattern to emulate). Canonical user-side declaration:

  ```haskell
  Config.field @(Redacted Text) "<integrationName>ApiKey"
    |> Config.doc "<one-line purpose>"
    |> Config.required
    |> Config.envVar "<UPPER_SNAKE_ENV_VAR>"
    |> Config.secret
  ```

  Canonical integration-side `send`-style signature:

  ```haskell
  send ::
    ( ?config :: config
    , HasField "<integrationName>ApiKey" config (Redacted Text)
    ) =>
    ... ->
    Request command
  ```

- `canAccess` / `canView` two-phase authorization is a compile-time requirement for every query.
- `RequestContext` threading is compile-time-enforced for every command.
- `constEq` is the constant-time comparison primitive, with a mandatory `{-# INLINE constEq #-}` pragma.
- Postgres EventStore uses Hasql's typed `Statement` + `Encoders` + `Decoders` — string concatenation is unrepresentable.
- `Crypto.Random.getRandomBytes` is the only sanctioned CSPRNG source for security-sensitive random.

## Red-flag-but-already-handled list

A grounding pass demotes any finding that names one of these conditions, because the framework already handles it:

- "Strict fields are missing" → `Strict` covers it; demote.
- "Use `foldl'` instead of `foldl`" → nhcore's `foldl` is strict; demote.
- "SQL string concatenation risk" → Hasql forbids it; demote unless the diff bypasses Hasql.
- "Lazy `String` in hot path" → nhcore exports `Text`, not `String`; demote unless `String` appears.
- "Missing `pure`/`return` discipline" → `Task.yield`/`Result.ok` are the framework calls; demote.
- "Secret stored as plain `Text`" → if the integration consumes the value via `?config.<field>` declared as `@(Redacted Text)`, the framework handles redaction; demote. Only kept-as-blocker when the diff actually unwraps `Redacted` somewhere other than the single audited site building the outbound request header.
