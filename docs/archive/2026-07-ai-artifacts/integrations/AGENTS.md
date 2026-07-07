> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

# nhintegrations Knowledge Base

**Package:** nhintegrations — outbound integrations with external services.  
**Design:** ADR-0008, ADR-0015 (two-persona model).

## Structure

```text
integrations/
├── Integration/
│   ├── Http/               # Low-level HTTP client (retry, auth, response)
│   ├── Http.hs             # Jess-facing facade
│   ├── OpenRouter/         # LLM routing via OpenRouter
│   ├── OpenRouter.hs
│   ├── Agent/              # Provider-agnostic AI agent with tool calling
│   ├── Agent.hs
│   ├── Audio/              # Transcription (whisper-compatible)
│   ├── Brevo/              # Transactional email
│   ├── Brevo.hs
│   ├── Ocr/                # OCR via AI
│   ├── Oura/               # Oura ring health data
│   ├── Oura.hs
│   └── Pdf/                # PDF text extraction
└── test/Integration/       # One *Spec.hs per Internal module
```

## Two-Persona Model (load-bearing)

Every integration exposes **two layers**:

| Layer | Audience | What it contains |
|---|---|---|
| Facade (`Integration.Foo`) | **Jess** — integration user | Pure record config, smart constructors, zero HTTP/IO/Task visible |
| Internal (`Integration.Foo.Internal`) | **Nick** — integration developer | `ToAction` instance, retries, auth, error handling |

Jess calls `Integration.outbound myRequest` in her event handler — she never touches the Internal module. Nick implements the Internal module once and Jess's code never changes when implementation details evolve.

## Module layout per integration

```
Integration/
├── Foo.hs              # Re-exports: Request, smart constructors, Response
├── Foo/
│   ├── Request.hs      # Request record + sub-types
│   ├── Response.hs     # Response type
│   └── Internal.hs     # ToAction instance (Nick's code)
```

Sub-types (Auth, Retry, Body variants) go in their own files when they exceed ~60 lines.

## Usage pattern (Jess-side)

```haskell
import Integration qualified
import Integration.Brevo qualified as Brevo

cartIntegrations :: CartEntity -> CartEvent -> Integration.Outbound
cartIntegrations cart event = case event of
  OrderConfirmed info -> Integration.batch
    [ Brevo.send
        (Brevo.sender "noreply@myapp.com")
        (Brevo.recipient info.email)
        "Your order is confirmed"
        (Brevo.HtmlBody "<h1>Thank you</h1>")
        (\resp -> EmailSent { messageId = resp.messageId })
        (\err  -> EmailFailed { reason = err })
        |> Integration.outbound
    ]
  _ -> Integration.none
```

## WHERE TO LOOK

| Task | Location |
|---|---|
| Add HTTP call to external API | `Integration.Http` — use `Request`, `Auth`, `Retry` |
| Call an LLM | `Integration.OpenRouter` or `Integration.Agent` |
| Send transactional email | `Integration.Brevo` |
| Add a new integration | invoke `integration-pipeline-preview` skill |
| Implement `ToAction` | `Integration.Foo.Internal` — see existing Internal modules |

## Adding a new integration

Use the `integration-pipeline-preview` skill (17-phase pipeline: design → security/perf review → tests → impl → PR). State lives in `.integration-pipeline/`.

Manual checklist if not using the pipeline:
1. Create `Integration/Foo.hs` (facade — Jess's API)
2. Create `Integration/Foo/Request.hs`, `Response.hs`, `Internal.hs`
3. Implement `ToAction` in `Internal.hs`
4. Expose modules in `nhintegrations.cabal`
5. Add `Integration.Foo.*Spec` to the test suite in `nhintegrations.cabal`
6. Write tests under `test/Integration/Foo/`

## Build & test

```bash
cabal build nhintegrations          # build only
cabal test nhintegrations-test      # integration unit tests
```

## Secrets & environment variables

Never hardcode credentials. Use `${VAR_NAME}` template syntax in request fields — missing vars surface as `Integration.AuthenticationError` at runtime.

```haskell
auth = Http.ApiKey "X-Api-Key" "${SHIPPO_API_KEY}"
url  = "https://api.example.com/${API_VERSION}/orders"
```

Configure via `Config.field @(Redacted Text) "fooApiKey" |> Config.envVar "FOO_API_KEY" |> Config.secret`.

## Anti-patterns

| Instead of | Use |
|---|---|
| Calling `Http.Client` directly in a command | `Integration.outbound` in an Outbound handler |
| Hardcoding secrets | `${ENV_VAR}` interpolation + `Config.secret` |
| Cross-entity calls inside `Internal.hs` | Emit events; let Integration handle reactions |
| Putting business logic in `Internal.hs` | Business logic → command `decide`; `Internal.hs` only handles the HTTP mechanics |
