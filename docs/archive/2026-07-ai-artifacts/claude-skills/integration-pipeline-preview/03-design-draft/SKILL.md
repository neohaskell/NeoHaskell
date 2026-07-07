> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 03-design-draft
description: Drafts the outbound integration's design document at .integration-pipeline/integration-design.md in Status Proposed, then opens a draft PR carrying the design in its body so the maintainer can review it on GitHub.
kind: leaf
executor: opus
model: claude-opus-4-7
---

# Integration design draft

Produces `.integration-pipeline/integration-design.md` matching the integration design template, with Status: Proposed, then opens a **draft PR** so the maintainer reviews the design on GitHub. The design doc is the input to every later phase (security, perf, devex, architecture, test spec); the draft PR is converted to ready for review at the PR gate (phase 16) — this phase never opens a second PR.

**This pipeline never writes to `docs/decisions/`.** Repo-level ADRs are reserved for repo-level architecture; an outbound integration is scoped to be portable to a separate repo, and an ADR slot here would orphan if/when that move happens. The design doc therefore lives in the per-checkout `.integration-pipeline/` directory (which is gitignored) and travels with the integration's source.

**The design doc is never committed.** Because `.integration-pipeline/` is gitignored — the portability invariant — the draft PR cannot carry the design as a committed file the way the feature pipeline commits its ADR. Instead the draft PR is seeded with a single empty commit (which a squash-merge erases) and **carries the full design doc verbatim in the PR body**, so a remote reviewer sees the whole design in the PR description. Later phases push the real implementation commits onto the same branch.

## Inputs

- `integration_name` — string, from `pipeline.py get integration_name`.
- `issue_number` — string, from `pipeline.py get issue_number` (may be empty).
- `module_name` — string, Pascal-case suffix (e.g. `Stripe`), from `pipeline.py get module_name`.
- `module_path` — string, from `pipeline.py get module_path` (default `integrations/Integration/<module_name>.hs`).
- `design_path` — string, from `pipeline.py get design_path` (always `.integration-pipeline/integration-design.md`).
- `branch_name` — string, from `pipeline.py get branch_name`. The integration branch created in phase 1; the draft PR is opened from it.

## Plan

1. Read pipeline variables → verify: `integration_name` and `design_path` non-empty; `design_path` starts with `.integration-pipeline/`.
2. Refuse if `design_path` already exists with content — re-runs after maintainer feedback go through `pipeline.py reset` or an explicit overwrite gesture.
3. Draft the doc with every required section, every code example following the NeoHaskell style guide → verify: file written and contains `Status: Proposed`.
4. Open the draft PR carrying the design in its body → verify: `pipeline.py get pr_url` returns a URL.
5. Mark phase 3 complete → verify: `pipeline.py status` shows phase 3 awaiting approval.

Assumptions:
- The `.integration-pipeline/` directory exists (phase 1 created it).
- The integration branch (`branch_name`) was created in phase 1; at this point the working tree has no committed changes (the design doc is gitignored).
- `gh` is on PATH and the repository supports draft PRs.
- The maintainer will review the design in the draft PR before approving phase 3.
- Implementation lives under `integrations/Integration/<module_name>/` and `integrations/Integration/<module_name>.hs`.

If any assumption fails (style guide unclear, design template missing, `gh` absent), refuse and ask.

## Required sections

The draft must contain every section listed below. Wording mirrors the NeoHaskell ADR template so reviewers familiar with feature ADRs see no surprise, but the doc is **not** an ADR and must not claim a `docs/decisions/` number.

1. `# <integration_name> integration design` — title.
2. `Status: Proposed` — single line, on its own.
3. `## Context` — what external system this integrates with, why now, what user need it serves, the link to issue `#<issue_number>` if present.
4. `## Decision drivers` — bulleted list of forces driving the design (deadline, security, perf budget, Jess affordance).
5. `## Considered options` — at least two alternatives, with one-paragraph trade-off each. **If the integration is HTTP request/response (no streaming, no long-lived socket, no daemon, no non-HTTP transport), one of the considered options MUST be "wrap `Integration.Http` with a smart-constructor + `Request`/`Response`/`Internal` layout" (the Brevo / OpenRouter / Oura pattern), and that option MUST win the trade-off unless the design names a concrete blocker (streaming response, persistent connection, scheduled poller, non-HTTP wire). "For flexibility" or "for performance" is not a valid blocker — name the technical reason.**
6. `## Decision outcome` — the chosen option, restated in one paragraph. If the chosen option is anything other than "wrap `Integration.Http`", `## Decision drivers` must contain a line naming the specific blocker (streaming / persistent connection / daemon / non-HTTP wire). See [`../references/nhcore-context.md` "Framework-provided defaults"](../references/nhcore-context.md) for the canonical wrap-Http layout.
7. `## Trust boundary` — what crosses the network, what secrets are needed, where they are sourced, what error modes the caller sees. **Secrets MUST be sourced from the user's project `Config.hs`** via the `Config.field @(Redacted Text) "<integrationName>ApiKey" |> Config.required |> Config.envVar "<UPPER_SNAKE>" |> Config.secret` DSL, threaded through the integration's `send`-style smart constructor with a `(?config :: config, HasField "<fieldName>" config (Redacted Text))` constraint, and carried as `Redacted Text` end-to-end. The integration MUST NOT read environment variables itself (no `System.Environment.getEnv`, no `Env.lookupEnv`, no `"${VAR}"` literal-expansion pattern), MUST NOT accept raw `Text` keys in its public API, and MUST have exactly one `Redacted.unwrap` call site — the line inside `<Module>.Internal` that constructs the outbound `Http.ApiKey` / `Http.Bearer` / `Http.Basic` header tuple. See [`../references/security-methodology.md` §8a](../references/security-methodology.md) for the rule and [`../references/nhcore-context.md` "Framework-provided defaults"](../references/nhcore-context.md) for the canonical user-side declaration.
8. `## Public API` — every public function, type, and error the integration exports, with full signatures and a one-line doc-by-example for each. This is the surface the DevEx review (phase 6) inspects.
9. `## Out of scope` — adjacent concerns explicitly excluded (e.g. inbound webhooks if this is request/response only).
10. `## Consequences` — positive and negative downstream effects.
11. `## Portability note` — one short paragraph confirming that this integration is built to be portable to a separate repo and that no design artefact has been written to `docs/decisions/` or `docs/architecture/`.

## Steps

1. Compute the target path from `pipeline.py get design_path`. Refuse if the file exists and is non-empty.
2. Draft each section — keep code examples short, name only the public API the integration introduces, reference `#<issue_number>` once at the top of `## Context`.
3. Apply the NeoHaskell style to every code example: pipes, do-blocks, `case ... of`, `Task`/`Result`, no `let..in`, no `where`, no `$`, no single-letter type params. Refuse if the chosen API forces any of these.
4. Apply the Jess persona check (`../references/jess-persona.md`): every public function reachable from the `## Public API` block should be usable in 15 minutes from autocomplete alone. Refuse the design if it is not.
5. **Apply the secret-sourcing check.** If the integration needs any secret (API key, OAuth token, webhook signing secret, bearer), verify the draft (a) declares the canonical `Config.field @(Redacted Text) "<integrationName>ApiKey" |> Config.required |> Config.envVar "<UPPER_SNAKE>" |> Config.secret` snippet in `## Trust boundary`, (b) types the `send` smart constructor with a `(?config :: config, HasField "<fieldName>" config (Redacted Text))` constraint, (c) carries the value as `Redacted Text` through the `Request` record, (d) names exactly one `Redacted.unwrap` site (inside `<Module>.Internal.toHttpRequest` or equivalent). Refuse the draft if any of these are missing or if the draft proposes any of: `System.Environment.getEnv`, `Env.lookupEnv`, `"${VAR}"` literal-expansion, or a raw `Text` secret in the public API. See [`../references/security-methodology.md` §8a](../references/security-methodology.md).

   **Apply the wrap-Http default check.** If the integration's outbound shape is HTTP request/response over HTTPS — no WebSocket, no Server-Sent Events, no long-lived TCP/gRPC connection, no background polling daemon, no scheduled timer, no non-HTTP wire protocol — the draft MUST propose wrapping `Integration.Http` (the Brevo / OpenRouter / Oura pattern: `<Module>.hs` re-export shell + `<Module>/Request.hs` + `<Module>/Response.hs` + `<Module>/Internal.hs` with `toHttpRequest` and a `ToAction (Request command)` instance). Refuse the draft if (a) it hand-rolls an HTTP client, (b) it reaches for `http-client` / `wreq` / `req` / `servant-client` / etc. directly, or (c) `## Considered options` does not include a wrap-Http alternative. Departures are allowed only when `## Decision drivers` names a concrete blocker (streaming response, persistent connection, scheduled poller, non-HTTP wire) — "for flexibility" or "for performance" without a measured cost is not a valid blocker. See [`../references/nhcore-context.md` "Framework-provided defaults"](../references/nhcore-context.md).
6. Verify the `## Portability note` section is present and does not reference `docs/decisions/` or `docs/architecture/` as a target.
7. Write the file with `Status: Proposed`.
8. Open the draft PR so the maintainer can review the design on GitHub. **The design doc is never committed** — it is gitignored under `.integration-pipeline/` — so it rides in the PR body instead:
   1. `BRANCH=$(git branch --show-current)`. If `$BRANCH = main`, refuse: "cannot open a PR from main".
   2. `command -v gh >/dev/null 2>&1` — if non-zero, refuse: "gh not found".
   3. Confirm the design doc will NOT be committed: `git status --porcelain | grep -q '^.. \.integration-pipeline/'` should be empty (the state dir's `.gitignore` handles this). If any `.integration-pipeline/` path is stage-able, refuse: "pipeline state must never be committed".
   4. Seed the branch with a single empty commit so the PR has something to open against (a squash-merge erases it): `git commit --allow-empty -m "chore(<slug>): open design-review PR (#<issue_number>)"`.
   5. Push the branch: `git push -u origin "$BRANCH"`.
   6. Build the draft PR body at `.integration-pipeline/draft-pr-body.md`: a short preamble stating this is a **design-review** PR for the `<integration_name>` integration, that the full design is embedded below, that implementation lands in later commits, that the PR is marked Ready for review at the pipeline's PR gate (phase 16), and a `Closes #<issue_number>` line — followed by a `---` separator and then the **entire verbatim contents** of `.integration-pipeline/integration-design.md` (so a remote reviewer sees the whole design without local access to the gitignored file).
   7. Open the draft PR (provisional title; phase 16 finalizes it): `URL=$(gh pr create --draft --title "<integration_name> (#<issue_number>)" --body-file .integration-pipeline/draft-pr-body.md)`.
   8. `NUM=$(gh pr view --json number -q .number)`.
   9. Store both: `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py set pr_url "$URL"` and `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py set pr_number "$NUM"`.
9. Run `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py complete 3`.

## Output

- The file at `pipeline.py get design_path` (default `.integration-pipeline/integration-design.md`) exists, Status: Proposed, all 11 sections present.
- A **draft PR** is open against the integration branch, carrying the full design in its body; `pr_url` and `pr_number` are stored in pipeline state.
- No design artefact was committed — the working tree carries only a single empty seed commit.
- Phase 3 marked complete; pipeline now in `waiting_for_approval` for phase 3 — the maintainer reviews the design in the draft PR.

## Refusals

- Design path already exists with content → stop, do not overwrite. Tell the maintainer to reset or move the existing file.
- Current branch is `main` → refuse: "cannot open a PR from main".
- `gh` not on PATH → refuse: "gh not found".
- Any `.integration-pipeline/` path would be committed → refuse: "pipeline state must never be committed"; the design rides in the PR body, not in a commit.
- Maintainer asks to write to `docs/decisions/...` or `docs/architecture/...` → refuse and explain: outbound integrations are scoped to be portable; the repo-level ADR slots would orphan.
- Design forces NeoHaskell-style violations → surface the conflict; do not silently rewrite the API.
- Integration fails the Jess 15-minute test → surface the failure and request a redesign before drafting.
- `## Portability note` would be missing or would reference `docs/decisions/` / `docs/architecture/` → refuse the draft.
- **The design proposes that the integration read environment variables itself** — i.e. calls `System.Environment.getEnv`, `Env.lookupEnv`, embeds `"${VAR}"` literals expected to be expanded by the integration or by `Integration.Http`, or otherwise sources secrets outside the user's project `Config.hs` — refuse the draft and rewrite to use the `Config.field @(Redacted Text) ... |> Config.required |> Config.envVar "..." |> Config.secret` DSL on the user side, with the integration consuming the value via a `(?config :: config, HasField "<fieldName>" config (Redacted Text))` constraint. The exception is the legacy pattern used by `Integration.OpenRouter`, `Integration.Oura`, and `Integration.Http.Auth`'s `${VAR}` expansion — those are pre-existing tech debt and may be cited only as work to be migrated, never as templates to copy.
- **The design exposes a raw `Text` secret in the public API of the integration** — i.e. any function or record field on the integration's public surface that takes a plain `Text` for an API key, OAuth token, webhook signing secret, or bearer — refuse the draft and rewrite to either (a) source the secret from `?config.<field>` (preferred), or (b) accept `Redacted Text` only.
- **The design implies more than one `Redacted.unwrap` site** in the integration — refuse and require a single audited unwrap inside the request-builder (`<Module>.Internal.toHttpRequest`-style function) that constructs the outbound header tuple.
- **The integration is HTTP request/response but the design does not wrap `Integration.Http`** — refuse. ~95% of outbound integrations are HTTPS request/response and MUST follow the canonical `<Module>.hs` + `<Module>/Request.hs` + `<Module>/Response.hs` + `<Module>/Internal.hs` (with `toHttpRequest` and `ToAction (Request command)`) layout used by `Integration.Brevo`, `Integration.OpenRouter`, `Integration.Oura`. Refuse if the design (a) hand-rolls an HTTP client, (b) imports `http-client` / `wreq` / `req` / `servant-client` directly, or (c) omits a wrap-Http alternative from `## Considered options`. The only valid departures are concrete, named blockers in `## Decision drivers`: WebSocket / SSE / chunked-streaming response, persistent TCP/gRPC connection, background polling daemon, scheduled timer, or non-HTTP wire (raw TCP / UDP / IPC / file-system notify). "For flexibility" or "for performance" without a measured cost is not a valid blocker.
