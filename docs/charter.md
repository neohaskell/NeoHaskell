---
convention: pai-freshness-v1
last_reviewed: 2026-08-11
review_cadence_days: 90
ratified_by: principal
---

# NeoHaskell Charter

## Mission

AI makes code abundant. Event Modeling makes generated software coherent. Event sourcing preserves its meaning over time. NeoHaskell makes those constraints the default rather than an optional convention.

The bottleneck has moved from producing syntax to maintaining a truthful model of what software means while humans and AI agents change it. NeoHaskell answers that: events, entities, commands, queries, services, and integrations are language primitives, not library patterns — if a state transition is wrong, the program does not compile. Auditability is the system: the audit log is the database. DDD by compile error, not by discipline.

Primary user: the human+agent pair building reliable software together. The unaided human newcomer is the accessibility proof, not the only customer.

## Architecture layers

Lower layers never know about higher ones.

1. **Core (stdlib)** — the NeoHaskell standard library.
2. **Framework, abstract** — event-sourcing/CQRS primitives and contracts (events, entities, commands, queries, services).
3. **Framework, infra implementations** — concrete backends for the abstract framework (Postgres event store, Azure, filesystem, ...).
4. **Integrations** — outbound capability adapters (email, LLM providers, PDF, OCR, HTTP, ...).
5. **neopackages** — the package repository, connected to the docs so every package is documented where it is discovered.
6. **Platform modules** — live surfaces consumed primarily through `neo ide`: event-model diagram (exists today), logs, live recompilation, tests, and connection to deployed applications.

## Horizon (6–18 months)

- 0.10.0 shipped: automated releases (version bump, release notes, migration prompt, blog post from merged PRs) and book-style docs including generated ADRs.
- A newcomer builds and ships a real app unaided; clean-machine onboarding within the 600s SLO.
- First external contributor merges a PR unaided.
- Nightly benchmarks green for 30 consecutive days.
- A real production application built on NeoHaskell ships and stays up.
- Cold-start rebuild no longer blocks deployment readiness (no liveness crash-loops).
- Discipline: at most 3 fronts open at once.

## No-goals

- Not a general-purpose Haskell replacement, and not competing with systems languages.
- Unbound seams (message-bus bindings, FaaS targets, CRDT/offline, custom syntax) are declared honestly as seams — never promised as features.
- No mass-community growth targets yet; authority is earned by shipping.
- Claims describe only what is implemented. The bar is honesty: a vague claim is worse than no claim.

## Kill criteria

No global kill: this is a 10-year project (authority horizon 2036: 1.0 plus a community that ships without its founder). Each sub-project instead carries a quarterly kill-or-grow review against this charter; a layer that no serving consumer needs and no horizon outcome requires is a kill candidate.

## Canonical specs

Consumer applications carry `event-model.json` (skill-generated) as their canonical product spec. This repo's own contracts live in `docs/decisions/` (ADRs) and the spec-gated pipeline.
