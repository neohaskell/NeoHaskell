> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 04-security-adr
description: Runs the ADR-level security review (threat-model → ground → record).
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Security review (ADR)

Runs a threat-model pass, then the mandatory grounding pass against the feature classification, then writes the structured findings file. Replaces a single monolithic "security review" with three small steps so the grounding loop cannot be skipped.

## Steps

1. **Threat model** — spawn an Agent (model: opus) and instruct it to read `./01-threat-model/SKILL.md` and follow it on the ADR at `docs/decisions/<adr>-<slug>.md`. Verify: stdout returns a JSON array of raw findings with `severity` and `rule` fields.
2. **Ground** — spawn an Agent (model: sonnet) and instruct it to read `./02-ground/SKILL.md` and follow it on the threat-model output, the classification at `.pipeline/classification.json`, and the references in `../references/grounding-loop.md`. Verify: stdout returns a JSON array where every finding has a `grounding_outcome` and a `severity_after_grounding`.
3. **Record** — read `./03-record/SKILL.md` and follow it. Verify: `.pipeline/findings-04.json` is written and `pipeline.py status` shows the findings registered for phase 4.

Walk these steps in order. The grounding step MUST run, even on a `trivial` feature with zero raw findings — the resulting record is the audit trail. After step 3, the orchestrator runs `pipeline.py complete 4`.

## Shared invariants

- The threat-model step does not filter findings — that is the grounding step's job.
- The grounding step does not introduce new findings — it only re-classifies the ones produced by step 1.
- Findings file path is fixed at `.pipeline/findings-04.json` so the parent process can verify deterministically.
