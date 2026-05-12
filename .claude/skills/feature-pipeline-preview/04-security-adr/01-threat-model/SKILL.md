---
name: 01-threat-model
description: Produces raw security findings from the ADR using OWASP + STRIDE applied to a feature diff.
kind: leaf
executor: opus
model: claude-opus-4-7
---

# Threat model

Reads the proposed ADR and produces a raw findings array using the methodology in `../../references/security-methodology.md` (OWASP Top 10 2025, STRIDE, SLSA, SSDF PW practices, parse-don't-validate, secret handling).

## Inputs

- `adr_path` — path to the ADR at `docs/decisions/<NNNN>-<slug>.md`.

## Plan (Karpathy 1 + 4)

1. Read the ADR end-to-end → verify: section list matches (Context, Decision drivers, Considered options, Decision outcome, Public API, Consequences).
2. For each STRIDE letter and each active OWASP category, ask the per-section reviewer check from `../../references/security-methodology.md` → verify: at least one explicit yes/no answer recorded for each.
3. Emit a raw findings JSON array → verify: every entry has `id`, `severity` (Critical/High/Medium/Low/Informational), `rule` (e.g. "tampering", "info-disclosure", "owasp-A03"), `location` (ADR section), `message` (one paragraph), `recommendation` (one paragraph).

Assumptions:
- The ADR exists at `adr_path` and uses the NeoHaskell template.
- The `references/security-methodology.md` digest is authoritative for the questions to ask.
- The framework defaults in `../../references/nhcore-context.md` are honoured — flag a finding only when the diff bypasses or contradicts a framework default.

If any assumption fails, refuse and ask.

## Steps (Karpathy 2 + 3)

1. Read the ADR. Refuse if Status is not `Proposed`.
2. Walk each methodology section (1–9) in order. For each, record the yes/no answer; on "yes" produce a finding entry.
3. Cap raw severity by the answer's actual impact on the feature's reachable surface. Do NOT apply blast-radius filtering — that is the grounding step's job.
4. Emit the JSON array on stdout. Do not write any file (the record step owns persistence).

## Output

Stdout: a JSON array of raw findings. Stderr: empty unless an assumption was violated.

## Refusals

- ADR missing or wrong Status → name the violation, stop.
- Methodology reference missing → refuse and ask the user to restore the skill.
- An ADR section is empty or contradicts itself → record a finding under "owasp-A06 / insecure design" and continue.
