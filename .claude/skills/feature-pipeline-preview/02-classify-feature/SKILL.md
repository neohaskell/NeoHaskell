---
name: 02-classify-feature
description: Classifies the feature into a complexity tier via opus judgment, then persists it for the grounding loop.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Classify feature

Decides the feature's tier (`trivial` / `simple` / `moderate` / `complex` / `security-critical`) using an opus-tier judgment leaf, then persists the decision to `.pipeline/classification.json`. The grounding loop in phases 4, 5, 12, 13 reads this file to scale review intensity.

Classification is the load-bearing input for every grounding pass. A wrong call here cascades — a `trivial`-misclassified security feature skips the cryptographic and authorisation rubrics; a `security-critical`-misclassified mock test fixture accumulates a cascade of `constEq` / `ScrubbedBytes` / SHA-pinning recommendations. The decision step uses opus because the cost of a wrong call is higher than the cost of the model.

## Steps

1. **Decide** — spawn an Agent (model: opus) and instruct it to read `./01-decide/SKILL.md` and follow it on the available context (feature name, issue text, module path, ADR draft if present, the diff scope so far). Verify: stdout contains a single JSON document with `tier`, `rationale`, and `signals`; `tier` is one of the five valid values.
2. **Persist** — read `./02-persist/SKILL.md` and follow it with the JSON document from step 1. Verify: `.pipeline/classification.json` exists with the chosen `tier`; `pipeline.py status` lists phase 2 as completed.

Walk these steps in order. After each, run the verify check before continuing. If a verify fails, stop and surface.

## Shared invariants

- The decide step is the only step that interprets the feature; the persist step does not second-guess or override it.
- The decide step uses opus because the cost of misclassification is higher than the cost of the model.
- A maintainer override (e.g. forcing `security-critical` on what the agent calls `simple`) is recorded in the `rationale` field rather than silently substituted.
