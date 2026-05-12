---
name: 03-record
description: Persists grounded performance findings to the pipeline state and marks phase 5 complete.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Record Performance Findings

Writes grounded findings to `.pipeline/findings-05.json` and registers them via the pipeline script.

## Inputs

- stdin or `--input <path>` — JSON array of grounded findings from step 2.

## Plan (Karpathy 1 + 4)

1. Read findings JSON → verify: array parses.
2. Compute aggregate counts (`total_findings`, `blockers`, `kept`, `demoted`, `framework_debt`) → verify: counts sum to total.
3. Write `.pipeline/findings-05.json` → verify: file exists and reparses.
4. Call `pipeline.py findings 5 <path>` then `pipeline.py complete 5` → verify: both exit 0.

Assumptions:
- An empty findings array is valid — still produces a record with zero counts.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Run a single inline python invocation that reads stdin, computes aggregates, and writes the file:

```
python3 -c "import sys, json
data = json.load(sys.stdin)
blockers = sum(1 for f in data if f.get('severity_after_grounding') == 'Blocking')
kept = sum(1 for f in data if f.get('grounding_outcome') == 'keep')
demoted = sum(1 for f in data if f.get('grounding_outcome') == 'demote')
framework = sum(1 for f in data if f.get('grounding_outcome') == 'framework-debt')
out = {'total_findings': len(data), 'blockers': blockers, 'kept': kept, 'demoted': demoted, 'framework_debt': framework, 'findings': data}
open('.pipeline/findings-05.json','w').write(json.dumps(out, indent=2))"
```

2. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py findings 5 .pipeline/findings-05.json`.
3. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 5`.
4. On any non-zero exit, surface stderr and stop.

## Output

`.pipeline/findings-05.json` written; phase 5 marked complete.

## Refusals

- stdin not valid JSON → refuse: "input is not a JSON array".
- `pipeline.py` returns non-zero → refuse and surface stderr.
