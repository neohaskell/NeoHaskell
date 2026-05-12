---
name: 03-record
description: Persists grounded security findings to the pipeline state and marks phase 4 complete.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Record Security Findings

Writes grounded findings to `.pipeline/findings-04.json` with aggregate counts and registers them via the pipeline script.

## Inputs

- stdin or `--input <path>` — JSON array of grounded findings from step 2.

## Plan (Karpathy 1 + 4)

1. Read findings JSON → verify: array parses.
2. Compute aggregate counts (`total_findings`, `blockers`, `kept`, `demoted`, `framework_debt`) → verify: counts sum to total.
3. Write `.pipeline/findings-04.json` → verify: file exists and reparses.
4. Call `pipeline.py findings 4 <path>` then `pipeline.py complete 4` → verify: both exit 0.

Assumptions:
- An empty findings array is valid — still produces a record with zero counts.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Run a single inline python invocation that reads stdin (or `--input`), computes aggregates, writes `.pipeline/findings-04.json`, then shells out:

```
python3 -c "import sys, json, subprocess
data = json.load(sys.stdin)
blockers = sum(1 for f in data if f.get('severity_after_grounding') in ('Critical','High'))
kept = sum(1 for f in data if f.get('grounding_outcome') == 'keep')
demoted = sum(1 for f in data if f.get('grounding_outcome') == 'demote')
framework = sum(1 for f in data if f.get('grounding_outcome') == 'framework-debt')
out = {'total_findings': len(data), 'blockers': blockers, 'kept': kept, 'demoted': demoted, 'framework_debt': framework, 'findings': data}
open('.pipeline/findings-04.json','w').write(json.dumps(out, indent=2))"
```

2. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py findings 4 .pipeline/findings-04.json`.
3. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 4`.
4. On any non-zero exit, surface stderr and stop.

## Output

`.pipeline/findings-04.json` written; phase 4 marked complete in pipeline state.

## Refusals

- stdin not valid JSON → refuse: "input is not a JSON array".
- `pipeline.py` returns non-zero → refuse and surface stderr.
