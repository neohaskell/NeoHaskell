---
name: 04-record
description: Persists grounded implementation performance findings and marks phase 13 complete.
kind: leaf
executor: script
---

# Record Implementation Performance Findings

Writes the grounded findings to `.pipeline/findings-13.json` and registers them via the pipeline script.

## Inputs

- stdin or `--input <path>` — JSON array of grounded findings from step 3.

## Plan (Karpathy 1 + 4)

1. Read findings JSON → verify: array parses.
2. Compute aggregate counts → verify: counts sum to total.
3. Write `.pipeline/findings-13.json` → verify: file exists.
4. Call `pipeline.py findings 13 <path>` then `pipeline.py complete 13` → verify: both exit 0.

Assumptions:
- An empty findings array is valid.
- `blocker` flags derive from `severity_after_grounding == 'Blocking'`.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Run a single inline python invocation:

```
python3 -c "import sys, json
data = json.load(sys.stdin)
for f in data:
    f['blocker'] = f.get('severity_after_grounding') == 'Blocking'
blockers = sum(1 for f in data if f['blocker'])
kept = sum(1 for f in data if f.get('grounding_outcome') == 'keep')
demoted = sum(1 for f in data if f.get('grounding_outcome') == 'demote')
framework = sum(1 for f in data if f.get('grounding_outcome') == 'framework-debt')
out = {'total_findings': len(data), 'blockers': blockers, 'kept': kept, 'demoted': demoted, 'framework_debt': framework, 'findings': data}
open('.pipeline/findings-13.json','w').write(json.dumps(out, indent=2))"
```

2. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py findings 13 .pipeline/findings-13.json`.
3. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 13`.
4. Surface stderr on any non-zero exit.

## Output

`.pipeline/findings-13.json` written; phase 13 marked complete.

## Refusals

- stdin not valid JSON → refuse: "input is not a JSON array".
- `pipeline.py` returns non-zero → refuse and surface stderr.
