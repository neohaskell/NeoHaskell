#!/usr/bin/env bash
# Deterministic consistency check for the change-process documents
# (ADR-0075, amended by ADR-0076). The step ids, the model tiers and the
# V1-V9 verdict are stated in several places — the orchestrator skill, the
# per-step agent definitions, and the ADR — because each document has to be
# self-contained for the agent that reads it. That redundancy is deliberate,
# which makes DRIFT the real failure mode, not duplication.
#
# Before ADR-0076 this compared formula/skill/ADR. The formula is gone; the
# agent definitions took its place as the authoritative per-step contract,
# and they carry something the formula never did — a pinned `model:`. A tier
# that drifts between the dispatch table and the agent frontmatter would send
# a step to the wrong model silently, so it is checked here.
set -euo pipefail
cd "$(dirname "$0")/.."

skill=.claude/skills/change/SKILL.md
adr=docs/decisions/0076-session-launched-change-process.md
agents_dir=.claude/agents

fail=0
err() { echo "process-check: $1" >&2; fail=1; }

for f in "$skill" "$adr"; do
  [ -f "$f" ] || err "missing $f"
done
[ "$fail" -eq 0 ] || exit 1

# 1. The canonical steps each have an agent definition. `spec-approval` is
#    deliberately absent: it is a conversation with Nick, not an agent.
want="spec adr build verify pr"
for s in $want; do
  [ -f "${agents_dir}/change-${s}.md" ] \
    || err "no agent definition for step '${s}' (${agents_dir}/change-${s}.md)"
  grep -q "change-${s}" "$skill" \
    || err "the orchestrator skill never dispatches step '${s}'"
done

# 2. Model tiers agree between the skill's dispatch table and each agent's
#    frontmatter. The table row is what a reader trusts; the frontmatter is
#    what actually runs.
declare -a pairs=("spec:opus" "adr:opus" "build:sonnet" "verify:opus" "pr:sonnet")
for pair in "${pairs[@]}"; do
  step="${pair%%:*}"; tier="${pair##*:}"
  f="${agents_dir}/change-${step}.md"
  [ -f "$f" ] || continue
  actual=$(grep -m1 '^model:' "$f" | sed 's/model:[[:space:]]*//')
  [ "$actual" = "$tier" ] \
    || err "step '${step}': agent frontmatter says model '${actual}', process says '${tier}'"
  grep -qE "\| ${step} \| \`change-${step}\` \| ${tier} \|" "$skill" \
    || err "step '${step}': the skill's dispatch table does not pin ${tier}"
done

# 3. The verdict is V1-V9 wherever it is named; a stale V1-V8 (or a V10 added
#    in only one place) surfaces as a mismatch.
for f in "$skill" "$adr" "${agents_dir}/change-verify.md"; do
  grep -qE "V1.{1,3}V9" "$f" || err "$f does not mention the V1-V9 verdict"
  if grep -qE "V1.{1,3}V8" "$f"; then err "$f still mentions V1-V8"; fi
done

# 4. The retired substrate stays retired. Re-adding any of these without
#    revisiting ADR-0076 would give the repo two live, contradictory
#    playbooks — which is exactly the state ADR-0076 was written to end.
for gone in .beads/formulas/change.formula.toml \
            .claude/skills/neohaskell-change/SKILL.md \
            .claude/skills/neohaskell-enqueue/SKILL.md; do
  [ -e "$gone" ] && err "$gone is back — ADR-0076 retired it; reconcile the ADR before restoring"
done

if [ "$fail" -eq 0 ]; then
  echo "process-check: OK — 5 steps, tiers pinned and agreed, V1-V9 consistent"
fi
exit "$fail"
