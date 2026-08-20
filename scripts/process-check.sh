#!/usr/bin/env bash
# Deterministic consistency check for the change-process v2 documents
# (ADR-0075). The V1-V9 verdict and the five step ids are stated in three
# places (formula, skill, ADR); this catches them drifting apart — the
# process content is triplicated by design (each doc is self-contained),
# so drift is a real failure mode, not a hypothetical.
set -euo pipefail
cd "$(dirname "$0")/.."

formula=.beads/formulas/change.formula.toml
skill=.claude/skills/neohaskell-change/SKILL.md
adr=docs/decisions/0075-change-process-v2.md

fail=0
err() { echo "process-check: $1" >&2; fail=1; }

for f in "$formula" "$skill" "$adr"; do
  [ -f "$f" ] || { err "missing $f"; }
done
[ "$fail" -eq 0 ] || exit 1

# 1. Step ids in the formula are exactly the canonical five, in order
want="spec spec-approval build verify pr"
got=$(grep -E '^id = ' "$formula" | sed 's/id = "\(.*\)"/\1/' | tr '\n' ' ' | sed 's/ $//')
[ "$got" = "$want" ] || err "formula steps are '$got', expected '$want'"

# 2. The verdict is V1-V9 in all three docs; a stale V1-V8 (or a V10 added
#    in only one place) surfaces as a mismatch
for f in "$formula" "$skill" "$adr"; do
  grep -qE "V1.{1,3}V9" "$f" || err "$f does not mention the V1-V9 verdict"
  if grep -qE "V1.{1,3}V8" "$f"; then err "$f still mentions V1-V8"; fi
done

# 3. The skill documents every formula step
for s in $want; do
  grep -q "$s" "$skill" || err "skill does not mention step '$s'"
done

if [ "$fail" -eq 0 ]; then
  echo "process-check: OK — formula/skill/ADR agree on steps and verdict"
fi
exit "$fail"
