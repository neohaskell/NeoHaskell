#!/usr/bin/env bash
# Deterministic coherence check for the active change process (ADR-0076).
# Exactly one process may be discoverable: neohaskell-pipeline backed by
# .pipeline/state.json. Beads-era execution assets remain historical only.
set -euo pipefail
cd "$(dirname "$0")/.."

fail=0
err() { echo "process-check: $1" >&2; fail=1; }

active_skill=.claude/skills/neohaskell-pipeline/SKILL.md
legacy_root=docs/legacy/neohaskell-beads
adr_0075=docs/decisions/0075-change-process-v2.md
adr_0076=docs/decisions/0076-restore-resumable-change-pipeline.md

required=(
  "$active_skill"
  scripts/pipeline-state
  "$legacy_root/README.md"
  "$legacy_root/skills/neohaskell-change/SKILL.md"
  "$legacy_root/skills/neohaskell-enqueue/SKILL.md"
  "$legacy_root/skills/beads/SKILL.md"
  "$legacy_root/formulas/change.formula.toml"
  "$legacy_root/hooks/bd-token-tracking.py"
  "$legacy_root/hooks/bd-dolt-sync.sh"
  "$legacy_root/git-hooks/post-checkout"
  "$legacy_root/git-hooks/post-merge"
  "$legacy_root/git-hooks/pre-commit"
  "$legacy_root/git-hooks/pre-push"
  "$legacy_root/git-hooks/prepare-commit-msg"
  "$legacy_root/codex/hooks.json"
  "$legacy_root/codex/config.toml"
  "$adr_0075"
  "$adr_0076"
)
for path in "${required[@]}"; do
  [ -e "$path" ] || err "missing required active/archive asset: $path"
done

# The restored process must be discoverable and internally coherent.
grep -qF 'name: neohaskell-pipeline' "$active_skill" || err "active skill has the wrong name"
grep -qF '.pipeline/state.json' "$active_skill" || err "active skill does not name its resume state"
grep -qF '.claude/allow-expectation-edits' "$active_skill" || err "active skill has a stale expectation-approval path"
grep -qF './dev telemetry' "$active_skill" || err "active skill does not restore pipeline telemetry"
grep -qF 'Any request that should end in a PR runs the `neohaskell-pipeline` skill' AGENTS.md || err "AGENTS.md does not route PR work to the restored pipeline"
grep -qF '.pipeline/state.json' AGENTS.md || err "AGENTS.md does not name the resume contract"

if grep -qF 'WARNING: ./dev pipeline is deprecated' dev; then
  err "./dev pipeline still emits the Beads-era deprecation warning"
fi
if ! grep -qE '^  pipeline\) exec scripts/pipeline-state ' dev; then
  err "./dev pipeline is not directly registered to scripts/pipeline-state"
fi

# Beads-era entry points and automatic hooks must stay outside discovery and
# runtime configuration. The historical .beads store itself is intentionally
# retained for inspection.
for path in \
  .claude/skills/neohaskell-change \
  .claude/skills/neohaskell-enqueue \
  .claude/skills/beads \
  .agents/skills/beads \
  .beads/formulas/change.formula.toml \
  .beads/hooks \
  .claude/hooks/bd-token-tracking.py \
  .claude/hooks/bd-dolt-sync.sh \
  .codex/hooks.json \
  .codex/config.toml; do
  [ ! -e "$path" ] || err "deprecated Beads execution asset is still active: $path"
done

if grep -qE 'bd-token-tracking|bd prime|bd-dolt-sync' .claude/settings.json; then
  err ".claude/settings.json still activates Beads automation"
fi
if grep -qE 'bd ready|neohaskell-enqueue|BEGIN BEADS INTEGRATION|Beads Issue Tracker' AGENTS.md; then
  err "AGENTS.md still advertises Beads work intake"
fi
grep -qF 'deprecated for change execution' .beads/README.md || err ".beads/README.md does not mark the store as historical"
grep -qF 'Historical only' "$legacy_root/skills/neohaskell-change/SKILL.md" || err "archived change skill lacks its historical-only guard"
grep -qF 'Historical only' "$legacy_root/skills/neohaskell-enqueue/SKILL.md" || err "archived enqueue skill lacks its historical-only guard"
grep -qF 'DEPRECATED by ADR-0076' "$legacy_root/formulas/change.formula.toml" || err "archived formula lacks its deprecation guard"

status_of() {
  awk '/^## Status$/ { getline; getline; print; exit }' "$1"
}
[ "$(status_of "$adr_0075")" = "Superseded" ] || err "ADR-0075 is not Superseded"
[ "$(status_of "$adr_0076")" = "Accepted" ] || err "ADR-0076 is not Accepted"

grep -qF '| [0075](0075-change-process-v2.md) | Change process v2: five coarse steps, one human gate, verified auto-merge | Superseded |' docs/decisions/README.md || err "ADR index does not mark ADR-0075 Superseded"
grep -qF '| [0076](0076-restore-resumable-change-pipeline.md) | Restore the resumable contract-delta change pipeline | Accepted |' docs/decisions/README.md || err "ADR index omits accepted ADR-0076"

if [ "$fail" -eq 0 ]; then
  echo "process-check: OK — .pipeline is authoritative; Beads execution assets are historical"
fi
exit "$fail"
