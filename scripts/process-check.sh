#!/usr/bin/env bash
# Deterministic coherence check for the active change process (ADR-0076).
# Exactly one process may be discoverable: neohaskell-pipeline backed by
# .pipeline/state.json. Superseded queue-backed execution assets stay deleted.
set -euo pipefail
cd "$(dirname "$0")/.."

fail=0
err() { echo "process-check: $1" >&2; fail=1; }

active_skill=.pi/skills/neohaskell-pipeline/SKILL.md
adr_0067=docs/decisions/0067-contract-delta-spec-gate.md
adr_0075=docs/decisions/0075-change-process-v2.md
adr_0076=docs/decisions/0076-restore-resumable-change-pipeline.md

required=(
  "$active_skill"
  scripts/pipeline-state
  "$adr_0067"
  "$adr_0075"
  "$adr_0076"
)
for path in "${required[@]}"; do
  [ -e "$path" ] || err "missing active-process asset: $path"
done

# The restored process must be discoverable and internally coherent.
grep -qF 'name: neohaskell-pipeline' "$active_skill" || err "active skill has the wrong name"
grep -qF '.pipeline/state.json' "$active_skill" || err "active skill does not name its resume state"
grep -qF '.claude/allow-expectation-edits' "$active_skill" || err "active skill has a stale expectation-approval path"
grep -qF './dev telemetry' "$active_skill" || err "active skill does not restore pipeline telemetry"
grep -qF 'telemetry schema v5 canon' "$adr_0067" || err "ADR-0067 has a stale telemetry schema version"
grep -qF '.claude/allow-expectation-edits' "$adr_0067" || err "ADR-0067 has a stale expectation-approval path"
grep -qF './dev pipeline approve spec --by <who> --via <channel>' "$adr_0067" || err "ADR-0067 does not name the canonical local approval record"
if grep -qE 'telemetry schema v2|\.pipeline/allow-expectation-edits|maintainer `@claude` comment' "$adr_0067"; then
  err "ADR-0067 still contains a superseded active-process contract"
fi
grep -qF 'Any request that should end in a PR runs the `neohaskell-pipeline` skill' AGENTS.md || err "AGENTS.md does not route PR work to the restored pipeline"
grep -qF '.pipeline/state.json' AGENTS.md || err "AGENTS.md does not name the resume contract"

# The authoritative process must be available to Pi, not merely present under a
# harness-specific directory. Delegate the discovery/frontmatter/pipeline
# contract to its single validator; keep Neo starter/routing checks out of this
# process-specific gate.
if ! scripts/neo-skills-check --pi-only >/dev/null; then
  err "canonical pipeline is not discoverable or valid in Pi"
fi

if grep -qF 'WARNING: ./dev pipeline is deprecated' dev; then
  err "./dev pipeline still emits the retired queue warning"
fi
if ! grep -qE '^  pipeline\) exec scripts/pipeline-state ' dev; then
  err "./dev pipeline is not directly registered to scripts/pipeline-state"
fi

# The superseded process is recoverable from Git history, not from artifacts in
# the current tree. Keep this list explicit so any attempted reintroduction is a
# reviewable process-check change.
retired_paths=(
  .beads
  .agents/skills/beads
  .claude/agents
  .claude/skills/beads
  .claude/skills/neohaskell-change
  .claude/skills/neohaskell-enqueue
  .claude/hooks/bd-token-tracking.py
  .claude/hooks/bd-dolt-sync.sh
  .codex/config.toml
  .codex/hooks.json
  docs/legacy/neohaskell-beads
)
for path in "${retired_paths[@]}"; do
  [ ! -e "$path" ] || err "superseded process artifact still exists: $path"
done

if git ls-files | grep -qE '(^|/)(\.beads|neohaskell-beads|beads)(/|$)|bd-(token-tracking|dolt-sync)'; then
  err "tracked Beads artifact remains in the current tree"
fi
if grep -qE 'bd-token-tracking|bd prime|bd-dolt-sync' .claude/settings.json; then
  err ".claude/settings.json still activates retired automation"
fi
if grep -qE 'bd ready|neohaskell-enqueue|BEGIN BEADS INTEGRATION|Beads Issue Tracker' AGENTS.md; then
  err "AGENTS.md still advertises retired work intake"
fi
if grep -qiE 'beads|inputs\.beads|packages.*bd' flake.nix flake.lock; then
  err "Nix configuration still contains the retired Beads dependency"
fi
if grep -qE '\.beads|beads-credential|\.dolt/' .gitignore .gitattributes; then
  err "Git ignore/merge configuration still contains retired store rules"
fi
grep -qF 'implementation artifacts from this process were deleted' "$adr_0075" || err "ADR-0075 does not record artifact deletion"
grep -qF 'Git history is the' "$adr_0076" || err "ADR-0076 does not name Git history"
grep -qF 'single recovery source' "$adr_0076" || err "ADR-0076 does not make Git history the recovery source"

status_of() {
  awk '/^## Status$/ { getline; getline; print; exit }' "$1"
}
[ "$(status_of "$adr_0075")" = "Superseded" ] || err "ADR-0075 is not Superseded"
[ "$(status_of "$adr_0076")" = "Implemented" ] || err "ADR-0076 is not Implemented at PR-ready"

grep -qF '| [0075](0075-change-process-v2.md) | Change process v2: five coarse steps, one human gate, verified auto-merge | Superseded |' docs/decisions/README.md || err "ADR index does not mark ADR-0075 Superseded"
grep -qF '| [0076](0076-restore-resumable-change-pipeline.md) | Restore the resumable contract-delta change pipeline | Implemented |' docs/decisions/README.md || err "ADR index omits implemented ADR-0076"

if [ "$fail" -eq 0 ]; then
  echo "process-check: OK — .pipeline is authoritative; superseded queue artifacts are absent"
fi
exit "$fail"
