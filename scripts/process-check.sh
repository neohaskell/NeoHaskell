#!/usr/bin/env bash
# Deterministic consistency check for the change-process v2 documents
# (ADR-0075). The V1-V9 verdict and the five step ids are stated in three
# places (formula, skill, ADR); this catches them drifting apart — the
# process content is triplicated by design (each doc is self-contained),
# so drift is a real failure mode, not a hypothetical.
#
# It also guards agent-guidance REFERENCE INTEGRITY (change 007): guidance
# files must not cite a repo path that does not exist, nor a role name that
# ADR-0075 commit 65f1921 retired. That commit deleted the 20-step roster but
# left nine files citing it, and left the per-step authority contract — above
# all the git authority for steps that push branches and open PRs — homeless.
# References are resolved by path existence ONLY: nothing here opens, fetches,
# or executes a referenced target.
set -euo pipefail
cd "$(dirname "$0")/.."

formula=.beads/formulas/change.formula.toml
skill=.claude/skills/neohaskell-change/SKILL.md
adr=docs/decisions/0075-change-process-v2.md

# The fourteen role files deleted in one go by 65f1921. Citing any of them is
# a dangling reference even though no path is written out. `implementer` is
# split out because it is also an ordinary English noun ("# NeoHaskell
# implementer" is a heading, not a citation), so it only counts when written
# as a reference — backticked or path-suffixed.
retired_roles='bench-runner|bench-sentinel|coverage-auditor|doc-writer|docs-architect|docs-visualizer|perf-reviewer|primitives-reviewer|security-reviewer|spec-writer|test-writer|triager|ui-implementer'
retired_roles_as_ref='`implementer`|implementer\.md'

fail=0
err() { echo "process-check: $1" >&2; fail=1; }

# ── reference-integrity primitives (also exercised by --self-test) ──────────

# The guidance surface: files whose job is to tell an agent what to do and
# where to look. Deliberately NOT all of scripts/ — helper scripts carry test
# fixtures and template placeholders that are paths on purpose, not citations.
# This script is EXCLUDED from its own scan: it has to spell the fourteen
# retired names out in order to detect them.
guidance_files() {
  local root="${1:-.}"
  find "$root/.claude/agents" -name '*.md' 2>/dev/null || true
  find "$root/.claude/skills" -name 'SKILL.md' 2>/dev/null || true
  [ -f "$root/scripts/pr-comments-allowlisted" ] && echo "$root/scripts/pr-comments-allowlisted"
  return 0
}

# A token that looks like a repo-relative path. Placeholder shapes (`NNN-slug`,
# a glob) are skipped: a template that shows where a file WILL live is not a
# citation of a file that should exist.
check_dangling_paths() {
  local root="${1:-.}" f ref
  while IFS= read -r f; do
    [ -n "$f" ] || continue
    while IFS= read -r ref; do
      [ -n "$ref" ] || continue
      [ -e "$root/$ref" ] && continue
      err "$f cites '$ref', which does not exist in the tree"
    done < <(grep -oE '(docs|scripts|codemap|core|testbed|integrations)/[A-Za-z0-9_./-]*\.(md|yaml|yml|toml|txt|hs)' "$f" \
               | grep -vE 'NNN|<|>' | sort -u)
  done < <(guidance_files "$root")
}

check_retired_roles() {
  local root="${1:-.}" f hit
  while IFS= read -r f; do
    [ -n "$f" ] || continue
    hit=$(grep -nEo "(^|[^A-Za-z0-9_-])($retired_roles)([^A-Za-z0-9_-]|$)|($retired_roles_as_ref)" "$f" | head -3 || true)
    [ -n "$hit" ] || continue
    err "$f cites a role retired by 65f1921: $(echo "$hit" | tr '\n' ' ')"
  done < <(guidance_files "$root")
}

# The authority contract the deleted role files used to carry, re-homed into
# the change skill: one block per canonical step id.
check_step_authority() {
  local sk="${1:-$skill}" s block
  for s in spec spec-approval build verify pr; do
    block=$(sed -n "/^### Authority: \`$s\`$/,/^### \|^## /p" "$sk" || true)
    if [ -z "$block" ]; then
      err "$sk has no '### Authority: \`$s\`' block"
      continue
    fi
    echo "$block" | grep -qi 'git authority' \
      || err "$sk authority block for '$s' states no git authority"
    echo "$block" | grep -qi 'never' \
      || err "$sk authority block for '$s' states no never-do clause"
  done
  # The trust boundary the deleted role files carried (change 007 C7).
  grep -qi 'UNTRUSTED INPUT' "$sk" \
    || err "$sk states no untrusted-input rule for GitHub-sourced text"
  # The sharp edge from nh-hg6: the spec step pushes and opens a DRAFT PR,
  # and nothing before the pr step merges.
  sed -n '/^### Authority: `spec`$/,/^### /p' "$sk" | grep -qi 'draft' \
    || err "$sk spec authority does not say the PR it opens is a draft"
}

# ── --self-test: fixtures for the guard behaviors (change 007 C3/C4/C5) ─────

self_test() {
  local tmp status=0
  tmp=$(mktemp -d)
  trap 'rm -rf "$tmp"' RETURN
  mkdir -p "$tmp/.claude/agents" "$tmp/.claude/skills/x" "$tmp/scripts" "$tmp/docs"

  t() { # t <name> <expect-fail|expect-pass> <fn> [args...]
    local name="$1" expect="$2"; shift 2
    fail=0; "$@" 2>/dev/null
    if [ "$expect" = fail ] && [ "$fail" -eq 0 ]; then
      echo "self-test FAIL: $name should have been flagged" >&2; status=1
    elif [ "$expect" = pass ] && [ "$fail" -ne 0 ]; then
      echo "self-test FAIL: $name should have passed" >&2; status=1
    fi
  }

  # C3 — dangling vs resolving path reference
  printf 'see `docs/processes/neohaskell-agents.md`\n' > "$tmp/.claude/agents/a.md"
  t "dangling path is flagged" fail check_dangling_paths "$tmp"
  printf 'see `docs/real.md`\n' > "$tmp/.claude/agents/a.md"; : > "$tmp/docs/real.md"
  t "resolving path passes" pass check_dangling_paths "$tmp"

  # C4 — retired role names vs the eight survivors
  printf 'no PR creation (spec-writer is the one)\n' > "$tmp/.claude/agents/a.md"
  t "retired role is flagged" fail check_retired_roles "$tmp"
  printf 'findings route to ci-medic, docs-auditor, retrospective-miner,\n' > "$tmp/.claude/agents/a.md"
  printf 'seneschal, skill-auditor, skill-designer, ui-reviewer, ux-designer,\n' >> "$tmp/.claude/agents/a.md"
  printf 'and the neohaskell-implementer skill\n' >> "$tmp/.claude/agents/a.md"
  t "surviving agents and hyphenated skill names pass" pass check_retired_roles "$tmp"

  # C5/C6/C7 — per-step authority blocks
  t "missing authority blocks are flagged" fail check_step_authority "$tmp/.claude/skills/x/SKILL.md"

  [ "$status" -eq 0 ] && echo "process-check: self-test OK"
  return "$status"
}

if [ "${1:-}" = "--self-test" ]; then
  self_test
  exit $?
fi

# ── the checks ─────────────────────────────────────────────────────────────

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

# 4. Reference integrity (change 007)
check_dangling_paths .
check_retired_roles .
check_step_authority "$skill"

if [ "$fail" -eq 0 ]; then
  echo "process-check: OK — formula/skill/ADR agree, guidance references resolve"
fi
exit "$fail"
