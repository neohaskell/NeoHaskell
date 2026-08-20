#!/usr/bin/env bash
# SessionEnd hook: sync beads Dolt data to the git remote (refs/dolt/data).
# Best-effort by design — a sync failure must never block session exit,
# so every step swallows errors and we always exit 0.

cd "${CLAUDE_PROJECT_DIR:-.}" 2>/dev/null || exit 0
[ -d .beads ] || exit 0
command -v bd >/dev/null 2>&1 || exit 0

# only sync when a Dolt remote is configured (setup-bd-project.sh adds it)
bd dolt remote list 2>/dev/null | grep -q '^origin' || exit 0

bd dolt commit -m "session sync" >/dev/null 2>&1
if ! bd dolt push >/dev/null 2>&1; then
    # non-fast-forward: another machine pushed first — merge, then retry
    bd dolt pull >/dev/null 2>&1
    bd dolt push >/dev/null 2>&1
fi
exit 0
