#!/usr/bin/env bash
# SessionEnd hook: sync beads Dolt data to the PINNED git remote.
#
# Fail-closed: pushes only when the configured Dolt remote's URL matches the
# committed pin in .beads/dolt-remote — beads carry candid agent notes, and
# a remote merely NAMED "origin" could be the project's public git origin.
# Best-effort: never blocks session exit, but failures are not silent —
# they leave .beads/last-sync-error.log and surface a systemMessage.

proj="${CLAUDE_PROJECT_DIR:-.}"
cd "$proj" 2>/dev/null || exit 0
[ -d .beads ] || exit 0

# Tooling runs inside the pinned flake shell, never off the host PATH
# (AGENTS.md "Toolchain"). A host `bd` is a different build whose Dolt schema
# drifts behind the project DB, and it fails looking like beads is broken
# rather than like the wrong binary was picked. with-toolchain is a no-op
# when the shell is already fingerprint-verified.
beads() { scripts/with-toolchain bd "$@"; }
beads --version >/dev/null 2>&1 || exit 0

pin_file=".beads/dolt-remote"
[ -f "$pin_file" ] || exit 0    # no pin, no sync (fail closed by design)
pin=$(head -1 "$pin_file" | tr -d '[:space:]')
actual=$(beads dolt remote list 2>/dev/null | awk '$1=="origin"{print $2; exit}')
[ -n "$actual" ] || exit 0

marker=".beads/last-sync-error.log"
if [ "$actual" != "$pin" ]; then
    printf '%s\nrefused: dolt remote %s does not match pinned %s\n' \
        "$(date)" "$actual" "$pin" > "$marker"
    echo '{"systemMessage": "beads sync REFUSED: dolt remote URL does not match the .beads/dolt-remote pin — fix the remote before any push"}'
    exit 0
fi

# a failed commit means the working set never made it into what push
# ships — pushing anyway would report success while losing the changes
if ! commit_out=$(beads dolt commit -m "session sync" 2>&1); then
    { date; printf 'commit: %s\n' "$commit_out"; } > "$marker"
    echo '{"systemMessage": "beads dolt sync FAILED at commit — see .beads/last-sync-error.log and run ./dev bd dolt status"}'
    exit 0
fi
if push_out=$(beads dolt push 2>&1); then
    rm -f "$marker"
    exit 0
fi
# non-fast-forward: another machine pushed first — merge, then retry once
pull_out=$(beads dolt pull 2>&1)
if push_out=$(beads dolt push 2>&1); then
    rm -f "$marker"
    exit 0
fi
{
    date
    printf 'push: %s\n' "$push_out"
    printf 'pull: %s\n' "$pull_out"
} > "$marker"
echo '{"systemMessage": "beads dolt sync FAILED (likely merge conflict) — see .beads/last-sync-error.log and run ./dev bd dolt status"}'
exit 0
