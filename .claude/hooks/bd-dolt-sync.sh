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
command -v bd >/dev/null 2>&1 || exit 0

pin_file=".beads/dolt-remote"
[ -f "$pin_file" ] || exit 0    # no pin, no sync (fail closed by design)
pin=$(head -1 "$pin_file" | tr -d '[:space:]')
actual=$(bd dolt remote list 2>/dev/null | awk '$1=="origin"{print $2; exit}')
[ -n "$actual" ] || exit 0

marker=".beads/last-sync-error.log"
if [ "$actual" != "$pin" ]; then
    printf '%s\nrefused: dolt remote %s does not match pinned %s\n' \
        "$(date)" "$actual" "$pin" > "$marker"
    echo '{"systemMessage": "beads sync REFUSED: dolt remote URL does not match the .beads/dolt-remote pin — fix the remote before any push"}'
    exit 0
fi

bd dolt commit -m "session sync" >/dev/null 2>&1
if push_out=$(bd dolt push 2>&1); then
    rm -f "$marker"
    exit 0
fi
# non-fast-forward: another machine pushed first — merge, then retry once
pull_out=$(bd dolt pull 2>&1)
if push_out=$(bd dolt push 2>&1); then
    rm -f "$marker"
    exit 0
fi
{
    date
    printf 'push: %s\n' "$push_out"
    printf 'pull: %s\n' "$pull_out"
} > "$marker"
echo '{"systemMessage": "beads dolt sync FAILED (likely merge conflict) — see .beads/last-sync-error.log and run bd dolt status"}'
exit 0
