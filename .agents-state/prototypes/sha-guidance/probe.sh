#!/bin/sh
set -eu

base=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
work=$(mktemp -d "$base/run.XXXXXX")
source_dir="$work/source"
consumer="$work/consumer"
mkdir -p "$source_dir/docs/agent" "$consumer/.neo/agent-docs/cache" "$consumer/.agents/skills/custom"
git -C "$source_dir" init -q

printf '%s\n' '# NeoHaskell guidance' '' 'Open [entities](entities.md) for entity work.' > "$source_dir/docs/agent/index.md"
printf '%s\n' '# Entities' '' 'example-token: alpha' > "$source_dir/docs/agent/entities.md"
git -C "$source_dir" add .
git -C "$source_dir" -c user.name=Prototype -c user.email=prototype@example.invalid commit -qm alpha
sha_a=$(git -C "$source_dir" rev-parse HEAD)

printf '%s\n' '# Entities' '' 'example-token: beta' > "$source_dir/docs/agent/entities.md"
git -C "$source_dir" add .
git -C "$source_dir" -c user.name=Prototype -c user.email=prototype@example.invalid commit -qm beta
sha_b=$(git -C "$source_dir" rev-parse HEAD)

git -C "$source_dir" rm -qr docs/agent
git -C "$source_dir" -c user.name=Prototype -c user.email=prototype@example.invalid commit -qm no-docs
sha_missing=$(git -C "$source_dir" rev-parse HEAD)

cat > "$consumer/AGENTS.md" <<'EOF'
# NeoHaskell project guidance

For NeoHaskell tasks, read `.neo/agent-docs/index.md` first. Open only the relevant page linked there. Use only docs for the resolved SHA shown by that index. If the index says unavailable, report that and do not use another SHA. Preserve this project's developer instructions and skills.

Developer rule: keep the user-provided marker `KEEP-ME`.
EOF
printf '%s\n' '@AGENTS.md' > "$consumer/CLAUDE.md"
printf '%s\n' '---' 'name: custom' 'description: User-owned workflow for the scratch app.' '---' '' 'KEEP-ME' > "$consumer/.agents/skills/custom/SKILL.md"
agent_hash=$(shasum -a 256 "$consumer/AGENTS.md" | cut -d ' ' -f 1)
skill_hash=$(shasum -a 256 "$consumer/.agents/skills/custom/SKILL.md" | cut -d ' ' -f 1)

sync() {
  sha=$1
  cache="$consumer/.neo/agent-docs/cache/$sha"
  index="$consumer/.neo/agent-docs/index.md"
  printf '# NeoHaskell docs\n\nResolved SHA: `%s`\n\nExact-SHA docs unavailable.\n' "$sha" > "$index"
  if ! git -C "$source_dir" cat-file -e "$sha:docs/agent/index.md" 2>/dev/null; then
    return 1
  fi
  mkdir -p "$cache"
  git -C "$source_dir" show "$sha:docs/agent/index.md" > "$cache/index.md"
  git -C "$source_dir" show "$sha:docs/agent/entities.md" > "$cache/entities.md"
  printf '# NeoHaskell docs\n\nResolved SHA: `%s`\n\nOpen [task index](cache/%s/index.md).\n' "$sha" "$sha" > "$index"
}

sync "$sha_a"
grep -q 'alpha' "$consumer/.neo/agent-docs/cache/$sha_a/entities.md"
grep -q "$sha_a" "$consumer/.neo/agent-docs/index.md"
printf '%s\n' 'PASS initial exact-SHA sync'

mv "$source_dir" "$work/source.offline"
grep -q 'alpha' "$consumer/.neo/agent-docs/cache/$sha_a/entities.md"
printf '%s\n' 'PASS offline read after sync'
mv "$work/source.offline" "$source_dir"

sync "$sha_b"
grep -q 'beta' "$consumer/.neo/agent-docs/cache/$sha_b/entities.md"
grep -q "$sha_b" "$consumer/.neo/agent-docs/index.md"
! grep -q "$sha_a" "$consumer/.neo/agent-docs/index.md"
printf '%s\n' 'PASS SHA change selects beta without deleting alpha'

if sync "$sha_missing"; then
  printf '%s\n' 'FAIL missing exact-SHA docs were accepted' >&2
  exit 1
fi
grep -q 'unavailable' "$consumer/.neo/agent-docs/index.md"
grep -q "$sha_missing" "$consumer/.neo/agent-docs/index.md"
! grep -q "$sha_b" "$consumer/.neo/agent-docs/index.md"
printf '%s\n' 'PASS missing exact-SHA docs have no active fallback'

ln -s "cache/$sha_b" "$consumer/.neo/agent-docs/current"
grep -q 'beta' "$consumer/.neo/agent-docs/current/entities.md"
rm "$consumer/.neo/agent-docs/current"
ln -s "cache/$sha_missing" "$consumer/.neo/agent-docs/current"
test ! -e "$consumer/.neo/agent-docs/current/index.md"
printf '%s\n' 'PASS symlink variant resolves exact SHA but has no unavailable message'

test "$agent_hash" = "$(shasum -a 256 "$consumer/AGENTS.md" | cut -d ' ' -f 1)"
test "$skill_hash" = "$(shasum -a 256 "$consumer/.agents/skills/custom/SKILL.md" | cut -d ' ' -f 1)"
test "$(cat "$consumer/CLAUDE.md")" = '@AGENTS.md'
printf '%s\n' 'PASS developer instructions, skill, and Claude import preserved'

sync "$sha_b"
printf 'RUN=%s\nSHA_A=%s\nSHA_B=%s\nSHA_MISSING=%s\n' "$work" "$sha_a" "$sha_b" "$sha_missing"
