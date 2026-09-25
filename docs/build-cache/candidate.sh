#!/usr/bin/env bash
# Same sequential target/execution workload as baseline.sh, on a fresh runner.
set -euo pipefail
recorder="${1:?absolute path to measure.py}"
evidence="${2:?absolute path to evidence directory}"
initial_revision="$(git rev-parse HEAD)"
assert_clean_tracked() {
  local changes
  changes="$(git status --porcelain --untracked-files=no)"
  if [[ -n "$changes" ]]; then
    echo 'refusing checkout with uncommitted tracked changes:' >&2
    printf '%s\n' "$changes" >&2
    return 1
  fi
}
assert_clean_tracked
mkdir -p "$evidence"
printf '%s\n' "$initial_revision" > "$evidence/initial-revision.txt"
restore_initial_revision() {
  local original_exit="$1"
  if ! assert_clean_tracked; then
    return 1
  fi
  if [[ "$(git rev-parse HEAD)" != "$initial_revision" ]]; then
    if ! git checkout --detach "$initial_revision"; then
      echo "refusing to continue: could not return to initial revision $initial_revision" >&2
      return 1
    fi
  fi
  if ! assert_clean_tracked; then
    return 1
  fi
  return "$original_exit"
}
trap 'restore_initial_revision "$?"' EXIT
record() {
  local name="$1" scenario="$2" stage="$3" state="$4"
  shift 4
  python3 "$recorder" record "$evidence/$name" \
    --scenario "$scenario" --stage "$stage" --local-state "$state" \
    --remote-state 'cache.nixos.org/cache.iog.io/neohaskell.cachix.org enabled; actual substitutions/builds in logs; no magic cache' \
    -- "$@"
}
nix path-info --all > "$evidence/store-before.txt"
nix --version > "$evidence/nix-version.txt"
uname -a > "$evidence/platform.txt"
record build fresh evaluate-realize 'fresh hosted runner after Nix installation; see store-before.txt' \
  ./dev nix-components build --directory "$evidence/components"
record repeat repeat build 'exact component outputs and evaluation inputs present' \
  ./dev nix-components build --directory "$evidence/components"
for suite in nhcore-test-core nhcore-test-auth nhcore-test-integration nhcore-test-service nhintegrations-test; do
  record "test-$suite" fresh "execute-$suite" 'compiled tests and runtime present; real PostgreSQL fixture' \
    ./dev nix-components run --directory "$evidence/components" --suite "$suite" --report "$evidence/$suite.log"
done
record hurl fresh execute-hurl 'compiled testbed and runtime present; real PostgreSQL fixture' \
  ./dev nix-components hurl --directory "$evidence/components"
record cold-start fresh execute-cold-start 'compiled testbed and runtime present; real PostgreSQL fixture' \
  ./dev nix-components cold-start --directory "$evidence/components"
# Keep transfer encoding separate from the sequential execution comparison.
record export transfer encode-cache 'complete component closure present' \
  ./dev nix-components export --directory "$evidence/components"
# Do not upload a second full closure as measurement evidence.
du -sk "$evidence/components/cache" > "$evidence/cache-size-kib.txt"
rm -rf "$evidence/components/cache"
python3 - <<'PY'
from pathlib import Path
p=Path('foundation/core/Text.hs' if Path('foundation/nhfoundation.cabal').exists() else 'core/core/Text.hs');s=p.read_text()
old='isEmpty = Data.Text.null\n'
assert s.count(old)==1, 'representative mutation no longer matches candidate'
p.write_text(s.replace(old, 'isEmpty text = text |> Data.Text.null\n'))
PY
if [[ -f foundation/nhfoundation.cabal ]]; then
  git add -- foundation/core/Text.hs
else
  git add -- core/core/Text.hs
fi
GIT_AUTHOR_DATE='2026-09-25T12:00:00Z' GIT_COMMITTER_DATE='2026-09-25T12:00:00Z' \
  git -c user.name='Build measurement' -c user.email='build-measurement@neohaskell.org' \
  commit -m 'test: disposable representative Text implementation mutation'
git show --format=fuller HEAD > "$evidence/mutation.patch"
record edit implementation-edit evaluate-realize 'baseline Nix outputs retained before one committed implementation edit' \
  ./dev nix-components build --directory "$evidence/edited-components"
record edit-test implementation-edit execute-core 'edited core executable and runtime present' \
  ./dev nix-components run --directory "$evidence/edited-components" --suite nhcore-test-core --report "$evidence/edited-core.log"
# Return to the immutable seed before measuring the sibling edit; no reset or
# discarded mutation is used because both mutation commits are retained in evidence.
restore_initial_revision 0
python3 - <<'PY'
from pathlib import Path
p = Path('core/core/Int.hs')
s = p.read_text()
old = 'toInt64 = Prelude.fromIntegral\n'
assert s.count(old) == 1, 'sibling implementation mutation no longer matches candidate'
p.write_text(s.replace(old, 'toInt64 value = value |> Prelude.fromIntegral\n'))
PY
git add -- core/core/Int.hs
GIT_AUTHOR_DATE='2026-09-25T12:05:00Z' GIT_COMMITTER_DATE='2026-09-25T12:05:00Z' \
  git -c user.name='Build measurement' -c user.email='build-measurement@neohaskell.org' \
  commit -m 'test: disposable sibling Int implementation mutation'
git show --format=fuller HEAD > "$evidence/sibling-mutation.patch"
record sibling-edit sibling-implementation-edit evaluate-realize 'baseline Nix outputs retained before one committed sibling implementation edit' \
  ./dev nix-components build --directory "$evidence/sibling-components"
record sibling-edit-test sibling-implementation-edit execute-core 'edited sibling core executable and runtime present' \
  ./dev nix-components run --directory "$evidence/sibling-components" --suite nhcore-test-core --report "$evidence/sibling-core.log"
restore_initial_revision 0
