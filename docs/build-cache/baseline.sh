#!/usr/bin/env bash
# Run from a clean, immutable baseline checkout on a dedicated Linux runner.
set -euo pipefail
recorder="${1:?absolute path to measure.py}"
evidence="${2:?absolute path to evidence directory}"
initial_revision="$(git rev-parse HEAD)"

assert_clean_tracked() {
  local changes="$(git status --porcelain --untracked-files=no)"
  [[ -z "$changes" ]] && return 0
  echo 'refusing checkout with uncommitted tracked changes:' >&2
  printf '%s\n' "$changes" >&2
  return 1
}

refuse_overwrite() {
  [[ ! -e "$1" ]] && return 0
  echo "refusing to overwrite existing evidence: $1" >&2
  return 1
}

write_once() { local path="$1"; shift; refuse_overwrite "$path"; "$@" > "$path"; }

restore_baseline() {
  local original_exit="$1"
  assert_clean_tracked || return 1
  if [[ "$(git rev-parse HEAD)" != "$initial_revision" ]]; then
    git checkout --detach "$initial_revision" || { echo "refusing to continue: could not return to normalized baseline $initial_revision" >&2; return 1; }
  fi
  assert_clean_tracked || return 1
  return "$original_exit"
}

assert_clean_tracked
trap 'restore_baseline "$?"' EXIT
mkdir -p "$evidence"
write_once "$evidence/raw-baseline.txt" git rev-parse HEAD

# Normalize only the network-dependent mock fixture, identically to the candidate.
# Keep raw-baseline failure logs separately; production/library source is unchanged.
python3 - <<'PYFIXTURE'
from pathlib import Path
p = Path('integrations/test/Integration/Oura/SyncAllSpec.hs')
s = p.read_text()
old = '        , refreshToken = Just (mkRefreshToken "mock-refresh-token")'
assert s.count(old) == 1
assert s.count('  , mkRefreshToken\n') == 1
s = s.replace('  , mkRefreshToken\n', '').replace(old,
    '        -- Fetches are mocked; a refresh token would call the real token endpoint\n'
    '        -- after the Unauthorized fixtures and make these unit tests network-dependent.\n'
    '        , refreshToken = Nothing')
p.write_text(s)
PYFIXTURE
write_once "$evidence/fixture-normalization.patch" git diff
git add integrations/test/Integration/Oura/SyncAllSpec.hs
GIT_AUTHOR_DATE='2026-09-25T11:00:00Z' GIT_COMMITTER_DATE='2026-09-25T11:00:00Z' \
  git -c user.name='Build measurement' -c user.email='build-measurement@neohaskell.org' \
  commit -m 'test: isolate baseline mocked authorization failures from the network'
initial_revision="$(git rev-parse HEAD)"
write_once "$evidence/initial-revision.txt" printf '%s\n' "$initial_revision"
write_once "$evidence/comparable-baseline.txt" printf '%s\n' "$initial_revision"

targets=(lib:nhcore lib:nhintegrations lib:nhtestbed
  nhcore:test:nhcore-test-core nhcore:test:nhcore-test-auth
  nhcore:test:nhcore-test-integration nhcore:test:nhcore-test-service
  nhintegrations:test:nhintegrations-test exe:nhtestbed)
record() {
  local name="$1" scenario="$2" stage="$3" state="$4"
  shift 4
  python3 "$recorder" record "$evidence/$name" \
    --scenario "$scenario" --stage "$stage" --local-state "$state" \
    --remote-state 'cache.nixos.org/cache.iog.io/neohaskell.cachix.org enabled; baseline Cabal project outputs are not substitutable' \
    -- "$@"
}
write_once "$evidence/store-before.txt" nix path-info --all
write_once "$evidence/nix-version.txt" nix --version
write_once "$evidence/platform.txt" uname -a
record setup fresh setup 'fresh hosted runner after Nix installation; see store-before.txt' \
  nix develop --accept-flake-config --command bash -c 'ghc --numeric-version; cabal --numeric-version'
record build fresh compile-link 'development shell realized; no dist-newstyle' \
  nix develop --accept-flake-config --command cabal build "${targets[@]}" --disable-documentation -v2
record repeat repeat build 'exact Cabal outputs and shell present' \
  nix develop --accept-flake-config --command cabal build "${targets[@]}" --disable-documentation -v2
export POSTGRES_AVAILABLE=true
for suite in nhcore-test-core nhcore-test-auth nhcore-test-integration nhcore-test-service nhintegrations-test; do
  record "test-$suite" fresh "execute-$suite" 'compiled tests and shell present; real PostgreSQL fixture' \
    nix develop --accept-flake-config --command cabal test "$suite" --test-show-details=direct
done
record hurl fresh execute-hurl 'compiled testbed and shell present; real PostgreSQL fixture' \
  nix develop --accept-flake-config --command bash testbed/scripts/run-tests.sh
record cold-start fresh execute-cold-start 'compiled testbed and shell present; real PostgreSQL fixture' \
  nix develop --accept-flake-config --command bash testbed/scripts/cold-start-readiness.sh

# Deliberately change implementation, keeping behavior and expectations intact.
# Commit identity is reproducible across repetitions (fixed author/date/parent).
python3 - <<'PY'
from pathlib import Path
p = Path('core/core/Text.hs'); s = p.read_text()
old = 'isEmpty = Data.Text.null\n'
assert s.count(old) == 1, 'representative mutation no longer matches baseline'
p.write_text(s.replace(old, 'isEmpty text = text |> Data.Text.null\n'))
PY
git add -- core/core/Text.hs
GIT_AUTHOR_DATE='2026-09-25T12:00:00Z' GIT_COMMITTER_DATE='2026-09-25T12:00:00Z' \
  git -c user.name='Build measurement' -c user.email='build-measurement@neohaskell.org' \
  commit -m 'test: disposable representative Text implementation mutation'
write_once "$evidence/mutation.patch" git show --format=fuller HEAD
record edit implementation-edit compile-link 'baseline Cabal outputs retained before one committed implementation edit' \
  nix develop --accept-flake-config --command cabal build "${targets[@]}" --disable-documentation -v2
record edit-test implementation-edit execute-core 'edited core test executable and shell present' \
  nix develop --accept-flake-config --command cabal test nhcore-test-core --test-show-details=direct

# Return to the normalized seed before measuring the sibling edit. Both
# mutation commits remain available in evidence; no reset or clean is used.
restore_baseline 0
python3 - <<'PY'
from pathlib import Path
p = Path('core/core/Int.hs'); s = p.read_text()
old = 'toInt64 = Prelude.fromIntegral\n'
assert s.count(old) == 1, 'sibling implementation mutation no longer matches baseline'
p.write_text(s.replace(old, 'toInt64 value = value |> Prelude.fromIntegral\n'))
PY
git add -- core/core/Int.hs
GIT_AUTHOR_DATE='2026-09-25T12:05:00Z' GIT_COMMITTER_DATE='2026-09-25T12:05:00Z' \
  git -c user.name='Build measurement' -c user.email='build-measurement@neohaskell.org' \
  commit -m 'test: disposable sibling Int implementation mutation'
write_once "$evidence/sibling-mutation.patch" git show --format=fuller HEAD
record sibling-edit sibling-implementation-edit compile-link 'baseline Cabal outputs retained before one committed sibling implementation edit' \
  nix develop --accept-flake-config --command cabal build "${targets[@]}" --disable-documentation -v2
record sibling-edit-test sibling-implementation-edit execute-core 'edited sibling core executable and shell present' \
  nix develop --accept-flake-config --command cabal test nhcore-test-core --test-show-details=direct
restore_baseline 0
