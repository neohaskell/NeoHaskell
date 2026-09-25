#!/usr/bin/env bash
# Run from a clean, immutable baseline checkout on a dedicated Linux runner.
set -euo pipefail
recorder="${1:?absolute path to measure.py}"
evidence="${2:?absolute path to evidence directory}"
mkdir -p "$evidence"
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
nix-store --query --all > "$evidence/store-before.txt"
nix --version > "$evidence/nix-version.txt"
uname -a > "$evidence/platform.txt"
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
p=Path('core/core/Text.hs');s=p.read_text()
old='isEmpty = Data.Text.null\n'
assert s.count(old)==1, 'representative mutation no longer matches baseline'
p.write_text(s.replace(old, 'isEmpty text = text |> Data.Text.null\n'))
PY
git add core/core/Text.hs
GIT_AUTHOR_DATE='2026-09-25T12:00:00Z' GIT_COMMITTER_DATE='2026-09-25T12:00:00Z' \
  git -c user.name='Build measurement' -c user.email='build-measurement@neohaskell.org' \
  -c core.hooksPath=/dev/null commit -m 'test: disposable representative Text implementation mutation'
git show --format=fuller HEAD > "$evidence/mutation.patch"
record edit implementation-edit compile-link 'baseline Cabal outputs retained before one committed implementation edit' \
  nix develop --accept-flake-config --command cabal build "${targets[@]}" --disable-documentation -v2
record edit-test implementation-edit execute-core 'edited core test executable and shell present' \
  nix develop --accept-flake-config --command cabal test nhcore-test-core --test-show-details=direct
