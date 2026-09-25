#!/usr/bin/env bash
# Disposable hosted checkout only: retain the patch and deterministic pilot SHA.
set -euo pipefail
recorder="${1:?absolute path to measure.py}"
evidence="${2:?absolute path to evidence directory}"
harness="$(dirname "$recorder")"
mkdir -p "$evidence"
git rev-parse HEAD > "$evidence/pilot-parent.txt"
git apply --index "$harness/foundation-pilot.patch"
GIT_AUTHOR_DATE='2026-09-25T11:00:00Z' GIT_COMMITTER_DATE='2026-09-25T11:00:00Z' \
  git -c user.name='Build measurement' -c user.email='build-measurement@neohaskell.org' \
  commit -m 'experiment: apply recorded foundation extraction pilot'
git show --format=fuller HEAD > "$evidence/pilot.patch"
bash "$harness/candidate.sh" "$recorder" "$evidence"
# Additional compatibility validation is excluded from matched workload timings.
python3 "$recorder" record "$evidence/compatibility" \
  --scenario pilot-compatibility --stage build-and-execute \
  --local-state 'pilot component outputs already present' \
  --remote-state 'configured flake substituters; inspect logs' -- \
  bash -euo pipefail -c '
    outputs=$(nix build --accept-flake-config -L --no-link --print-out-paths \
      .#nhfoundation:test:nhfoundation-test .#nhcore-consumer:exe:nhcore-consumer)
    foundation=false
    consumer=false
    while IFS= read -r output; do
      if [ -x "$output/bin/nhfoundation-test" ]; then "$output/bin/nhfoundation-test"; foundation=true; fi
      if [ -x "$output/bin/nhcore-consumer" ]; then "$output/bin/nhcore-consumer"; consumer=true; fi
    done <<< "$outputs"
    test "$foundation" = true && test "$consumer" = true
  '
