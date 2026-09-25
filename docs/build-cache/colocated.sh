#!/usr/bin/env bash
# Disposable hosted experiment: same outputs, fresh execution on both passes.
set -euo pipefail
[[ "${GITHUB_ACTIONS:-}" == true && "${RUNNER_OS:-}" == Linux ]] || {
  echo 'Run only on a disposable GitHub Linux runner.' >&2; exit 1;
}
root="$(git rev-parse --show-toplevel)"
cd "$root"
[[ -z "$(git status --porcelain --untracked-files=no)" ]] || exit 1
revision="$(git rev-parse HEAD)"
[[ "$revision" == "${GITHUB_SHA:?}" ]] || exit 1
repetition="${COLOCATED_REPETITION:?}"
[[ "$repetition" =~ ^[123]$ ]] || { echo 'COLOCATED_REPETITION must be 1, 2, or 3.' >&2; exit 1; }
evidence="${1:?new absolute evidence directory}"
[[ "$evidence" == /* && ! -e "$evidence" ]] || {
  echo 'Evidence directory must be absolute and new.' >&2; exit 1;
}
mkdir -p "$evidence"
recorder="$root/docs/build-cache/measure.py"
container=""
container_log_dir=""
cleanup() {
  local status=$?
  trap - EXIT
  set +e
  if [[ -n "$container" ]]; then
    if [[ -n "$container_log_dir" ]]; then
      docker logs "$container" > "$container_log_dir/postgres-failure.log" 2>&1 || true
    fi
    docker rm -f -v "$container" >/dev/null 2>&1 || true
  fi
  exit "$status"
}
trap cleanup EXIT
record() {
  local destination="$1" scenario="$2" stage="$3" state="$4"
  shift 4
  python3 "$recorder" record "$evidence/$destination" --scenario "$scenario" \
    --stage "$stage" --local-state "$state" \
    --remote-state 'configured public substituters; exact availability in cache-state audit; no magic cache' \
    -- "$@"
}
printf '%s\n' "$revision" > "$evidence/revision.txt"
printf '%s\n' "$repetition" > "$evidence/repetition.txt"
nix --version > "$evidence/nix-version.txt"
nix path-info --all > "$evidence/store-before.txt"
uname -a > "$evidence/platform.txt"
python3 - "$evidence" <<'PY'
import hashlib, json, pathlib, sys
entries = {}
for group in ('commands', 'queries', 'scenarios', 'integrations'):
    files = sorted(pathlib.Path('testbed/tests', group).glob('*.hurl'))
    if not files:
        raise SystemExit(f'missing Hurl fixtures: {group}')
    entries[group] = {str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in files}
pathlib.Path(sys.argv[1], 'hurl-fixtures.json').write_text(json.dumps(entries, indent=2)+'\n')
PY
record postgres-image setup download-fixture 'fresh hosted runner; Docker image availability recorded' \
  docker pull postgres:16-alpine
docker image inspect postgres:16-alpine > "$evidence/postgres-image.json"
for pass in first warm; do
  mkdir "$evidence/$pass"
  cp "$evidence/hurl-fixtures.json" "$evidence/$pass/hurl-fixtures.json"
  export TMPDIR="$evidence/$pass/tmp"
  mkdir "$TMPDIR"
  name="nh-colocated-${GITHUB_RUN_ID:?}-${GITHUB_RUN_ATTEMPT:?}-$pass"
  container_log_dir="$evidence/$pass"
  if docker container inspect "$name" >/dev/null 2>&1; then
    echo "Refusing to replace existing container: $name" >&2; exit 1;
  fi
  record "$pass/postgres-start" "$pass" fixture-start 'new disposable database, empty data volume' \
    docker run -d --name "$name" -e POSTGRES_USER=neohaskell \
      -e POSTGRES_PASSWORD=neohaskell -e POSTGRES_DB=neohaskell \
      -p 5432:5432 postgres:16-alpine postgres -c max_connections=200
  container="$name"
  record "$pass/postgres-ready" "$pass" fixture-readiness 'new PostgreSQL instance; TCP readiness and SQL required' \
    bash -c 'set -euo pipefail; for _ in {1..30}; do
      if docker exec "$1" pg_isready -h 127.0.0.1 -U neohaskell; then break; fi; sleep 1;
    done
    docker exec "$1" pg_isready -h 127.0.0.1 -U neohaskell
    docker exec -e PGPASSWORD=neohaskell "$1" psql -h 127.0.0.1 -U neohaskell -d neohaskell -v ON_ERROR_STOP=1 -c "SELECT 1"' _ "$container"
  python3 "$root/docs/build-cache/cache-state.py" --output "$evidence/$pass/cache-state.json"
  if [[ "$pass" == first ]]; then
    state='fresh hosted Nix store after installation and cache audit; see store-before.txt'
  else
    state='same worker/store and immutable checkout; previous outputs present; fresh database and uploads'
  fi
  record "$pass/build" "$pass" evaluate-realize "$state" \
    ./dev nix-components build --directory "$evidence/$pass/components"
  for suite in nhcore-test-core nhcore-test-auth nhcore-test-integration nhcore-test-service nhintegrations-test; do
    record "$pass/test-$suite" "$pass" "execute-$suite" 'compiled outputs present; tests actually execute' \
      ./dev nix-components run --directory "$evidence/$pass/components" \
        --suite "$suite" --report "$evidence/$pass/$suite.log"
  done
  record "$pass/hurl" "$pass" execute-hurl 'compiled testbed; actual PostgreSQL and Hurl fixtures' \
    ./dev nix-components hurl --directory "$evidence/$pass/components"
  record "$pass/cold-start" "$pass" execute-cold-start 'compiled testbed; actual readiness assertions' \
    ./dev nix-components cold-start --directory "$evidence/$pass/components"
  docker logs "$container" > "$evidence/$pass/postgres.log" 2>&1
  record "$pass/postgres-stop" "$pass" fixture-stop 'discard only this pass disposable database' \
    docker rm -f -v "$container"
  container=""
done
python3 - "$evidence" <<'PY'
import json, pathlib, sys
root = pathlib.Path(sys.argv[1])
passes = {}
for label in ('first', 'warm'):
    folder = root / label
    entries = [json.loads(line) for line in (folder/'components/timings.jsonl').read_text().splitlines()]
    suites = [{k: entry[k] for k in ('suite', 'examples', 'pending', 'executed')} for entry in entries if entry['action'] == 'run']
    if len(suites) != 5 or any(entry['exit_code'] for entry in entries):
        raise SystemExit('missing or failed component execution')
    audit = json.loads((folder/'cache-state.json').read_text())
    passes[label] = {'suites': suites, 'root': (folder/'components/root.txt').read_text().strip(),
                     'cache_comparison_unusable': audit['comparison_unusable']}
if passes['first']['suites'] != passes['warm']['suites'] or passes['first']['root'] != passes['warm']['root']:
    raise SystemExit('warm pass changed output paths or suite counts')
(root/'summary.json').write_text(json.dumps({'schema': 1, 'scope': 'five component suites + Hurl + cold-start; excludes codemap/doctest',
    'persistence': 'same job only', 'passes': passes}, indent=2)+'\n')
print(json.dumps(passes, indent=2))
PY
[[ "$(git rev-parse HEAD)" == "$revision" && -z "$(git status --porcelain --untracked-files=no)" ]]
