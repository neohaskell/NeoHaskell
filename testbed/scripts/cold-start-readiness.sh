#!/usr/bin/env bash

set -euo pipefail

readonly health_budget_ms=5000
readonly flatness_budget_ms=2000
readonly ready_budget_ms=120000
readonly bootstrap_budget_ms=1200000
readonly port=8080
readonly base_url="http://[::1]:${port}"
cabal build exe:nhtestbed >/dev/null
readonly app_binary="$(cabal list-bin exe:nhtestbed)"
readonly sizes=(1000 10000 100000)
readonly log_dir="${TMPDIR:-/tmp}/neohaskell-cold-start"

app_pid=""

cleanup() {
  if [[ -n "$app_pid" ]]; then
    kill "$app_pid" 2>/dev/null || true
    wait "$app_pid" 2>/dev/null || true
    app_pid=""
  fi
}

trap cleanup EXIT
mkdir -p "$log_dir"

now_ms() {
  python3 -c 'import time; print(int(time.monotonic() * 1000))'
}

wait_for_log() {
  local pattern="$1"
  local log_file="$2"
  local budget_ms="$3"
  local started
  started="$(now_ms)"
  while true; do
    if grep -q "$pattern" "$log_file"; then
      return 0
    fi
    if (( $(now_ms) - started >= budget_ms )); then
      echo "Timed out waiting for log field ${pattern} in ${log_file}" >&2
      return 1
    fi
    sleep 0.05
  done
}

wait_for_status() {
  local path="$1"
  local expected="$2"
  local budget_ms="$3"
  local started
  started="$(now_ms)"
  while true; do
    local status
    status="$(curl --noproxy '*' -g -sS -o /dev/null -w '%{http_code}' "${base_url}${path}" 2>/dev/null || true)"
    if [[ "$status" == "$expected" ]]; then
      return 0
    fi
    if (( $(now_ms) - started >= budget_ms )); then
      echo "Timed out waiting for ${path}=${expected}; last status=${status}" >&2
      return 1
    fi
    sleep 0.05
  done
}

start_app() {
  local log_file="$1"
  "$app_binary" >"$log_file" 2>&1 &
  app_pid=$!
}

stop_app() {
  cleanup
  for _ in $(seq 1 100); do
    if ! curl --noproxy '*' -g -sS "${base_url}/health" >/dev/null 2>&1; then
      return 0
    fi
    sleep 0.05
  done
  echo "Testbed did not release port ${port}" >&2
  return 1
}

if curl --noproxy '*' -g -sS "${base_url}/health" >/dev/null 2>&1; then
  echo "Cold-start test port ${port} is already in use" >&2
  exit 1
fi

export PGPASSWORD="${PGPASSWORD:-neohaskell}"
readonly psql_args=(-h "${POSTGRES_HOST:-127.0.0.1}" -U "${POSTGRES_USER:-neohaskell}" -d "${POSTGRES_DB:-neohaskell}" -v ON_ERROR_STOP=1)

# Bootstrap one canonical event through the public command endpoint. Subsequent
# fixture sizes duplicate its validated eventData/metadata while assigning
# independent database IDs and monotonically increasing stream positions.
start_app "$log_dir/bootstrap.log"
wait_for_status /health 200 "$bootstrap_budget_ms"
psql "${psql_args[@]}" -c 'TRUNCATE TABLE events RESTART IDENTITY'
curl --noproxy '*' -g -fsS -X POST -H 'Content-Type: application/json' -d '[]' "${base_url}/commands/create-cart" >/dev/null
stop_app

psql "${psql_args[@]}" <<'SQL'
DROP TABLE IF EXISTS cold_start_event_template;
CREATE TABLE cold_start_event_template AS
SELECT eventData, metadata, inlinedStreamId, entity
FROM events
ORDER BY globalPosition
LIMIT 1;
SQL

latencies=()
for size in "${sizes[@]}"; do
  psql "${psql_args[@]}" -v event_count="$size" <<'SQL'
TRUNCATE TABLE events RESTART IDENTITY;
INSERT INTO events (eventId, localPosition, inlinedStreamId, entity, eventData, metadata)
SELECT md5('cold-start-' || i::text)::uuid,
       i,
       template.inlinedStreamId,
       template.entity,
       template.eventData,
       template.metadata
FROM cold_start_event_template AS template
CROSS JOIN generate_series(0, :event_count - 1) AS i;
SQL

  log_file="$log_dir/${size}.log"
  started="$(now_ms)"
  start_app "$log_file"
  wait_for_status /health 200 "$health_budget_ms"
  elapsed=$(( $(now_ms) - started ))
  latencies+=("$elapsed")
  if (( elapsed > health_budget_ms )); then
    echo "/health took ${elapsed}ms for ${size} events; budget is ${health_budget_ms}ms" >&2
    exit 1
  fi

  hurl --test --ipv6 testbed/tests/scenarios/cold-start-readiness.hurl
  ready_status="$(curl --noproxy '*' -g -sS -o /dev/null -w '%{http_code}' "${base_url}/ready" 2>/dev/null || true)"
  if [[ "$ready_status" != "503" ]]; then
    echo "/ready was ${ready_status} when /health first bound for ${size} events; expected 503" >&2
    exit 1
  fi

  if [[ "$size" == "1000" ]]; then
    wait_for_status /ready 200 "$ready_budget_ms"
    curl --noproxy '*' -g -fsS "${base_url}/queries/cart-summary" | python3 -c 'import json,sys; body=json.load(sys.stdin); assert body["total"] >= 1'
    wait_for_log 'events_replayed' "$log_file" 5000
    wait_for_log 'lag_from_head' "$log_file" 5000
    wait_for_log 'duration_seconds' "$log_file" 5000
    if grep -Eq 'postgres(ql)?://|password[=:]' "$log_file"; then
      echo "Cold-start log exposed a connection string or password" >&2
      exit 1
    fi
  fi

  stop_app
done

min_latency="${latencies[0]}"
max_latency="${latencies[0]}"
for latency in "${latencies[@]}"; do
  (( latency < min_latency )) && min_latency="$latency"
  (( latency > max_latency )) && max_latency="$latency"
done
spread=$((max_latency - min_latency))
if (( spread > flatness_budget_ms )); then
  echo "Health-to-bind spread was ${spread}ms (${latencies[*]}); budget is ${flatness_budget_ms}ms" >&2
  exit 1
fi

psql "${psql_args[@]}" -c 'DROP TABLE IF EXISTS cold_start_event_template'
echo "cold-start readiness: health latencies ${latencies[*]}ms; spread ${spread}ms"
