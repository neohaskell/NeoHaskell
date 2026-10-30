#!/usr/bin/env bash

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track testbed PID for cleanup
TESTBED_PID=""
OAUTH_TESTBED_PID=""
FAKE_SERVICES_PID=""
ERASURE_STATE_FILE=""
ERASURE_COMMIT_MARKER=""
ERASURE_PAUSE_SELECTOR=""
ERASURE_PRE_ACTION_SELECTOR=""
ERASURE_PRE_ACTION_MARKER=""
OAUTH_TESTBED_LOG=""
HURL_TEMP_DIR=""
TESTBED_HTTP_PORT="${TESTBED_HTTP_PORT:-8080}"
TESTBED_OAUTH_PORT="${TESTBED_OAUTH_PORT:-8081}"
TESTBED_FIXTURE_PORT="${TESTBED_FIXTURE_PORT:-8181}"
export TESTBED_OAUTH_PORT TESTBED_FIXTURE_PORT
HURL_TEST_ROOT="testbed/tests"
CABAL_EXE_OPTIONS=()
CABAL_GLOBAL_OPTIONS=()
if [ -n "${TESTBED_CABAL_BUILDDIR:-}" ]; then
	CABAL_GLOBAL_OPTIONS+=(--builddir="$TESTBED_CABAL_BUILDDIR")
fi
if [ "$(uname)" = "Darwin" ]; then
	CABAL_EXE_OPTIONS+=(--enable-executable-dynamic)
fi

cleanup() {
	if [ -n "$ERASURE_STATE_FILE" ]; then
		rm -f "$ERASURE_STATE_FILE"
	fi
	if [ -n "$ERASURE_COMMIT_MARKER" ]; then
		rm -f "$ERASURE_COMMIT_MARKER"
	fi
	if [ -n "$ERASURE_PAUSE_SELECTOR" ]; then
		rm -f "$ERASURE_PAUSE_SELECTOR"
	fi
	if [ -n "$ERASURE_PRE_ACTION_SELECTOR" ]; then
		rm -f "$ERASURE_PRE_ACTION_SELECTOR"
	fi
	if [ -n "$ERASURE_PRE_ACTION_MARKER" ]; then
		rm -f "$ERASURE_PRE_ACTION_MARKER"
	fi
	if [ -n "$HURL_TEMP_DIR" ]; then
		rm -rf "$HURL_TEMP_DIR"
	fi
	for pid in "$OAUTH_TESTBED_PID" "$TESTBED_PID" "$FAKE_SERVICES_PID"; do
		if [ -n "$pid" ]; then
			kill "$pid" 2>/dev/null || true
			wait "$pid" 2>/dev/null || true
		fi
	done
}

trap cleanup EXIT

echo "Running NeoHaskell Testbed Tests..."
echo ""

is_testbed_ready() {
	curl -sf "http://127.0.0.1:$TESTBED_HTTP_PORT/health" >/dev/null 2>&1
}

is_oauth_testbed_ready() {
	curl -sf "http://127.0.0.1:$TESTBED_OAUTH_PORT/health" >/dev/null 2>&1
}

is_fake_services_ready() {
	curl -sf "http://127.0.0.1:$TESTBED_FIXTURE_PORT/health" 2>/dev/null | grep -q '"fixture":"neohaskell-testbed"'
}

wait_for_service() {
	endpoint="$1"
	label="$2"
	pid="$3"
	for i in {1..30}; do
		if curl -sf "$endpoint" >/dev/null 2>&1; then
			echo -e "${GREEN}${label} is ready${NC}"
			return 0
		fi
		if ! kill -0 "$pid" 2>/dev/null; then
			echo -e "${RED}Error: ${label} exited before becoming ready${NC}"
			return 1
		fi
		sleep 1
	done
	echo -e "${RED}Error: ${label} failed to start within 30 seconds${NC}"
	return 1
}

# Check if hurl is installed
if ! command -v hurl &>/dev/null; then
	echo -e "${RED}Error: hurl is not installed${NC}"
	echo "Please install hurl: https://hurl.dev/docs/installation.html"
	exit 1
fi

if [ "$TESTBED_HTTP_PORT:$TESTBED_OAUTH_PORT:$TESTBED_FIXTURE_PORT" != "8080:8081:8181" ]; then
	HURL_TEMP_DIR=$(mktemp -d)
	cp -R testbed/tests/. "$HURL_TEMP_DIR/"
	python3 - "$HURL_TEMP_DIR" "$TESTBED_HTTP_PORT" "$TESTBED_OAUTH_PORT" "$TESTBED_FIXTURE_PORT" <<'PY'
from pathlib import Path
import sys

root = Path(sys.argv[1])
for path in root.rglob("*.hurl"):
    content = path.read_text()
    for old, new in zip(("8080", "8081", "8181"), sys.argv[2:]):
        content = content.replace(f":{old}", f":{new}")
    path.write_text(content)
PY
	HURL_TEST_ROOT="$HURL_TEMP_DIR"
fi

cabal "${CABAL_GLOBAL_OPTIONS[@]}" build "${CABAL_EXE_OPTIONS[@]}" exe:nhtestbed exe:nhoauthtestbed

if is_testbed_ready || curl -s "http://127.0.0.1:$TESTBED_HTTP_PORT" >/dev/null 2>&1; then
	echo -e "${RED}Error: Port $TESTBED_HTTP_PORT is already in use${NC}"
	exit 1
fi

if is_oauth_testbed_ready || curl -s "http://127.0.0.1:$TESTBED_OAUTH_PORT" >/dev/null 2>&1; then
	echo -e "${RED}Error: Port $TESTBED_OAUTH_PORT is already in use${NC}"
	exit 1
fi

if is_fake_services_ready; then
	echo -e "${GREEN}Fake services already running on http://127.0.0.1:$TESTBED_FIXTURE_PORT${NC}"
elif curl -s "http://127.0.0.1:$TESTBED_FIXTURE_PORT" >/dev/null 2>&1; then
	echo -e "${RED}Error: Port $TESTBED_FIXTURE_PORT is in use, but the testbed fake services are unavailable${NC}"
	exit 1
else
	echo -e "${YELLOW}Starting fake external services...${NC}"
	python3 testbed/scripts/fake_external_services.py &
	FAKE_SERVICES_PID=$!
	wait_for_service "http://127.0.0.1:$TESTBED_FIXTURE_PORT/health" "Fake services" "$FAKE_SERVICES_PID"
fi

export OAUTH2_TESTBED_STATE_KEY="${OAUTH2_TESTBED_STATE_KEY:-testbed-oauth-state-key-that-is-at-least-32-bytes}"
ERASURE_COMMIT_MARKER=$(mktemp)
rm -f "$ERASURE_COMMIT_MARKER"
export CRYPTOERASURE_COMMIT_MARKER="$ERASURE_COMMIT_MARKER"
ERASURE_PAUSE_SELECTOR=$(mktemp)
rm -f "$ERASURE_PAUSE_SELECTOR"
export CRYPTOERASURE_PAUSE_SELECTOR="$ERASURE_PAUSE_SELECTOR"
ERASURE_PRE_ACTION_SELECTOR=$(mktemp)
rm -f "$ERASURE_PRE_ACTION_SELECTOR"
export CRYPTOERASURE_PRE_ACTION_SELECTOR="$ERASURE_PRE_ACTION_SELECTOR"
ERASURE_PRE_ACTION_MARKER=$(mktemp)
rm -f "$ERASURE_PRE_ACTION_MARKER"
export CRYPTOERASURE_PRE_ACTION_MARKER="$ERASURE_PRE_ACTION_MARKER"

export PGPASSWORD="${DB_PASSWORD:-neohaskell}"
database_suffix=$(python3 -c 'import uuid; print(uuid.uuid4().hex[:12])')
testbed_db="nh_testbed_${database_suffix}"
oauth_db="nh_oauth_${database_suffix}"
createdb -h "${DB_HOST:-localhost}" -p "${DB_PORT:-5432}" -U "${DB_USER:-neohaskell}" "$testbed_db"
createdb -h "${DB_HOST:-localhost}" -p "${DB_PORT:-5432}" -U "${DB_USER:-neohaskell}" "$oauth_db"
echo "Testbed databases: $testbed_db $oauth_db"

echo -e "${YELLOW}Starting testbed...${NC}"
DB_NAME="$testbed_db" "$(cabal "${CABAL_GLOBAL_OPTIONS[@]}" list-bin "${CABAL_EXE_OPTIONS[@]}" exe:nhtestbed)" &
TESTBED_PID=$!
wait_for_service "http://127.0.0.1:$TESTBED_HTTP_PORT/health" "Testbed" "$TESTBED_PID"
echo ""

echo -e "${YELLOW}Starting OAuth testbed...${NC}"
oauth_log_dir="${TESTBED_OAUTH_LOG_DIR:-${TMPDIR:-/tmp}}"
mkdir -p "$oauth_log_dir"
OAUTH_TESTBED_LOG=$(mktemp "$oauth_log_dir/oauth-cryptoerasure-XXXXXX")
DB_NAME="$oauth_db" "$(cabal "${CABAL_GLOBAL_OPTIONS[@]}" list-bin "${CABAL_EXE_OPTIONS[@]}" exe:nhoauthtestbed)" >>"$OAUTH_TESTBED_LOG" 2>&1 &
OAUTH_TESTBED_PID=$!
wait_for_service "http://127.0.0.1:$TESTBED_OAUTH_PORT/health" "OAuth testbed" "$OAUTH_TESTBED_PID"

# Run command tests
if [ -d "$HURL_TEST_ROOT/commands" ] && [ "$(ls -A "$HURL_TEST_ROOT"/commands/*.hurl 2>/dev/null)" ]; then
	echo -e "${GREEN}Running command tests...${NC}"
	hurl --test "$HURL_TEST_ROOT"/commands/*.hurl
	echo ""
fi

# Run query tests
if [ -d "$HURL_TEST_ROOT/queries" ] && [ "$(ls -A "$HURL_TEST_ROOT"/queries/*.hurl 2>/dev/null)" ]; then
	echo -e "${GREEN}Running query tests...${NC}"
	hurl --test "$HURL_TEST_ROOT"/queries/*.hurl
	echo ""
fi

# Run scenario tests
if [ -d "$HURL_TEST_ROOT/scenarios" ] && [ "$(ls -A "$HURL_TEST_ROOT"/scenarios/*.hurl 2>/dev/null)" ]; then
	echo -e "${GREEN}Running scenario tests...${NC}"
	hurl --test "$HURL_TEST_ROOT"/scenarios/*.hurl
	echo ""
fi

# Run integration tests (PDF extraction, etc.)
if [ -d "$HURL_TEST_ROOT/integrations" ] && [ "$(ls -A "$HURL_TEST_ROOT"/integrations/*.hurl 2>/dev/null)" ]; then
	echo -e "${GREEN}Running integration tests...${NC}"
	delivered_effect_id=$(python3 -c 'import uuid; print(uuid.uuid4())')
	failed_effect_id=$(python3 -c 'import uuid; print(uuid.uuid4())')
	echo "Generic HTTP effect IDs: $delivered_effect_id $failed_effect_id"
	hurl --test --variable "delivered_effect_id=$delivered_effect_id" --variable "failed_effect_id=$failed_effect_id" "$HURL_TEST_ROOT"/integrations/*.hurl
	echo ""
fi

bash testbed/scripts/verify-database-isolation.sh "$testbed_db" "$oauth_db"

python3 testbed/scripts/verify-cryptoerasure-restart.py concurrent "$oauth_db"
ERASURE_STATE_FILE=$(mktemp)
python3 testbed/scripts/verify-cryptoerasure-restart.py prepare "$ERASURE_STATE_FILE"
python3 testbed/scripts/verify-cryptoerasure-restart.py verify-callback-log "$ERASURE_STATE_FILE" "$OAUTH_TESTBED_LOG"
restart_oauth_testbed() {
	kill "$OAUTH_TESTBED_PID"
	wait "$OAUTH_TESTBED_PID" 2>/dev/null || true
	OAUTH_TESTBED_PID=""
	if is_oauth_testbed_ready; then
		echo -e "${RED}Error: OAuth testbed still responds after its owned process stopped${NC}"
		exit 1
	fi
	DB_NAME="$oauth_db" "$(cabal "${CABAL_GLOBAL_OPTIONS[@]}" list-bin "${CABAL_EXE_OPTIONS[@]}" exe:nhoauthtestbed)" >>"$OAUTH_TESTBED_LOG" 2>&1 &
	OAUTH_TESTBED_PID=$!
	wait_for_service "http://127.0.0.1:$TESTBED_OAUTH_PORT/health" "Restarted OAuth testbed" "$OAUTH_TESTBED_PID"
}
restart_oauth_testbed
python3 testbed/scripts/verify-cryptoerasure-restart.py verify "$ERASURE_STATE_FILE"
python3 testbed/scripts/verify-cryptoerasure-restart.py prepare-crash "$ERASURE_STATE_FILE" "$ERASURE_COMMIT_MARKER" "$ERASURE_PAUSE_SELECTOR"
rm -f "$ERASURE_PAUSE_SELECTOR"
restart_oauth_testbed
python3 testbed/scripts/verify-cryptoerasure-restart.py verify "$ERASURE_STATE_FILE"
restart_oauth_testbed
python3 testbed/scripts/verify-cryptoerasure-restart.py verify-completed "$ERASURE_STATE_FILE" "$oauth_db"
python3 testbed/scripts/verify-cryptoerasure-restart.py prepare-pre-action "$ERASURE_STATE_FILE" "$ERASURE_PRE_ACTION_MARKER" "$ERASURE_PRE_ACTION_SELECTOR"
rm -f "$ERASURE_PRE_ACTION_SELECTOR"
restart_oauth_testbed
python3 testbed/scripts/verify-cryptoerasure-restart.py verify-pre-action "$ERASURE_STATE_FILE" "$oauth_db"

if [ "${CRYPTOERASURE_FAULT_TESTS:-0}" = "1" ]; then
	: "${TESTBED_FAULT_HTTP_PORT:?Set a private fault HTTP port}"
	: "${TESTBED_FAULT_FIXTURE_PORT:?Set a private fault fixture port}"
	fault_count=0
	for fault in missing-key version tag missing-envelope bad-nonce bad-ciphertext unavailable-store; do
		DB_HOST="${DB_HOST:-127.0.0.1}" DB_PASSWORD="${DB_PASSWORD:-neohaskell}" \
			python3 testbed/scripts/verify-cryptoerasure-faults.py "$fault"
		fault_count=$((fault_count + 1))
	done
	test "$fault_count" -eq 7
	echo "Cryptoerasure fault cases: $fault_count"
	DB_HOST="${DB_HOST:-127.0.0.1}" DB_PASSWORD="${DB_PASSWORD:-neohaskell}" \
		python3 testbed/scripts/verify-cryptoerasure-replay-overlap.py
fi

echo -e "${GREEN}All tests completed!${NC}"
