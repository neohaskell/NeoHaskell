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
CABAL_EXE_OPTIONS=()
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
	curl -sf http://127.0.0.1:8080/health >/dev/null 2>&1
}

is_oauth_testbed_ready() {
	curl -sf http://127.0.0.1:8081/health >/dev/null 2>&1
}

is_fake_services_ready() {
	curl -sf http://127.0.0.1:8181/health 2>/dev/null | grep -q '"fixture":"neohaskell-testbed"'
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

cabal build "${CABAL_EXE_OPTIONS[@]}" exe:nhtestbed exe:nhoauthtestbed

if is_testbed_ready || curl -s http://127.0.0.1:8080 >/dev/null 2>&1; then
	echo -e "${RED}Error: Port 8080 is already in use${NC}"
	exit 1
fi

if is_oauth_testbed_ready || curl -s http://127.0.0.1:8081 >/dev/null 2>&1; then
	echo -e "${RED}Error: Port 8081 is already in use${NC}"
	exit 1
fi

if is_fake_services_ready; then
	echo -e "${GREEN}Fake services already running on http://127.0.0.1:8181${NC}"
elif curl -s http://127.0.0.1:8181 >/dev/null 2>&1; then
	echo -e "${RED}Error: Port 8181 is in use, but the testbed fake services are unavailable${NC}"
	exit 1
else
	echo -e "${YELLOW}Starting fake external services...${NC}"
	python3 testbed/scripts/fake_external_services.py &
	FAKE_SERVICES_PID=$!
	wait_for_service http://127.0.0.1:8181/health "Fake services" "$FAKE_SERVICES_PID"
fi

export OAUTH2_TESTBED_STATE_KEY="${OAUTH2_TESTBED_STATE_KEY:-testbed-oauth-state-key-that-is-at-least-32-bytes}"
ERASURE_COMMIT_MARKER=$(mktemp)
rm -f "$ERASURE_COMMIT_MARKER"
export CRYPTOERASURE_COMMIT_MARKER="$ERASURE_COMMIT_MARKER"

export PGPASSWORD="${DB_PASSWORD:-neohaskell}"
database_suffix=$(python3 -c 'import uuid; print(uuid.uuid4().hex[:12])')
testbed_db="nh_testbed_${database_suffix}"
oauth_db="nh_oauth_${database_suffix}"
createdb -h "${DB_HOST:-localhost}" -p "${DB_PORT:-5432}" -U "${DB_USER:-neohaskell}" "$testbed_db"
createdb -h "${DB_HOST:-localhost}" -p "${DB_PORT:-5432}" -U "${DB_USER:-neohaskell}" "$oauth_db"
echo "Testbed databases: $testbed_db $oauth_db"

echo -e "${YELLOW}Starting testbed...${NC}"
DB_NAME="$testbed_db" "$(cabal list-bin "${CABAL_EXE_OPTIONS[@]}" exe:nhtestbed)" &
TESTBED_PID=$!
wait_for_service http://127.0.0.1:8080/health "Testbed" "$TESTBED_PID"
echo ""

echo -e "${YELLOW}Starting OAuth testbed...${NC}"
DB_NAME="$oauth_db" "$(cabal list-bin "${CABAL_EXE_OPTIONS[@]}" exe:nhoauthtestbed)" &
OAUTH_TESTBED_PID=$!
wait_for_service http://127.0.0.1:8081/health "OAuth testbed" "$OAUTH_TESTBED_PID"

# Run command tests
if [ -d "testbed/tests/commands" ] && [ "$(ls -A testbed/tests/commands/*.hurl 2>/dev/null)" ]; then
	echo -e "${GREEN}Running command tests...${NC}"
	hurl --test testbed/tests/commands/*.hurl
	echo ""
fi

# Run query tests
if [ -d "testbed/tests/queries" ] && [ "$(ls -A testbed/tests/queries/*.hurl 2>/dev/null)" ]; then
	echo -e "${GREEN}Running query tests...${NC}"
	hurl --test testbed/tests/queries/*.hurl
	echo ""
fi

# Run scenario tests
if [ -d "testbed/tests/scenarios" ] && [ "$(ls -A testbed/tests/scenarios/*.hurl 2>/dev/null)" ]; then
	echo -e "${GREEN}Running scenario tests...${NC}"
	hurl --test testbed/tests/scenarios/*.hurl
	echo ""
fi

# Run integration tests (PDF extraction, etc.)
if [ -d "testbed/tests/integrations" ] && [ "$(ls -A testbed/tests/integrations/*.hurl 2>/dev/null)" ]; then
	echo -e "${GREEN}Running integration tests...${NC}"
	delivered_effect_id=$(python3 -c 'import uuid; print(uuid.uuid4())')
	failed_effect_id=$(python3 -c 'import uuid; print(uuid.uuid4())')
	echo "Generic HTTP effect IDs: $delivered_effect_id $failed_effect_id"
	hurl --test --variable "delivered_effect_id=$delivered_effect_id" --variable "failed_effect_id=$failed_effect_id" testbed/tests/integrations/*.hurl
	echo ""
fi

bash testbed/scripts/verify-database-isolation.sh "$testbed_db" "$oauth_db"

python3 testbed/scripts/verify-cryptoerasure-restart.py concurrent
ERASURE_STATE_FILE=$(mktemp)
python3 testbed/scripts/verify-cryptoerasure-restart.py prepare "$ERASURE_STATE_FILE"
restart_oauth_testbed() {
	kill "$OAUTH_TESTBED_PID"
	wait "$OAUTH_TESTBED_PID" 2>/dev/null || true
	OAUTH_TESTBED_PID=""
	if is_oauth_testbed_ready; then
		echo -e "${RED}Error: OAuth testbed still responds after its owned process stopped${NC}"
		exit 1
	fi
	DB_NAME="$oauth_db" "$(cabal list-bin "${CABAL_EXE_OPTIONS[@]}" exe:nhoauthtestbed)" &
	OAUTH_TESTBED_PID=$!
	wait_for_service http://127.0.0.1:8081/health "Restarted OAuth testbed" "$OAUTH_TESTBED_PID"
}
restart_oauth_testbed
python3 testbed/scripts/verify-cryptoerasure-restart.py verify "$ERASURE_STATE_FILE"
python3 testbed/scripts/verify-cryptoerasure-restart.py prepare-crash "$ERASURE_STATE_FILE" "$ERASURE_COMMIT_MARKER"
restart_oauth_testbed
python3 testbed/scripts/verify-cryptoerasure-restart.py verify "$ERASURE_STATE_FILE"
restart_oauth_testbed
python3 testbed/scripts/verify-cryptoerasure-restart.py verify-completed "$ERASURE_STATE_FILE"

echo -e "${GREEN}All tests completed!${NC}"
