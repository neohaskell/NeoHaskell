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

cleanup() {
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

cabal build exe:nhtestbed exe:nhoauthtestbed

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

# Start the testbed if not already running
if is_testbed_ready; then
	echo -e "${GREEN}Testbed already running on http://127.0.0.1:8080${NC}"
	echo ""
elif curl -s http://127.0.0.1:8080 >/dev/null 2>&1; then
	echo -e "${RED}Error: Port 8080 is in use, but NeoHaskell testbed endpoints are not available${NC}"
	echo "Please stop the process using port 8080 and run again."
	exit 1
else
	echo -e "${YELLOW}Starting testbed...${NC}"
	cabal run exe:nhtestbed &
	TESTBED_PID=$!
	wait_for_service http://127.0.0.1:8080/health "Testbed" "$TESTBED_PID"
	echo ""
fi

if is_oauth_testbed_ready; then
	echo -e "${GREEN}OAuth testbed already running on http://127.0.0.1:8081${NC}"
elif curl -s http://127.0.0.1:8081 >/dev/null 2>&1; then
	echo -e "${RED}Error: Port 8081 is in use, but NeoHaskell OAuth testbed endpoints are not available${NC}"
	exit 1
else
	echo -e "${YELLOW}Starting OAuth testbed...${NC}"
	cabal run exe:nhoauthtestbed &
	OAUTH_TESTBED_PID=$!
	wait_for_service http://127.0.0.1:8081/health "OAuth testbed" "$OAUTH_TESTBED_PID"
fi

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

echo -e "${GREEN}All tests completed!${NC}"
