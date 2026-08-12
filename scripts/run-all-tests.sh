#!/usr/bin/env bash

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

FAILED=0
PASSED=0

run_suite() {
	local name="$1"
	local cmd="$2"
	echo -e "${BLUE}━━━ $name ━━━${NC}"
	# NOTE: `((VAR++))` returns the PRE-increment value as its exit status, so
	# the first increment (0 -> 1) exits 1 and `set -e` killed the whole run
	# right after the first passing suite. Arithmetic assignment always
	# succeeds, so the runner now reaches every suite.
	if eval "$cmd"; then
		echo -e "${GREEN}✓ $name passed${NC}"
		echo ""
		PASSED=$((PASSED + 1))
	else
		echo -e "${RED}✗ $name failed${NC}"
		echo ""
		FAILED=$((FAILED + 1))
	fi
}

echo -e "${BLUE}╔══════════════════════════════════════╗${NC}"
echo -e "${BLUE}║   NeoHaskell — Full Test Suite       ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════╝${NC}"
echo ""

# Phase 1: Build
echo -e "${YELLOW}▸ Building...${NC}"
cabal build all
echo -e "${GREEN}✓ Build succeeded${NC}"
echo ""

# Phase 2: Unit test suites (no Postgres)
echo -e "${YELLOW}▸ Unit tests (no Postgres required)${NC}"
echo ""
run_suite "nhcore-test-core" "LOG_LEVEL=Warn cabal test nhcore-test-core"
run_suite "nhcore-test-auth" "LOG_LEVEL=Warn cabal test nhcore-test-auth"
run_suite "nhcore-test-integration" "LOG_LEVEL=Warn cabal test nhcore-test-integration"

# Phase 3: Service tests (needs Postgres)
echo -e "${YELLOW}▸ Service tests (requires Postgres on localhost:5432)${NC}"
echo ""
if pg_isready -h 127.0.0.1 -U neohaskell -q 2>/dev/null; then
	run_suite "nhcore-test-service" "LOG_LEVEL=Warn cabal test nhcore-test-service"
else
	echo -e "${YELLOW}⚠ Postgres not available — skipping nhcore-test-service${NC}"
	echo -e "  Start it with:"
	echo -e "  docker run -d --name neohaskell-postgres \\"
	echo -e "    -e POSTGRES_USER=neohaskell -e POSTGRES_PASSWORD=neohaskell \\"
	echo -e "    -e POSTGRES_DB=neohaskell -p 5432:5432 postgres:16-alpine"
	echo ""
fi

# Phase 4: Hurl integration tests (needs Postgres + testbed)
echo -e "${YELLOW}▸ Hurl integration tests (requires Postgres + testbed)${NC}"
echo ""
if command -v hurl &>/dev/null && pg_isready -h 127.0.0.1 -U neohaskell -q 2>/dev/null; then
	run_suite "Hurl integration tests" "./testbed/scripts/run-tests.sh"
else
	if ! command -v hurl &>/dev/null; then
		echo -e "${YELLOW}⚠ hurl not installed — skipping integration tests${NC}"
		echo -e "  Install: https://hurl.dev/docs/installation.html"
	else
		echo -e "${YELLOW}⚠ Postgres not available — skipping integration tests${NC}"
	fi
	echo ""
fi

# Phase 5: Lint
echo -e "${YELLOW}▸ Linting${NC}"
echo ""
run_suite "hlint" "hlint core/ testbed/src/"

# Summary
echo -e "${BLUE}╔══════════════════════════════════════╗${NC}"
echo -e "${BLUE}║   Results                            ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════╝${NC}"
echo -e "  ${GREEN}Passed: $PASSED${NC}"
if [ "$FAILED" -gt 0 ]; then
	echo -e "  ${RED}Failed: $FAILED${NC}"
	exit 1
else
	echo -e "  ${RED}Failed: 0${NC}"
	echo -e "${GREEN}All tests passed!${NC}"
fi
