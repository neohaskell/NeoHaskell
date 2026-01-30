#!/usr/bin/env bash

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track testbed PID for cleanup
TESTBED_PID=""

cleanup() {
    if [ -n "$TESTBED_PID" ]; then
        echo ""
        echo "Stopping testbed (PID: $TESTBED_PID)..."
        kill "$TESTBED_PID" 2>/dev/null || true
        wait "$TESTBED_PID" 2>/dev/null || true
        echo "Testbed stopped."
    fi
}

trap cleanup EXIT

echo "Running NeoHaskell Testbed Tests..."
echo ""

# Check if hurl is installed
if ! command -v hurl &> /dev/null; then
    echo -e "${RED}Error: hurl is not installed${NC}"
    echo "Please install hurl: https://hurl.dev/docs/installation.html"
    exit 1
fi

# Start the testbed if not already running
if ! curl -s http://localhost:8080 > /dev/null 2>&1; then
    echo -e "${YELLOW}Starting testbed...${NC}"
    cabal run nhtestbed &
    TESTBED_PID=$!

    echo "Waiting for testbed to start (PID: $TESTBED_PID)..."
    for i in {1..30}; do
        if curl -s http://localhost:8080/queries/cart-summary > /dev/null 2>&1; then
            echo -e "${GREEN}Testbed is ready${NC}"
            break
        fi
        if [ $i -eq 30 ]; then
            echo -e "${RED}Error: Testbed failed to start within 30 seconds${NC}"
            exit 1
        fi
        sleep 1
    done
    echo ""
else
    echo -e "${GREEN}Testbed already running on http://localhost:8080${NC}"
    echo ""
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
    hurl --test testbed/tests/integrations/*.hurl
    echo ""
fi

echo -e "${GREEN}All tests completed!${NC}"
