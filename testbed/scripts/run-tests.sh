#!/usr/bin/env bash

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "Running NeoHaskell Testbed Tests..."
echo ""

# Check if hurl is installed
if ! command -v hurl &> /dev/null; then
    echo -e "${RED}Error: hurl is not installed${NC}"
    echo "Please install hurl: https://hurl.dev/docs/installation.html"
    exit 1
fi

# Check if the testbed is running
if ! curl -s http://localhost:8080 > /dev/null 2>&1; then
    echo -e "${YELLOW}Warning: Testbed doesn't appear to be running on http://localhost:8080${NC}"
    echo "Please start it with: cabal run testbed"
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

echo -e "${GREEN}All tests completed!${NC}"
