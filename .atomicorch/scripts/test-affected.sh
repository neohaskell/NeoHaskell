#!/usr/bin/env bash
set -euo pipefail

# Determines affected test suites based on changed files
# and runs them. Returns exit 0 only if all pass.

CHANGED=$(git diff --name-only HEAD 2>/dev/null || git diff --name-only)

if echo "$CHANGED" | grep -q "^core/core/"; then
    cabal test nhcore-test-core
fi
if echo "$CHANGED" | grep -q "^core/auth/"; then
    cabal test nhcore-test-auth
fi
if echo "$CHANGED" | grep -q "^core/service/"; then
    cabal test nhcore-test-service
fi
if echo "$CHANGED" | grep -q "^testbed/"; then
    cabal test nhcore-test-integration
fi
