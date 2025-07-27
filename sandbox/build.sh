#!/bin/bash

SANDBOX_DIR="$(pwd)"
PROJECT_ROOT="$(dirname "$SANDBOX_DIR")"
NEO_JSON_PATH="$SANDBOX_DIR/neo.json"

# Use nix to build the project
nix-build --show-trace -E "
let
  lib = import $PROJECT_ROOT/nix/lib.nix {};
in
  lib.buildNeoProject {
    neoJsonPath = $NEO_JSON_PATH;
    srcPath = $SANDBOX_DIR/src;
  }
"
