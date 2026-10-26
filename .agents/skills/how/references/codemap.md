# Codemap

Run the owner command from the repository root:

    python3 .agents/skills/how/scripts/codemap.py map
    python3 .agents/skills/how/scripts/codemap.py map "service transport"
    python3 .agents/skills/how/scripts/codemap.py symbol Task.yield
    python3 .agents/skills/how/scripts/codemap.py callers Task.yield
    python3 .agents/skills/how/scripts/codemap.py api "Task Text ExecutionResult"
    python3 .agents/skills/how/scripts/codemap.py refresh
    python3 .agents/skills/how/scripts/codemap.py status

Every command hashes eligible current source content and refreshes the worktree-local SQLite index before answering. The inventory includes tracked files and nonignored untracked files under the Cabal, Nix, language, and maintained toolchain roots in codemap-catalog.json. It checks the inventory again before committing an index update. A source race, locked or corrupt cache, missing Git, or unwritable state directory produces a bounded current-source fallback with visible coverage limits.

The source index lives at .agents-state/how/codemap.sqlite3. Corrupt databases are moved to a timestamped file in the same directory before the current lookup rebuilds the index. Lookups do not run Cabal, Haddock, Nix, compiler builds, installation, or network requests.

The symbol command returns declared Haskell signatures and their module paths. The callers command returns lexical identifier matches from current Haskell source. These matches can include references that are not calls, and they do not form a complete call graph. The api command searches normalized declared signature text. It does not unify types.

If hiedb is on PATH or CODEMAP_HIEDB names an existing executable, symbol and callers also return compiler_results from .hie files under dist-newstyle. The worktree-local database lives at .agents-state/how/compiler.hiedb. Codemap hashes the artifact inventory and reindexes when an artifact changes or disappears. It bounds artifact scanning, output, and foreground time. HIE results contain module spans, not verified current source locations. Treat them as historical or freshness-unknown evidence. Codemap does not claim which source revision or build produced an HIE result. Read current source before using any result as evidence.

JSON responses for symbol and callers report current_source_truncated and compiler_results_truncated separately. The top-level truncated field is true when either result group reaches its limit.

The refresh command is a best-effort warm-up for callers that already run after successful development actions. Correctness does not depend on it. The status command verifies the current source inventory before reporting cache state. Symbol, callers, refresh, and status also reconcile local HIE artifacts when hiedb is available. Every command uses the same foreground deadline.

The script requires Python 3 and Git. HIE lookup is optional. If hiedb is unavailable or times out, codemap keeps the current-source results and reports the compiler-artifact status. Codemap never installs or provisions its own dependencies.
