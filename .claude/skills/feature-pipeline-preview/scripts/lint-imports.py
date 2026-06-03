#!/usr/bin/env python3
"""Lint Haskell source files for the import / IO-escape patterns the pipeline forbids.

Usage:
    lint-imports.py path/to/Module.hs [path/to/another.hs ...]

Exits nonzero if any anti-pattern is found. Output format:

    path:line: rule: snippet

Rules:
    hackage-import-without-ghc-prefix
        Imports of nhcore-replaceable Hackage modules (Control.Concurrent.Async,
        Data.Either, Data.IORef, Data.Map, Data.Map.Strict, Data.Vector, etc.)
        must be aliased with a `Ghc` prefix. Bare or differently-aliased
        imports are a finding.

    task-fromio-runresult-roundtrip
        `Task.fromIO (... Task.runResult ...)` pattern — escapes Task to use
        an IO library. Indicates a missing nhcore primitive.

This is a guide, not a hard gate by itself — the pipeline orchestrator reads
the output and decides.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path


# Modules that should always be aliased with the Ghc prefix when imported in
# nhcore application code. Add to this list as nhcore wraps more primitives.
FORBIDDEN_BARE_IMPORTS = [
    "Control.Concurrent.Async",
    "Control.Exception",
    "Data.Either",
    "Data.IORef",
    "Data.Map",
    "Data.Map.Strict",
    "Data.Set",
    "Data.Vector",
]


def lint_file(path: Path) -> list[tuple[Path, int, str, str]]:
    findings: list[tuple[Path, int, str, str]] = []
    text = path.read_text(encoding="utf-8", errors="replace")
    lines = text.splitlines()

    # Rule: hackage-import-without-ghc-prefix
    for idx, line in enumerate(lines, start=1):
        # match: import [qualified] <Mod> [as <Alias>]
        m = re.match(
            r"^\s*import\s+(qualified\s+)?([A-Za-z0-9_.]+)(\s+qualified)?(?:\s+as\s+([A-Za-z0-9_]+))?",
            line,
        )
        if not m:
            continue
        mod = m.group(2)
        alias = m.group(4)
        if mod not in FORBIDDEN_BARE_IMPORTS:
            continue
        if alias is None or not alias.startswith("Ghc"):
            findings.append(
                (
                    path,
                    idx,
                    "hackage-import-without-ghc-prefix",
                    line.strip(),
                )
            )

    # Rule: task-fromio-runresult-roundtrip
    # Look for Task.fromIO and Task.runResult appearing within the same
    # expression span (10-line window heuristic).
    fromio_re = re.compile(r"\bTask\.fromIO\b")
    runresult_re = re.compile(r"\bTask\.runResult\b")
    for idx, line in enumerate(lines, start=1):
        if not fromio_re.search(line):
            continue
        window = "\n".join(lines[idx - 1 : min(len(lines), idx + 10)])
        if runresult_re.search(window):
            findings.append(
                (
                    path,
                    idx,
                    "task-fromio-runresult-roundtrip",
                    line.strip(),
                )
            )

    return findings


def main(argv: list[str]) -> int:
    if len(argv) < 2:
        print(__doc__, file=sys.stderr)
        return 2
    all_findings: list[tuple[Path, int, str, str]] = []
    for p in argv[1:]:
        path = Path(p)
        if not path.exists():
            print(f"{p}: file not found", file=sys.stderr)
            return 2
        all_findings.extend(lint_file(path))
    for p, line, rule, snippet in all_findings:
        print(f"{p}:{line}: {rule}: {snippet}")
    return 1 if all_findings else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
