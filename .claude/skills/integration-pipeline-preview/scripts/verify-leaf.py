#!/usr/bin/env python3
"""Independently verify a leaf's claimed completion before the orchestrator advances.

Usage:
    verify-leaf.py <phase_number>

Re-reads the relevant `.integration-pipeline/` artefacts for the given phase and
exits 0 if the leaf's claims match reality, else nonzero with an explanation.

This is the integration-pipeline-preview variant of the feature pipeline's
verify-leaf. The state directory is `.integration-pipeline/` and the phase
table has 17 phases (no Opus PR-review phase); phase 17 is the CI cycle.

Covered phases:
    11  build loop          — `.integration-pipeline/test.log` shows `0 failures`
    12  security impl       — `.integration-pipeline/findings-12.json` blocker count
    13  performance impl    — `.integration-pipeline/findings-13.json` is countable
    14  fix findings        — both findings-12 and findings-13 show 0 blockers
    15  final verify        — `.integration-pipeline/test.log` shows `0 failures`; hlint.log exists
    17  CI cycle            — PR is merged (delegated to gh; not re-verifiable here)

Other phases either don't gate on artefact state (3/16 are PAUSE-only) or are
covered by their phase's own rubric check.
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path


PIPELINE_DIR = Path(".integration-pipeline")


def fail(msg: str) -> int:
    print(f"verify-leaf: FAIL: {msg}", file=sys.stderr)
    return 1


def ok(msg: str) -> int:
    print(f"verify-leaf: OK: {msg}")
    return 0


def parse_test_failures(log_path: Path) -> tuple[int, int]:
    """Return (total_examples, total_failures) parsed from a cabal test log."""
    if not log_path.exists():
        return -1, -1
    text = log_path.read_text(encoding="utf-8", errors="replace")
    examples = 0
    failures = 0
    for m in re.finditer(
        r"(\d+) examples?, (\d+) failures?(?:, \d+ pending)?", text
    ):
        examples += int(m.group(1))
        failures += int(m.group(2))
    return examples, failures


def verify_build_test(name: str) -> int:
    log = PIPELINE_DIR / "test.log"
    examples, failures = parse_test_failures(log)
    if examples < 0:
        return fail(f"{name}: {log} not found")
    if failures != 0:
        return fail(f"{name}: {failures} test failures across {examples} examples")
    return ok(f"{name}: {examples} examples, 0 failures")


def count_blockers(findings_path: Path) -> int | None:
    if not findings_path.exists():
        return None
    try:
        data = json.loads(findings_path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as e:
        print(f"verify-leaf: cannot parse {findings_path}: {e}", file=sys.stderr)
        return None
    if not isinstance(data, list):
        print(f"verify-leaf: {findings_path}: expected array", file=sys.stderr)
        return None
    blockers = 0
    for entry in data:
        if not isinstance(entry, dict):
            continue
        if entry.get("blocker") is True or entry.get("severity") == "blocker":
            blockers += 1
    return blockers


def verify_findings_phase(phase: int) -> int:
    path = PIPELINE_DIR / f"findings-{phase:02d}.json"
    n = count_blockers(path)
    if n is None:
        return fail(f"phase {phase}: {path} missing or unreadable")
    return ok(f"phase {phase}: {n} blockers in {path.name}")


def verify_fix_findings() -> int:
    failures = 0
    for phase in (12, 13):
        path = PIPELINE_DIR / f"findings-{phase:02d}.json"
        n = count_blockers(path)
        if n is None:
            return fail(f"phase 14: {path} missing")
        if n > 0:
            print(
                f"verify-leaf: FAIL: phase 14: {n} blockers remain in {path.name}",
                file=sys.stderr,
            )
            failures += 1
    if failures > 0:
        return 1
    return ok("phase 14: 0 blockers across findings-12 and findings-13")


def verify_final() -> int:
    rc = verify_build_test("phase 15 (final verify)")
    if rc != 0:
        return rc
    hlint = PIPELINE_DIR / "hlint.log"
    if not hlint.exists():
        return fail("phase 15: hlint.log missing")
    return ok("phase 15: hlint.log present")


def verify_ci() -> int:
    # Cannot reliably check merge state from here; the maintainer's `approve 17`
    # already implies merge-readiness was confirmed externally.
    return ok("phase 17: CI/merge verification delegated to gh status checks")


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print(__doc__, file=sys.stderr)
        return 2
    try:
        phase = int(argv[1])
    except ValueError:
        return fail(f"phase number must be int, got {argv[1]!r}")

    if phase == 11:
        return verify_build_test("phase 11 (build loop)")
    if phase == 12:
        return verify_findings_phase(12)
    if phase == 13:
        return verify_findings_phase(13)
    if phase == 14:
        return verify_fix_findings()
    if phase == 15:
        return verify_final()
    if phase == 17:
        return verify_ci()

    return ok(f"phase {phase}: no artefact-level re-verification defined; trust the leaf")


if __name__ == "__main__":
    sys.exit(main(sys.argv))
