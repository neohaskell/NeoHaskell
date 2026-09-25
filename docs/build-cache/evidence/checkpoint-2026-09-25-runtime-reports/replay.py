#!/usr/bin/env python3
"""Run from the NeoHaskell checkout at176a5bf with actual archived report paths."""
import hashlib
import json
from pathlib import Path
import runpy
import sys

validator = runpy.run_path("scripts/spec-check")["runtime_report_errors"]
entries = [entry for entry in json.loads(Path("docs/changes/test-surfaces.json").read_text())["boundaries"]
           if entry.get("suite") == "nhcore-test-service"]
assert entries
assert len(sys.argv) > 1, "provide actual hosted .nhcore-test-service.report paths"
for name in sys.argv[1:]:
    path = Path(name)
    report = path.read_text()
    for entry in entries:
        locator = dict(entry, raw=f"hspec:{entry['suite']}:{entry['path']}#{entry['match']}")
        assert not validator(locator, entry, report), locator
        assert validator(locator, entry, report.replace(entry['match'], 'REMOVED_SELECTOR')), locator
        assert validator(locator, entry, report.replace(entry['fixture'], 'REMOVED_FIXTURE')), locator
    print(path.name, hashlib.sha256(path.read_bytes()).hexdigest(), len(entries), "locators and removal negatives passed")
