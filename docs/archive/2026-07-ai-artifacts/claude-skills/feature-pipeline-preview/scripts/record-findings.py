# ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.
# Superseded by codemap/ + root AGENTS.md. Deleted once pipeline verified (Phase 6).
# Manifest: docs/archive/2026-07-ai-artifacts/MANIFEST.md
#!/usr/bin/env python3
"""
Record grounded review findings for a pipeline phase.

Reads a JSON array of grounded findings on stdin (or from `--input <path>`),
computes aggregate counts, writes `.pipeline/findings-{NN}.json`, then
registers the findings file with pipeline.py and marks the phase complete.

Used by the record-findings leaves in phases 4, 5, 12, and 13. Centralising
the logic here keeps the leaves themselves trivial (one shell invocation +
refusal on non-zero exit) and ensures every phase produces the same aggregate
shape downstream consumers expect.

Output JSON shape (always):
  {
    "total_findings": N,
    "blockers":       N,   # count where finding["blocker"] == true
    "kept":           N,   # grounding_outcome == "keep"
    "demoted":        N,   # grounding_outcome == "demote"
    "framework_debt": N,   # grounding_outcome == "framework-debt"
    "findings":       [ ...input findings with `blocker` flag added... ]
  }

Severity schemes (which severities map to `blocker = true`):
  - security:    {"critical", "high"}   used by phases 4 and 12
  - performance: {"blocking"}           used by phases 5 and 13

Exit codes:
  0  success
  1  user error (bad JSON, missing input, unknown phase / scheme)
  2  state error from pipeline.py findings/complete
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path

VALID_PHASES = {4, 5, 12, 13}

# Severity strings that count as blockers, lower-cased for case-insensitive
# comparison. The agents producing the findings are LLMs and may emit any
# of {Critical, critical, CRITICAL}, so normalisation is load-bearing.
BLOCKER_SEVERITIES = {
    "security": {"critical", "high"},
    "performance": {"blocking"},
}

PIPELINE_PY = Path(__file__).parent / "pipeline.py"


def die(msg: str, code: int = 1) -> None:
    print(f"ERROR: {msg}", file=sys.stderr)
    sys.exit(code)


def load_findings(path: str | None) -> list[dict]:
    """Read JSON from --input or stdin; refuse anything that isn't an array."""
    raw = Path(path).read_text() if path else sys.stdin.read()
    if not raw.strip():
        die("input is empty — pass a JSON array on stdin or via --input")
    try:
        data = json.loads(raw)
    except json.JSONDecodeError as exc:
        die(f"input is not valid JSON: {exc}")
    if not isinstance(data, list):
        die("input is not a JSON array")
    for i, entry in enumerate(data):
        if not isinstance(entry, dict):
            die(f"input[{i}] is not an object")
    return data


VALID_OUTCOMES = {"keep", "demote", "drop", "framework-debt"}


def compute_record(
    findings: list[dict], blocker_set: set[str]
) -> dict:
    """Stamp every finding with `blocker` and emit the aggregate envelope.

    Fail-closed: a missing `severity_after_grounding` or an unknown
    `grounding_outcome` is rejected before the blocker count is computed,
    so phase advance cannot suppress a real blocker via a malformed record.
    """
    for i, f in enumerate(findings):
        if "severity_after_grounding" not in f:
            die(f"input[{i}] missing 'severity_after_grounding'")
        outcome = f.get("grounding_outcome")
        if outcome not in VALID_OUTCOMES:
            die(
                f"input[{i}] has invalid 'grounding_outcome' "
                f"(expected one of {sorted(VALID_OUTCOMES)}, got {outcome!r})"
            )
        sev = str(f["severity_after_grounding"]).strip().lower()
        f["blocker"] = sev in blocker_set
    blockers = sum(1 for f in findings if f["blocker"])
    kept = sum(1 for f in findings if f.get("grounding_outcome") == "keep")
    demoted = sum(1 for f in findings if f.get("grounding_outcome") == "demote")
    framework = sum(
        1 for f in findings if f.get("grounding_outcome") == "framework-debt"
    )
    return {
        "total_findings": len(findings),
        "blockers": blockers,
        "kept": kept,
        "demoted": demoted,
        "framework_debt": framework,
        "findings": findings,
    }


def write_record(out_path: Path, record: dict) -> None:
    """Atomic-ish write via the parent dir; refuse if the dir doesn't exist."""
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with open(out_path, "w") as f:
        json.dump(record, f, indent=2, sort_keys=True)
        f.write("\n")


def run_pipeline(*args: str) -> None:
    """Shell out to pipeline.py; surface stderr verbatim on non-zero exit."""
    cmd = [sys.executable, str(PIPELINE_PY), *args]
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.stdout:
        sys.stdout.write(result.stdout)
    if result.returncode != 0:
        if result.stderr:
            sys.stderr.write(result.stderr)
        die(f"pipeline.py {' '.join(args)} exited {result.returncode}", 2)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--phase", type=int, required=True,
                        help=f"pipeline phase number (one of {sorted(VALID_PHASES)})")
    parser.add_argument("--severity-scheme", required=True,
                        choices=sorted(BLOCKER_SEVERITIES),
                        help="which severity labels count as blockers")
    parser.add_argument("--input", help="path to findings JSON (default: stdin)")
    args = parser.parse_args()

    if args.phase not in VALID_PHASES:
        die(f"phase {args.phase} is not a record-findings phase (valid: {sorted(VALID_PHASES)})")

    out_path = Path(".pipeline") / f"findings-{args.phase:02d}.json"
    blocker_set = BLOCKER_SEVERITIES[args.severity_scheme]

    findings = load_findings(args.input)
    record = compute_record(findings, blocker_set)
    write_record(out_path, record)

    run_pipeline("findings", str(args.phase), str(out_path))
    run_pipeline("complete", str(args.phase))

    print(f"wrote {out_path} ({record['total_findings']} findings, "
          f"{record['blockers']} blocker(s))")


if __name__ == "__main__":
    main()
