#!/usr/bin/env python3
"""Pipeline telemetry emitter (Phase 1). Schema: telemetry/SCHEMA.md (v2, frozen).

One JSON line per pipeline run, appended to telemetry/runs.jsonl. Never
hand-written — pipeline scripts call this tool.

Usage:
  telemetry.py start   --run-id 2026-07-07-001 --request "issue#712"
  telemetry.py stage   --name implement --event start [--model sonnet]
  telemetry.py stage   --name implement --event stop  [--repair-rounds 2] [--invented-api-events 3]
  telemetry.py wait    --seconds 340            # add waiting-on-human time
  telemetry.py rebuilt --count 4                # cache-health metric
  telemetry.py finish  --outcome ok
  telemetry.py finish  --outcome failed --failure-label invented-api [--failure-note "..."]
  telemetry.py golden  --request-file R --spec-file S --diff-file D --verdict "..." [--transcript-file T]
"""

import argparse
import datetime
import json
import re
import shutil
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CURRENT = ROOT / "telemetry" / ".current-run.json"
RUNS = ROOT / "telemetry" / "runs.jsonl"
GOLDEN = ROOT / "telemetry" / "golden"

STAGES = [
    "intake", "localize", "spec", "design-review", "plan",
    "test-writing", "implement", "verify", "pr", "ci",
]
OUTCOMES = ["ok", "parked", "failed", "abandoned"]
FAILURE_LABELS = [
    "wrong-intent", "wrong-localization", "dialect-violation", "invented-api",
    "test-failure", "spec-drift", "flaky-infra", "timeout",
    "human-rejected-spec", "human-rejected-pr", "other",
]


# De-facto run-id format (documented in telemetry/SCHEMA.md): also guards the
# telemetry/golden/<run_id>/ filesystem path against traversal/absolute values.
RUN_ID_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]*$")


def validate_run_id(run_id: str) -> str:
    if not RUN_ID_RE.fullmatch(run_id):
        sys.exit(f"telemetry: invalid run-id {run_id!r} (must match {RUN_ID_RE.pattern})")
    return run_id


def non_negative(value: int, flag: str) -> int:
    if value < 0:
        sys.exit(f"telemetry: {flag} must be >= 0")
    return value


def now() -> str:
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def load() -> dict:
    if not CURRENT.exists():
        sys.exit("telemetry: no run in progress (run `telemetry.py start` first)")
    return json.loads(CURRENT.read_text())


def save(run: dict) -> None:
    CURRENT.parent.mkdir(parents=True, exist_ok=True)
    CURRENT.write_text(json.dumps(run, indent=2))


def cmd_start(args) -> None:
    validate_run_id(args.run_id)
    if CURRENT.exists():
        sys.exit(f"telemetry: run already in progress ({CURRENT}); finish it first")
    save({
        "schema": 2,
        "run_id": args.run_id,
        "request_ref": args.request,
        "stages": {},
        "waiting_on_human_s": 0,
        "modules_rebuilt_after_restore": None,
        "outcome": None,
        "failure_label": None,
    })
    print(f"telemetry: started run {args.run_id}")


def cmd_stage(args) -> None:
    if args.name not in STAGES:
        sys.exit(f"telemetry: unknown stage '{args.name}' (schema v2 stages: {', '.join(STAGES)})")
    run = load()
    stage = run["stages"].setdefault(args.name, {"start": None, "stop": None, "model": None,
                                                 "repair_rounds": 0, "invented_api_events": 0})
    if args.event == "start":
        stage["start"] = now()
        if args.model:
            stage["model"] = args.model
    else:
        stage["stop"] = now()
        if args.repair_rounds is not None:
            stage["repair_rounds"] = non_negative(args.repair_rounds, "--repair-rounds")
        if args.invented_api_events is not None:
            stage["invented_api_events"] = non_negative(args.invented_api_events,
                                                        "--invented-api-events")
    save(run)


def cmd_wait(args) -> None:
    non_negative(args.seconds, "--seconds")
    run = load()
    run["waiting_on_human_s"] += args.seconds
    save(run)


def cmd_rebuilt(args) -> None:
    non_negative(args.count, "--count")
    run = load()
    run["modules_rebuilt_after_restore"] = args.count
    save(run)


def cmd_finish(args) -> None:
    run = load()
    if args.outcome not in OUTCOMES:
        sys.exit(f"telemetry: outcome must be one of {OUTCOMES}")
    if args.failure_label is not None and args.failure_label not in FAILURE_LABELS:
        sys.exit(f"telemetry: failure-label must be from the closed taxonomy: {', '.join(FAILURE_LABELS)}")
    if args.outcome == "ok" and args.failure_label is not None:
        sys.exit("telemetry: 'ok' runs must not carry a failure-label")
    if args.outcome in ("failed", "parked"):
        if args.failure_label is None:
            sys.exit(f"telemetry: failed/parked runs need --failure-label from: {', '.join(FAILURE_LABELS)}")
        if args.failure_label == "other" and not args.failure_note:
            sys.exit("telemetry: failure-label 'other' REQUIRES --failure-note (re-classified at weekly review)")
    run["outcome"] = args.outcome
    run["failure_label"] = args.failure_label
    if args.failure_note:
        run["failure_note"] = args.failure_note
    RUNS.parent.mkdir(parents=True, exist_ok=True)
    with RUNS.open("a") as f:
        f.write(json.dumps(run, separators=(",", ":")) + "\n")
    CURRENT.unlink()
    print(f"telemetry: run {run['run_id']} recorded -> {RUNS.relative_to(ROOT)}")


def cmd_golden(args) -> None:
    run_id = args.run_id
    if run_id is None:
        if not CURRENT.exists():
            sys.exit("telemetry: --run-id required when no run is in progress")
        run_id = json.loads(CURRENT.read_text())["run_id"]
    validate_run_id(run_id)
    dest = GOLDEN / run_id
    dest.mkdir(parents=True, exist_ok=True)
    shutil.copy(args.request_file, dest / "request.md")
    shutil.copy(args.spec_file, dest / "spec.md")
    shutil.copy(args.diff_file, dest / "final.diff")
    (dest / "verdict.md").write_text(args.verdict + "\n")
    if args.transcript_file:
        shutil.copy(args.transcript_file, dest / "transcript.md")
    print(f"telemetry: golden task archived -> {dest.relative_to(ROOT)}")


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__)
    sub = p.add_subparsers(dest="cmd", required=True)

    s = sub.add_parser("start")
    s.add_argument("--run-id", required=True)
    s.add_argument("--request", required=True)
    s.set_defaults(fn=cmd_start)

    s = sub.add_parser("stage")
    s.add_argument("--name", required=True)
    s.add_argument("--event", required=True, choices=["start", "stop"])
    s.add_argument("--model")
    s.add_argument("--repair-rounds", type=int)
    s.add_argument("--invented-api-events", type=int)
    s.set_defaults(fn=cmd_stage)

    s = sub.add_parser("wait")
    s.add_argument("--seconds", type=int, required=True)
    s.set_defaults(fn=cmd_wait)

    s = sub.add_parser("rebuilt")
    s.add_argument("--count", type=int, required=True)
    s.set_defaults(fn=cmd_rebuilt)

    s = sub.add_parser("finish")
    s.add_argument("--outcome", required=True)
    s.add_argument("--failure-label")
    s.add_argument("--failure-note")
    s.set_defaults(fn=cmd_finish)

    s = sub.add_parser("golden")
    s.add_argument("--run-id")
    s.add_argument("--request-file", required=True)
    s.add_argument("--spec-file", required=True)
    s.add_argument("--diff-file", required=True)
    s.add_argument("--verdict", required=True)
    s.add_argument("--transcript-file")
    s.set_defaults(fn=cmd_golden)

    args = p.parse_args()
    args.fn(args)


if __name__ == "__main__":
    main()
