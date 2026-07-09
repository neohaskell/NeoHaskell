#!/usr/bin/env python3
"""Pipeline telemetry emitter (Phase 1; schema v4 Phase 6). Schema: telemetry/SCHEMA.md.

One JSON line per pipeline run, appended to telemetry/runs.jsonl. Never
hand-written — pipeline scripts call this tool.

Usage (via the `./dev telemetry` verb — this file has no PATH entry):
  ./dev telemetry start   --run-id 2026-07-07-001 --request "issue#712"
  ./dev telemetry stage   --name implement --event start [--model sonnet]
  ./dev telemetry stage   --name implement --event stop  [--repair-rounds 2] [--invented-api-events 3]
  ./dev telemetry wait    --seconds 340            # add waiting-on-human time
  ./dev telemetry rebuilt --count 4                # cache-health metric
  ./dev telemetry consult --asset alias:auth       # usage accounting (Phase 6, task 5e)
  ./dev telemetry finish  --outcome ok
  ./dev telemetry finish  --outcome ok --improvement cli-utility:scripts/changelog  # class-fix shipped on success (v4)
  ./dev telemetry finish  --outcome parked --failure-label timeout --asset-delta alias:codemap/capabilities.yaml
  ./dev telemetry golden  --request-file R --spec-file S --diff-file D --verdict "..." [--transcript-file T]
  ./dev telemetry --self-test                      # doctor/CI

A failed/parked run MUST carry --asset-delta (the class-fix ships with the
instance-fix, Phase 6 task 4). Schema bumps (new fields) are documented in
telemetry/SCHEMA.md — this emitter writes the current version; readers tolerate
older ones.
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
# Delta-type taxonomy v1 (closed; Phase 6). Single source — SCHEMA.md and the
# retrospective miner reference it; `none` = a justified no-asset (destination
# carries the reason). Each maps to a real destination file (see the miner skill).
DELTA_TYPES = [
    "alias", "extension-point", "phrasebook", "hot-card", "hlint-rule",
    "hook", "cli-utility", "skill-edit", "telemetry-label", "PRUNE", "none",
]
CURRENT_SCHEMA = 4


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


def run_id_recorded(run_id: str) -> bool:
    """True if run_id already appears in the committed ledger. Two parallel
    worktree pipelines can each pick `<date>-001` (the per-worktree
    `.current-run.json` guard can't see across worktrees); a collision would
    corrupt the first-5-runs baseline and clobber golden/<run_id>/. Guard here,
    and let `runs.jsonl merge=union` (.gitattributes) handle the append merge."""
    if not RUNS.exists():
        return False
    for line in RUNS.read_text().splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            if json.loads(line).get("run_id") == run_id:
                return True
        except json.JSONDecodeError:
            continue
    return False


def cmd_start(args) -> None:
    validate_run_id(args.run_id)
    if CURRENT.exists():
        sys.exit(f"telemetry: run already in progress ({CURRENT}); finish it first")
    if run_id_recorded(args.run_id):
        sys.exit(f"telemetry: run-id {args.run_id!r} is already recorded in runs.jsonl — pick a "
                 "unique suffix (e.g. `-b`) so parallel worktree runs don't collide "
                 "(baseline + golden/<run_id>/ key on run_id)")
    save({
        "schema": CURRENT_SCHEMA,
        "run_id": args.run_id,
        "request_ref": args.request,
        "stages": {},
        "waiting_on_human_s": 0,
        "modules_rebuilt_after_restore": None,
        "assets_consulted": [],
        "outcome": None,
        "failure_label": None,
        "asset_delta": None,
        "improvements": [],
    })
    print(f"telemetry: started run {args.run_id}")


def cmd_stage(args) -> None:
    if args.name not in STAGES:
        sys.exit(f"telemetry: unknown stage '{args.name}' (stage vocabulary: {', '.join(STAGES)})")
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


CONSULT_RE = re.compile(r"^[a-z][a-z0-9-]*:.+$")


def cmd_consult(args) -> None:
    """Usage accounting (Phase 6 task 5e): record that this run consulted an
    asset, so the miner can recommend PRUNE for never-consulted ones."""
    if not CONSULT_RE.fullmatch(args.asset):
        sys.exit(f"telemetry: --asset must be '<kind>:<name>' (kebab kind), got {args.asset!r}")
    run = load()
    consulted = run.setdefault("assets_consulted", [])
    if args.asset not in consulted:
        consulted.append(args.asset)
    save(run)


def parse_asset_delta(raw: str) -> dict:
    """`<type>:<destination>[:<ref>]` — the class-fix that ships with a failed
    run. type from the closed taxonomy; destination = the asset file. For
    `none`, everything after the first colon is the justification and is kept
    whole (a reason may itself contain colons — it is never split into `ref`)."""
    dtype, sep, rest = raw.partition(":")
    if dtype not in DELTA_TYPES:
        sys.exit(f"telemetry: asset-delta type must be from the closed taxonomy: {', '.join(DELTA_TYPES)}")
    if not sep or not rest:
        sys.exit("telemetry: --asset-delta needs '<type>:<destination>' "
                 "(destination = the asset file, or the justification for 'none')")
    if dtype == "none":
        return {"type": dtype, "destination": rest, "ref": None}
    dest, _, ref = rest.partition(":")
    if not dest:
        sys.exit("telemetry: --asset-delta needs a non-empty destination")
    return {"type": dtype, "destination": dest, "ref": ref or None}


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
    delta = parse_asset_delta(args.asset_delta) if args.asset_delta is not None else None
    if args.outcome in ("failed", "parked") and delta is None:
        sys.exit(f"telemetry: {args.outcome} runs need --asset-delta <type>:<dest> — the "
                 "class-fix ships with the instance-fix (Phase 6). Types: "
                 f"{', '.join(DELTA_TYPES)}; use 'none:<reason>' if genuinely no asset applies")
    # `improvements` (v4): class-fixes a run ships alongside SUCCESS — the loop
    # learns from ok runs, not only failures. Optional, any outcome, repeatable.
    # `none` is meaningless here (an improvement IS an asset change), so reject it.
    improvements = []
    for raw in getattr(args, "improvement", None) or []:
        imp = parse_asset_delta(raw)
        if imp["type"] == "none":
            sys.exit("telemetry: --improvement names a real asset change, never 'none' "
                     "('none' is only for a failed run's --asset-delta justification)")
        improvements.append(imp)
    run["outcome"] = args.outcome
    run["failure_label"] = args.failure_label
    run["asset_delta"] = delta
    run["improvements"] = improvements
    if args.failure_note:
        run["failure_note"] = args.failure_note
    RUNS.parent.mkdir(parents=True, exist_ok=True)
    with RUNS.open("a") as f:
        f.write(json.dumps(run, separators=(",", ":")) + "\n")
    CURRENT.unlink()
    try:
        where = RUNS.relative_to(ROOT)
    except ValueError:
        where = RUNS  # self-test redirects RUNS to a temp dir outside the repo
    print(f"telemetry: run {run['run_id']} recorded -> {where}")


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


def self_test() -> int:
    """Exercises schema-v3 fields, usage accounting, and the asset-delta
    enforcement in a temp dir (doctor/CI). Same contract as the other guards."""
    global CURRENT, RUNS, GOLDEN
    import tempfile
    fails = 0

    def ns(**kw):
        return argparse.Namespace(**kw)

    def check(label, fn, want_exit):
        nonlocal fails
        exited = False
        try:
            fn()
        except SystemExit as e:
            exited = e.code not in (None, 0)
        if exited != want_exit:
            print(f"telemetry self-test FAIL {label}: exited={exited}, want_exit={want_exit}")
            fails += 1

    with tempfile.TemporaryDirectory() as tmp:
        t = Path(tmp)
        CURRENT, RUNS, GOLDEN = t / ".current-run.json", t / "runs.jsonl", t / "golden"

        check("start", lambda: cmd_start(ns(run_id="t-001", request="issue#0")), False)
        cur = json.loads(CURRENT.read_text())
        if cur.get("schema") != CURRENT_SCHEMA or cur.get("asset_delta") is not None \
                or cur.get("assets_consulted") != [] or cur.get("improvements") != []:
            print(f"telemetry self-test FAIL start-shape: {cur}")
            fails += 1
        check("consult-ok", lambda: cmd_consult(ns(asset="alias:auth")), False)
        check("consult-bad", lambda: cmd_consult(ns(asset="nocolon")), True)
        # non-ok outcomes require an asset delta from the closed taxonomy
        check("finish-parked-no-delta", lambda: cmd_finish(
            ns(outcome="parked", failure_label="timeout", failure_note=None, asset_delta=None)), True)
        check("finish-bad-delta-type", lambda: cmd_finish(
            ns(outcome="parked", failure_label="timeout", failure_note=None, asset_delta="bogus:x")), True)
        check("finish-parked-ok", lambda: cmd_finish(
            ns(outcome="parked", failure_label="timeout", failure_note=None,
               asset_delta="alias:codemap/capabilities.yaml")), False)
        line = json.loads(RUNS.read_text().splitlines()[-1])
        if (line.get("asset_delta") or {}).get("type") != "alias" \
                or line.get("assets_consulted") != ["alias:auth"]:
            print(f"telemetry self-test FAIL finish-line: {line}")
            fails += 1
        # C: a run-id already recorded in the ledger is refused (parallel-worktree guard)
        check("start-dup-runid", lambda: cmd_start(ns(run_id="t-001", request="issue#0")), True)
        # H: a `none:<reason>` justification keeps its colons (never split into ref);
        #    a typed delta still splits an optional trailing ref
        nd = parse_asset_delta("none:blocked on upstream: see PR #9")
        if nd["destination"] != "blocked on upstream: see PR #9" or nd["ref"] is not None:
            print(f"telemetry self-test FAIL none-colon: {nd}")
            fails += 1
        td = parse_asset_delta("alias:codemap/capabilities.yaml:PR#12")
        if td["destination"] != "codemap/capabilities.yaml" or td["ref"] != "PR#12":
            print(f"telemetry self-test FAIL delta-ref: {td}")
            fails += 1
        # ok runs do not require an asset delta
        check("start2", lambda: cmd_start(ns(run_id="t-002", request="issue#0")), False)
        check("finish-ok", lambda: cmd_finish(
            ns(outcome="ok", failure_label=None, failure_note=None, asset_delta=None)), False)
        # v4: an ok run may record `improvements` (class-fixes shipped on success)
        check("start3", lambda: cmd_start(ns(run_id="t-003", request="issue#0")), False)
        check("finish-ok-improvement", lambda: cmd_finish(
            ns(outcome="ok", failure_label=None, failure_note=None, asset_delta=None,
               improvement=["cli-utility:scripts/changelog", "skill-edit:.claude/skills/x/SKILL.md"])), False)
        imp_line = json.loads(RUNS.read_text().splitlines()[-1])
        if [i["type"] for i in imp_line.get("improvements", [])] != ["cli-utility", "skill-edit"]:
            print(f"telemetry self-test FAIL improvements: {imp_line}")
            fails += 1
        # `none` is a failed-run asset-delta justification, never a valid improvement
        check("start4", lambda: cmd_start(ns(run_id="t-004", request="issue#0")), False)
        check("finish-improvement-none-rejected", lambda: cmd_finish(
            ns(outcome="ok", failure_label=None, failure_note=None, asset_delta=None,
               improvement=["none:whatever"])), True)

    # anti-rot: the failure-label taxonomy is duplicated in scripts/pipeline-state
    # (LABELS) — a drift there means a run the emitter accepts, the state machine
    # rejects (or vice-versa). Assert parity so the two can't silently diverge.
    ps = (ROOT / "scripts" / "pipeline-state").read_text()
    m = re.search(r"LABELS\s*=\s*\{(.*?)\}", ps, re.DOTALL)
    ps_labels = set(re.findall(r'"([^"]+)"', m.group(1))) if m else set()
    if ps_labels != set(FAILURE_LABELS):
        print(f"telemetry self-test FAIL label-parity: telemetry={sorted(FAILURE_LABELS)} "
              f"vs pipeline-state={sorted(ps_labels)}")
        fails += 1

    # anti-rot: schema-version parity. CURRENT_SCHEMA is the single source; a bump
    # that misses a doc silently lies to the most-read agent surface (AGENTS.md is
    # loaded every session). Every `schema v<N>`/`frozen v<N>` claim must equal it.
    # (Version *history* in SCHEMA.md is written bare — `v1 → v2` — so it is not a
    # claim and not matched here.)
    SKILLS = ROOT / ".claude" / "skills"
    version_docs = [
        ROOT / "telemetry" / "SCHEMA.md",
        ROOT / "AGENTS.md",
        SKILLS / "neohaskell-pipeline" / "SKILL.md",
        SKILLS / "neohaskell-retrospective-miner" / "SKILL.md",
    ]
    ver_re = re.compile(r"(?:schema|frozen)\s+v(\d+)", re.IGNORECASE)
    for doc in version_docs:
        if not doc.exists():
            continue
        for n in sorted(set(ver_re.findall(doc.read_text()))):
            if int(n) != CURRENT_SCHEMA:
                print(f"telemetry self-test FAIL schema-version-parity: "
                      f"{doc.relative_to(ROOT)} claims v{n}, CURRENT_SCHEMA={CURRENT_SCHEMA}")
                fails += 1

    # anti-rot: delta-type taxonomy prose parity. DELTA_TYPES is the single source;
    # the miner skill restates it as a `|`-list and SCHEMA.md as a `·`-list — both
    # must agree or the taxonomy the docs teach drifts from what `finish` enforces.
    miner = SKILLS / "neohaskell-retrospective-miner" / "SKILL.md"
    if miner.exists():
        mm = re.search(r"`delta_type`\s*∈\s*`([^`]+)`", miner.read_text())
        skill_types = {t.strip() for t in mm.group(1).split("|") if t.strip()} if mm else set()
        if skill_types != set(DELTA_TYPES):
            print(f"telemetry self-test FAIL delta-type-parity(miner skill): "
                  f"telemetry={sorted(DELTA_TYPES)} vs skill={sorted(skill_types)}")
            fails += 1
    schema_md = ROOT / "telemetry" / "SCHEMA.md"
    if schema_md.exists():
        sec = re.search(r"## Delta-type taxonomy.*?(?=\n## |\Z)", schema_md.read_text(), re.DOTALL)
        block = sec.group(0) if sec else ""
        missing = [t for t in DELTA_TYPES if f"`{t}`" not in block]
        if missing:
            print(f"telemetry self-test FAIL delta-type-parity(SCHEMA.md): "
                  f"taxonomy block missing {missing}")
            fails += 1

    if fails == 0:
        print("telemetry self-test: OK — schema-v4 fields, usage accounting, asset-delta "
              "enforcement, run-id collision guard, none-colon justification, and "
              "label/schema-version/delta-type parity covered")
    return 1 if fails else 0


def main() -> None:
    if len(sys.argv) > 1 and sys.argv[1] == "--self-test":
        sys.exit(self_test())
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

    s = sub.add_parser("consult")
    s.add_argument("--asset", required=True)
    s.set_defaults(fn=cmd_consult)

    s = sub.add_parser("finish")
    s.add_argument("--outcome", required=True)
    s.add_argument("--failure-label")
    s.add_argument("--failure-note")
    s.add_argument("--asset-delta")
    s.add_argument("--improvement", action="append",
                   help="class-fix shipped by this (successful) run: <type>:<dest>; repeatable")
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
