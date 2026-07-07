# ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.
# Superseded by codemap/ + root AGENTS.md. Deleted once pipeline verified (Phase 6).
# Manifest: docs/archive/2026-07-ai-artifacts/MANIFEST.md
#!/usr/bin/env python3
"""
NeoHaskell feature pipeline — deterministic state machine.

Reads/writes .pipeline/state.json under the project root. Each phase has a
status (pending | in_progress | completed), an optional approved_at
timestamp, and tracks its findings file (for the security/perf phases).
Phase numbers, names, dependencies, and PAUSE gates are defined in PHASES.

Commands:
  init <feature> [options]    Initialise the pipeline state.
  status                      Human-readable status table.
  next                        Emit the next actionable phase(s) as JSON.
  complete <phase>            Mark a phase as completed.
  approve <phase>             Approve a PAUSE-gated phase.
  classify <tier> <rationale> Persist the feature complexity tier.
  findings <phase> <path>     Register the findings file for a review phase.
  iter <phase>                Increment & print this phase's iteration counter.
  set <key> <value>           Set a variable (feature_name, branch_name, ...).
  get <key>                   Print a variable's value (empty if unset).
  reset                       Delete the pipeline state directory.

Exit codes: 0 success, 1 user-error (missing/invalid input), 2 state-error
(dependency unmet, wrong phase order, etc).
"""

from __future__ import annotations

import argparse
import fcntl
import json
import os
import re
import sys
import tempfile
from datetime import datetime, timezone
from typing import Any

PIPELINE_DIR = ".pipeline"
STATE_FILE = os.path.join(PIPELINE_DIR, "state.json")
LOCK_FILE = os.path.join(PIPELINE_DIR, ".lock")
CLASS_FILE = os.path.join(PIPELINE_DIR, "classification.json")

# Phase definitions. `depends` is the list of prerequisite phases. A PAUSE
# phase must be both completed AND approved before its dependents unlock.
PHASES: dict[int, dict[str, Any]] = {
    1:  {"name": "Init pipeline",          "depends": [],      "pause": False},
    2:  {"name": "Classify feature",       "depends": [1],     "pause": False},
    3:  {"name": "ADR draft",              "depends": [2],     "pause": True},
    4:  {"name": "Security review (ADR)",  "depends": [3],     "pause": False},
    5:  {"name": "Performance review (ADR)","depends": [3],    "pause": False},
    6:  {"name": "DevEx review",           "depends": [4, 5],  "pause": False},
    7:  {"name": "Architecture design",    "depends": [6],     "pause": False},
    8:  {"name": "Test spec design",       "depends": [7],     "pause": False},
    9:  {"name": "Test writing",           "depends": [8],     "pause": False},
    10: {"name": "Implementation",         "depends": [9],     "pause": False},
    11: {"name": "Build loop",             "depends": [10],    "pause": False},
    12: {"name": "Security review (impl)", "depends": [11],    "pause": False},
    13: {"name": "Performance review (impl)","depends": [11],  "pause": False},
    14: {"name": "Fix findings",           "depends": [12, 13],"pause": False},
    15: {"name": "Final verify",           "depends": [14],    "pause": False},
    16: {"name": "Finalize PR",            "depends": [15],    "pause": True},
    17: {"name": "Opus PR review",         "depends": [16],    "pause": True},
    18: {"name": "CI cycle",               "depends": [17],    "pause": True},
}

VALID_TIERS = {"trivial", "simple", "moderate", "complex", "security-critical"}


# ---------- helpers ----------

def slugify(name: str) -> str:
    """Lower-case + hyphenate, used to derive the ADR filename."""
    s = re.sub(r"[^a-z0-9\s-]", "", name.lower().strip())
    s = re.sub(r"\s+", "-", s)
    return re.sub(r"-+", "-", s).strip("-")


def now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def die(msg: str, code: int = 1) -> None:
    print(f"ERROR: {msg}", file=sys.stderr)
    sys.exit(code)


# ---------- state IO ----------

def _acquire_lock():
    """Open and exclusively-lock the lockfile; caller releases on exit."""
    os.makedirs(PIPELINE_DIR, exist_ok=True)
    fd = open(LOCK_FILE, "w")
    fcntl.flock(fd, fcntl.LOCK_EX)
    return fd


def load_state() -> dict[str, Any]:
    if not os.path.exists(STATE_FILE):
        die("no pipeline initialised — run: pipeline.py init <feature>", 2)
    lock = _acquire_lock()
    try:
        with open(STATE_FILE) as f:
            state = json.load(f)
    except Exception:
        fcntl.flock(lock, fcntl.LOCK_UN)
        lock.close()
        raise
    state["_lock"] = lock
    return state


def save_state(state: dict[str, Any]) -> None:
    """Atomic write via tempfile + rename; releases the lock."""
    lock = state.pop("_lock", None)
    state["updated_at"] = now_iso()
    fd, tmp = tempfile.mkstemp(dir=PIPELINE_DIR, suffix=".json")
    try:
        with os.fdopen(fd, "w") as f:
            json.dump(state, f, indent=2, sort_keys=True)
            f.write("\n")
        os.replace(tmp, STATE_FILE)
    except Exception:
        os.unlink(tmp)
        raise
    finally:
        if lock is not None:
            fcntl.flock(lock, fcntl.LOCK_UN)
            lock.close()


def release_lock(state: dict[str, Any]) -> None:
    """Drop the lock without writing (read-only commands)."""
    lock = state.pop("_lock", None)
    if lock is not None:
        fcntl.flock(lock, fcntl.LOCK_UN)
        lock.close()


# ---------- phase logic ----------

def phase_is_ready(num: int, state: dict[str, Any]) -> bool:
    """A phase is ready iff every dep is completed (and approved if PAUSE)."""
    for dep in PHASES[num]["depends"]:
        s = state["phases"][str(dep)]
        if s["status"] != "completed":
            return False
        if PHASES[dep]["pause"] and not s.get("approved_at"):
            return False
    return True


def validate_phase_num(num: int) -> None:
    if num not in PHASES:
        die(f"phase {num} does not exist (valid: 1..{max(PHASES)})", 1)


# ---------- commands ----------

def cmd_init(args: argparse.Namespace) -> None:
    if os.path.exists(STATE_FILE):
        die("pipeline already initialised — run 'pipeline.py reset' first.", 2)
    slug = slugify(args.feature)
    state = {
        "version": 1,
        "created_at": now_iso(),
        "updated_at": now_iso(),
        "variables": {
            "feature_name": args.feature,
            "slug": slug,
            "issue_number": args.issue or "",
            "branch_name": args.branch or f"feature/{slug}",
            "module_path": args.module or "",
            "test_path": args.test or "",
            "adr_number": args.adr or "",
            "adr_path": (
                f"docs/decisions/{args.adr}-{slug}.md" if args.adr else ""
            ),
            "pr_number": "",
            "pr_url": "",
        },
        "phases": {
            str(n): {
                "status": "pending",
                "completed_at": None,
                "approved_at": None,
                "findings_path": None,
            }
            for n in PHASES
        },
    }
    os.makedirs(PIPELINE_DIR, exist_ok=True)
    # Pipeline state is per-checkout, never committed.
    gi = os.path.join(PIPELINE_DIR, ".gitignore")
    if not os.path.exists(gi):
        with open(gi, "w") as f:
            f.write("*\n!.gitignore\n")
    save_state({**state, "_lock": _acquire_lock()})
    print(f"Pipeline initialised for: {args.feature}")
    print(f"  Branch:  {state['variables']['branch_name']}")
    print(f"  Module:  {state['variables']['module_path'] or '(unset)'}")
    print(f"  Test:    {state['variables']['test_path'] or '(unset)'}")
    print(f"  ADR:     {state['variables']['adr_number'] or '(unset)'}")


def cmd_status(args: argparse.Namespace) -> None:
    state = load_state()
    v = state["variables"]
    print(f"\nfeature-pipeline-preview: {v['feature_name']}")
    bits = []
    if v.get("issue_number"): bits.append(f"issue #{v['issue_number']}")
    if v.get("branch_name"): bits.append(f"branch {v['branch_name']}")
    if v.get("adr_number"): bits.append(f"ADR {v['adr_number']}")
    if bits:
        print("  " + " | ".join(bits))
    print()
    print(f"{'#':>3}  {'phase':<30}  {'status':<18}")
    print(f"{'-'*3}  {'-'*30}  {'-'*18}")
    for n in sorted(PHASES):
        p = PHASES[n]
        s = state["phases"].get(str(n), {"status": "pending", "approved_at": None, "completed_at": None, "findings_path": None})
        if s["status"] == "completed" and p["pause"]:
            disp = "approved" if s.get("approved_at") else "needs approval"
        elif s["status"] == "completed":
            disp = "done"
        elif phase_is_ready(n, state):
            disp = "ready"
        else:
            disp = "pending"
        print(f"{n:>3}  {p['name']:<30}  {disp:<18}")
    done = sum(1 for s in state["phases"].values() if s["status"] == "completed")
    pending_approval = sum(
        1
        for n, s in state["phases"].items()
        if s["status"] == "completed"
        and PHASES[int(n)]["pause"]
        and not s.get("approved_at")
    )
    suffix = f" ({pending_approval} awaiting approval)" if pending_approval else ""
    print(f"\n  progress: {done}/{len(PHASES)} phases complete{suffix}")
    release_lock(state)


def cmd_next(args: argparse.Namespace) -> None:
    state = load_state()
    waiting = [
        int(n) for n, s in state["phases"].items()
        if s["status"] == "completed"
        and PHASES[int(n)]["pause"]
        and not s.get("approved_at")
    ]
    if waiting:
        print(json.dumps({
            "status": "waiting_for_approval",
            "phases_needing_approval": sorted(waiting),
            "message": (
                "Phase(s) " + ", ".join(map(str, sorted(waiting)))
                + " completed but need approval. Run: pipeline.py approve <phase>"
            ),
        }, indent=2))
        release_lock(state)
        return

    ready = sorted(
        n for n in PHASES
        if state["phases"][str(n)]["status"] == "pending"
        and phase_is_ready(n, state)
    )
    if not ready:
        all_done = all(
            s["status"] == "completed"
            and (not PHASES[int(n)]["pause"] or s.get("approved_at"))
            for n, s in state["phases"].items()
        )
        if all_done:
            print(json.dumps({"status": "complete", "message": "All 17 phases done."}))
        else:
            print(json.dumps({"status": "blocked",
                              "message": "No ready phases. Check status for details."}))
        release_lock(state)
        return

    phases_out = []
    for n in ready:
        phases_out.append({
            "phase": n,
            "name": PHASES[n]["name"],
            "pause_after": PHASES[n]["pause"],
            "skill_dir": _phase_dir(n),
        })
    print(json.dumps({"status": "ready", "phases": phases_out}, indent=2))
    release_lock(state)


def _phase_dir(num: int) -> str:
    """Map a phase number to the on-disk skill directory name."""
    # The directory names follow the canonical numbering used in SKILL.md.
    names = {
        1:  "01-init-pipeline",
        2:  "02-classify-feature",
        3:  "03-adr-draft",
        4:  "04-security-adr",
        5:  "05-performance-adr",
        6:  "06-devex-review",
        7:  "07-architecture-design",
        8:  "08-test-spec-design",
        9:  "09-test-writing",
        10: "10-implementation",
        11: "11-build-loop",
        12: "12-security-impl",
        13: "13-performance-impl",
        14: "14-fix-findings",
        15: "15-final-verify",
        16: "16-create-pr",
        17: "17-ci-cycle",
    }
    return names[num]


def cmd_complete(args: argparse.Namespace) -> None:
    validate_phase_num(args.phase)
    state = load_state()
    s = state["phases"][str(args.phase)]
    if s["status"] == "completed":
        release_lock(state)
        print(f"Phase {args.phase} already completed.")
        return
    if not phase_is_ready(args.phase, state):
        missing = []
        for dep in PHASES[args.phase]["depends"]:
            d = state["phases"][str(dep)]
            if d["status"] != "completed":
                missing.append(f"  - phase {dep} ({PHASES[dep]['name']}): not completed")
            elif PHASES[dep]["pause"] and not d.get("approved_at"):
                missing.append(f"  - phase {dep} ({PHASES[dep]['name']}): not approved")
        release_lock(state)
        die(f"phase {args.phase} dependencies not met:\n" + "\n".join(missing), 2)
    s["status"] = "completed"
    s["completed_at"] = now_iso()
    save_state(state)
    print(f"Phase {args.phase} ({PHASES[args.phase]['name']}) completed.")
    if PHASES[args.phase]["pause"]:
        print(f"  PAUSE: run 'pipeline.py approve {args.phase}' before continuing.")


def cmd_approve(args: argparse.Namespace) -> None:
    validate_phase_num(args.phase)
    if not PHASES[args.phase]["pause"]:
        die(f"phase {args.phase} is not a PAUSE-gated phase.", 1)
    state = load_state()
    s = state["phases"][str(args.phase)]
    if s["status"] != "completed":
        release_lock(state)
        die(f"phase {args.phase} must be completed before it can be approved.", 2)
    if s.get("approved_at"):
        release_lock(state)
        print(f"Phase {args.phase} already approved.")
        return
    s["approved_at"] = now_iso()
    save_state(state)
    print(f"Phase {args.phase} approved. Run 'pipeline.py next' to continue.")


def cmd_classify(args: argparse.Namespace) -> None:
    if args.tier not in VALID_TIERS:
        die(f"invalid tier '{args.tier}' (valid: {sorted(VALID_TIERS)})", 1)
    # Loading state proves the pipeline is initialised before we write
    # the classification file; we don't otherwise mutate state here.
    state = load_state()
    release_lock(state)
    os.makedirs(PIPELINE_DIR, exist_ok=True)
    payload = {
        "tier": args.tier,
        "rationale": args.rationale,
        "classified_at": now_iso(),
    }
    with open(CLASS_FILE, "w") as f:
        json.dump(payload, f, indent=2, sort_keys=True)
        f.write("\n")
    print(f"Classification: {args.tier}")
    print(f"  Rationale: {args.rationale}")
    print(f"  Written to: {CLASS_FILE}")


def cmd_findings(args: argparse.Namespace) -> None:
    validate_phase_num(args.phase)
    if not os.path.exists(args.path):
        die(f"findings file does not exist: {args.path}", 1)
    state = load_state()
    state["phases"][str(args.phase)]["findings_path"] = args.path
    save_state(state)
    print(f"Phase {args.phase} findings registered: {args.path}")


def cmd_iter(args: argparse.Namespace) -> None:
    """Increment and print the iteration counter for a phase.

    Used by the build-loop (phase 11) and the bot-fix loop (phase 18) to
    cap themselves at a fixed number of retries without each leaf having
    to manage its own counter file.
    """
    validate_phase_num(args.phase)
    state = load_state()
    phase = state["phases"][str(args.phase)]
    phase["iter_count"] = int(phase.get("iter_count") or 0) + 1
    save_state(state)
    print(phase["iter_count"])


def cmd_set(args: argparse.Namespace) -> None:
    state = load_state()
    state["variables"][args.key] = args.value
    save_state(state)
    print(f"Set {args.key} = {args.value}")


def cmd_get(args: argparse.Namespace) -> None:
    state = load_state()
    value = state["variables"].get(args.key, "")
    release_lock(state)
    print(value)


def cmd_reset(args: argparse.Namespace) -> None:
    # Acquire the lock so a concurrent command cannot race the destructive
    # delete (e.g. a `complete` in flight while reset is wiping state).
    lock = _acquire_lock()
    try:
        if not os.path.exists(STATE_FILE):
            print("No pipeline state to delete.")
            return
        # Only remove the state files we own; preserve .gitignore. The
        # `.lock` file is removed last so the lock fd we still hold is the
        # final reference; the OS releases the flock when we close below.
        for fname in ("state.json", "classification.json", ".lock"):
            path = os.path.join(PIPELINE_DIR, fname)
            if os.path.exists(path):
                os.remove(path)
        # Findings files may have been registered elsewhere — leave them.
        print("Pipeline state cleared.")
    finally:
        fcntl.flock(lock, fcntl.LOCK_UN)
        lock.close()


# ---------- argparse wiring ----------

def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    sub = parser.add_subparsers(dest="command")

    p = sub.add_parser("init", help="Initialise a new pipeline.")
    p.add_argument("feature")
    p.add_argument("--issue", help="GitHub issue number")
    p.add_argument("--module", help="Main module path (e.g. core/decimal/Decimal.hs)")
    p.add_argument("--test", help="Test file path (e.g. core/test/DecimalSpec.hs)")
    p.add_argument("--branch", help="Git branch name (default: feature/<slug>)")
    p.add_argument("--adr", help="ADR number (e.g. 0041)")

    sub.add_parser("status", help="Show pipeline status")
    sub.add_parser("next",   help="Get next ready phase(s) as JSON")

    p = sub.add_parser("complete", help="Mark a phase complete")
    p.add_argument("phase", type=int)

    p = sub.add_parser("approve", help="Approve a PAUSE-gated phase")
    p.add_argument("phase", type=int)

    p = sub.add_parser("classify", help="Persist the feature complexity tier")
    p.add_argument("tier", help=f"one of: {sorted(VALID_TIERS)}")
    p.add_argument("rationale", help="One-line justification (e.g. 'no trust boundary, pure type')")

    p = sub.add_parser("findings", help="Register a findings file for a phase")
    p.add_argument("phase", type=int)
    p.add_argument("path")

    p = sub.add_parser("iter", help="Increment & print this phase's iteration counter")
    p.add_argument("phase", type=int)

    p = sub.add_parser("set", help="Set a pipeline variable")
    p.add_argument("key")
    p.add_argument("value")

    p = sub.add_parser("get", help="Get a pipeline variable")
    p.add_argument("key")

    sub.add_parser("reset", help="Delete pipeline state")

    args = parser.parse_args()
    if not args.command:
        parser.print_help()
        sys.exit(1)

    handlers = {
        "init":     cmd_init,
        "status":   cmd_status,
        "next":     cmd_next,
        "complete": cmd_complete,
        "approve":  cmd_approve,
        "classify": cmd_classify,
        "findings": cmd_findings,
        "iter":     cmd_iter,
        "set":      cmd_set,
        "get":      cmd_get,
        "reset":    cmd_reset,
    }
    handlers[args.command](args)


if __name__ == "__main__":
    main()
