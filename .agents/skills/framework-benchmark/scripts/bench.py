#!/usr/bin/env python3
from __future__ import annotations

import argparse
import datetime as dt
import importlib.util
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True

SCRIPT = Path(__file__).resolve()
MAX_OUTPUT_BYTES = 32 * 1024 * 1024
ANSI = re.compile(r"\x1b\[[0-?]*[ -/]*[@-~]")
SUMMARY = re.compile(r"^\s*(\d+)\s+examples?,\s*(\d+)\s+failures?\s*$", re.MULTILINE)


def find_root_helper() -> Path:
    suffix = Path(".agents/skills/neohaskell-best-practices/scripts/repo-root")
    for parent in SCRIPT.parents:
        candidate = parent / suffix
        if candidate.is_file():
            return candidate
    raise RuntimeError(f"repository root helper was not found from {SCRIPT}")


def resolve_root(invocation_cwd: Path, env: dict[str, str]) -> Path:
    root_helper = find_root_helper()
    result = subprocess.run(
        [str(root_helper)],
        cwd=invocation_cwd,
        env=env,
        capture_output=True,
        text=True,
        timeout=10,
        check=False,
    )
    if result.returncode != 0:
        raise RuntimeError(result.stderr.strip() or "repository root was not found")
    return Path(result.stdout.strip()).resolve()


def load_process_runner(root: Path):
    helper = root / ".agents/skills/neohaskell-best-practices/scripts/bounded_process.py"
    spec = importlib.util.spec_from_file_location("neohaskell_bounded_process", helper)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not load bounded process helper at {helper}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module.run_bounded


def load_config(path: Path) -> dict:
    config = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(config, dict) or config.get("schema") != 1:
        raise ValueError("nightly suite config needs schema 1")
    if not isinstance(config.get("mode"), str) or not config["mode"].strip():
        raise ValueError("nightly suite config needs a mode description")
    entries = config.get("entries")
    if not isinstance(entries, list) or not entries:
        raise ValueError("nightly suite config needs at least one entry")
    names: set[str] = set()
    for entry in entries:
        if not isinstance(entry, dict):
            raise ValueError("each nightly suite entry must be an object")
        for field in ("name", "suite", "match"):
            if not isinstance(entry.get(field), str) or not entry[field].strip():
                raise ValueError(f"nightly suite entry needs a nonempty {field}")
        if entry["name"] in names:
            raise ValueError(f"duplicate nightly suite name {entry['name']}")
        names.add(entry["name"])
        if entry.get("requires") not in (None, "postgres"):
            raise ValueError(f"unsupported prerequisite for {entry['name']}")
        timeout = entry.get("timeout_s", 1800)
        if (
            isinstance(timeout, bool)
            or not isinstance(timeout, (int, float))
            or not math.isfinite(timeout)
            or timeout <= 0
        ):
            raise ValueError(f"{entry['name']} timeout_s must be finite and positive")
        budget = entry.get("budget_s")
        if budget is not None and (
            isinstance(budget, bool)
            or not isinstance(budget, (int, float))
            or not math.isfinite(budget)
            or budget <= 0
        ):
            raise ValueError(f"{entry['name']} budget_s must be null or finite and positive")
    return config


def hspec_summary(output: bytes) -> tuple[int, int] | None:
    text = ANSI.sub("", output.decode("utf-8", errors="replace"))
    matches = SUMMARY.findall(text)
    if len(matches) != 1:
        return None
    return int(matches[0][0]), int(matches[0][1])


def suite_command(root: Path, entry: dict) -> list[str]:
    wrapper = root / ".agents/skills/neohaskell-best-practices/scripts/with-toolchain"
    return [
        str(wrapper),
        "cabal",
        "test",
        entry["suite"],
        "--project-file=cabal.project",
        "--test-show-details=direct",
        "--test-option=--match",
        f"--test-option={entry['match']}",
    ]


def run_entry(
    entry: dict,
    *,
    root: Path,
    output_dir: Path,
    env: dict[str, str],
    runner,
    strict: bool,
) -> dict:
    output_dir.mkdir(parents=True, exist_ok=True)
    log_name = f"{entry['name']}.log"
    log_path = output_dir / log_name
    command = suite_command(root, entry)
    started = dt.datetime.now(dt.timezone.utc).isoformat()
    if entry.get("requires") == "postgres" and env.get("POSTGRES_AVAILABLE") != "true":
        status = "failed-skip" if strict else "skipped"
        log_path.write_text("PostgreSQL prerequisite is unavailable.\n", encoding="utf-8")
        return {
            "name": entry["name"],
            "status": status,
            "reason": "PostgreSQL prerequisite is unavailable",
            "started_at": started,
            "command": command,
            "wallclock_s": None,
            "completed_examples": 0,
            "failures": None,
            "budget_s": entry.get("budget_s"),
            "timeout_s": entry.get("timeout_s", 1800),
            "log": log_name,
        }

    child_env = env.copy()
    child_env["NEOHASKELL_ROOT"] = str(root)
    try:
        result = runner(
            command,
            cwd=root,
            env=child_env,
            timeout=entry.get("timeout_s", 1800),
            max_output_bytes=MAX_OUTPUT_BYTES,
        )
    except OSError as error:
        log_path.write_text(str(error) + "\n", encoding="utf-8")
        return {
            "name": entry["name"],
            "status": "failed-start",
            "reason": str(error),
            "started_at": started,
            "command": command,
            "wallclock_s": None,
            "completed_examples": 0,
            "failures": None,
            "budget_s": entry.get("budget_s"),
            "timeout_s": entry.get("timeout_s", 1800),
            "log": log_name,
        }

    log_path.write_bytes(result.output)
    outcome = getattr(result.outcome, "name", str(result.outcome))
    elapsed = result.elapsed_seconds
    if (
        isinstance(elapsed, bool)
        or not isinstance(elapsed, (int, float))
        or not math.isfinite(elapsed)
        or elapsed < 0
    ):
        raise ValueError("bounded process helper returned an invalid elapsed_seconds value")
    record = {
        "name": entry["name"],
        "status": "failed",
        "started_at": started,
        "command": command,
        "wallclock_s": round(float(elapsed), 3),
        "completed_examples": 0,
        "failures": None,
        "budget_s": entry.get("budget_s"),
        "timeout_s": entry.get("timeout_s", 1800),
        "log": log_name,
    }
    if outcome == "TIMED_OUT":
        record.update(
            status="timeout",
            reason=f"suite command timed out after {record['timeout_s']} seconds",
        )
        return record
    if outcome == "CANCELLED":
        record.update(status="cancelled", reason="suite command was cancelled")
        return record
    if outcome == "OUTPUT_LIMITED":
        record.update(status="output-limited", reason=f"suite output exceeded {MAX_OUTPUT_BYTES} bytes")
        return record
    if outcome != "EXITED":
        record.update(
            status="failed-process",
            reason=f"bounded process returned unknown outcome {outcome}",
        )
        return record
    if result.returncode != 0:
        record.update(status="suite-failed", reason=f"suite command exited with status {result.returncode}")
        return record
    summary = hspec_summary(result.output)
    if summary is None or summary[0] < 1:
        record.update(
            status="vacuous",
            reason="expected exactly one Hspec summary with at least one example",
        )
        return record
    examples, failures = summary
    record.update(completed_examples=examples, failures=failures)
    if failures:
        record.update(status="test-failed", reason=f"Hspec reported {failures} failure(s)")
        return record
    budget = entry.get("budget_s")
    if budget is not None and elapsed > budget:
        record.update(
            status="budget-breach",
            reason=f"suite wallclock {elapsed:.3f}s exceeded {budget}s",
        )
    elif budget is None:
        record.update(status="calibration")
    else:
        record.update(status="passed")
    return record


def atomic_json(path: Path, value: dict) -> None:
    temporary: str | None = None
    try:
        with tempfile.NamedTemporaryFile("w", encoding="utf-8", dir=path.parent, prefix=f".{path.name}.", delete=False) as file:
            temporary = file.name
            file.write(json.dumps(value, indent=2) + "\n")
        os.replace(temporary, path)
    finally:
        if temporary and os.path.exists(temporary):
            os.unlink(temporary)


def record_suites(
    config: dict,
    *,
    root: Path,
    output_dir: Path,
    strict: bool = False,
    runner=None,
    env=None,
) -> dict:
    output_dir.mkdir(parents=True, exist_ok=False)
    env = (os.environ if env is None else env).copy()
    runner = load_process_runner(root) if runner is None else runner
    result = {
        "schema": 1,
        "status": "incomplete",
        "started_at": dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "mode": config["mode"],
        "measurement": (
            "suite wallclock includes toolchain startup and any build work; "
            "it is not runtime latency or framework throughput"
        ),
        "entries": [],
    }
    results_path = output_dir / "results.json"
    atomic_json(results_path, result)
    for entry in config["entries"]:
        record = run_entry(
            entry,
            root=root,
            output_dir=output_dir,
            env=env,
            runner=runner,
            strict=strict,
        )
        result["entries"].append(record)
        atomic_json(results_path, result)
        print(f"bench: {record['status']} {record['name']}")
    failures = [record for record in result["entries"] if record["status"] not in {"passed", "calibration", "skipped"}]
    result["status"] = "failed" if failures else "completed"
    result["finished_at"] = dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    atomic_json(results_path, result)
    print(f"bench: evidence {results_path}")
    return result


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Record bounded nightly Hspec suite durations and counts."
    )
    parser.add_argument("--config", type=Path, default=SCRIPT.parents[1] / "references/nightly-suites.json")
    parser.add_argument("--output-dir", type=Path)
    parser.add_argument("--strict", action="store_true", help="fail when a configured suite is skipped")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    invocation_cwd = Path.cwd().resolve()
    env = os.environ.copy()
    try:
        root = resolve_root(invocation_cwd, env)
        config_path = args.config.resolve()
        config = load_config(config_path)
        state_root = (root / ".agents-state/framework-benchmark").resolve()
        if args.output_dir:
            output_dir = args.output_dir.resolve()
            if not output_dir.is_relative_to(state_root):
                raise ValueError("evidence output must stay inside .agents-state/framework-benchmark")
            output_dir.parent.mkdir(parents=True, exist_ok=True)
        else:
            nightly_root = state_root / "nightly"
            nightly_root.mkdir(parents=True, exist_ok=True)
            output_dir = Path(tempfile.mkdtemp(prefix="run-", dir=nightly_root))
            output_dir.rmdir()
        result = record_suites(config, root=root, output_dir=output_dir, strict=args.strict, env=env)
    except (OSError, RuntimeError, ValueError, json.JSONDecodeError, subprocess.SubprocessError) as error:
        print(f"framework-benchmark: {error}", file=sys.stderr)
        return 2
    return 1 if result["status"] == "failed" else 0


if __name__ == "__main__":
    raise SystemExit(main())
