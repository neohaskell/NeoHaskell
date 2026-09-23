"""Paired task-classification experiment: Jev primitives versus Codex Luna Max.

No pipeline execution. Only the request and shared contract reach either model.
Saved references are graded in Python, never by either competing model.
"""
import argparse
from collections import Counter, defaultdict
from datetime import datetime, timezone
import hashlib
import json
import math
import os
from pathlib import Path
import random
import signal
import statistics
import subprocess
import tempfile
import time

from jev_judge import MODEL as JEV_MODEL, finite_number, validate_response
from measure_jev import credential, execute

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
VERSION = "intake-experiment-v1"
LUNA_MODEL = "gpt-5.6-luna"
EFFORT = "max"
PROVIDERS = ("jev", "luna")
DISABLED = (
    "shell_tool", "apps", "multi_agent", "hooks", "plugins", "browser_use",
    "browser_use_external", "computer_use", "code_mode_host", "image_generation",
    "in_app_browser", "in_app_chat", "in_app_local_automation", "remote_plugin",
    "skill_search", "skill_mcp_dependency_install", "sleep_tool", "goals",
    "memories", "view_image",
)


def encode(value):
    return json.dumps(value, sort_keys=True, ensure_ascii=False, allow_nan=False)


def digest(value):
    return hashlib.sha256(value.encode()).hexdigest()


def save(path, value):
    path.write_text(json.dumps(value, indent=2, ensure_ascii=False, allow_nan=False) + "\n")


def question_pack(request, contract):
    """Explicit allowlist: no case ID, reference label, split or rationale."""
    state = {"request": request, "policy": contract["policy"]}
    questions = {
        name: {"type": "choice", "instructions": instruction, "criteria": contract[name]}
        for name, instruction in (
            ("work", "Using `policy`, choose the substantive work type requested in `request`."),
            ("mode", "Using `policy`, choose the execution mode requested in `request`, independently of work type."),
        )
    }
    for action, definition in contract["actions"].items():
        questions[action] = {
            "type": "noul",
            "instructions": f"Using `policy`, does `request` ask for this lifecycle action: {definition} A mention, quotation, negation, or inferred consequence of autonomy is not a request.",
            "criteria": {"true": "This lifecycle action is requested.", "false": "This lifecycle action is not requested."},
        }
    questions["needs_clarification"] = {
        "type": "noul",
        "instructions": "Using `policy`, does `request` require clarification of missing intent or target before a meaningful next playbook step can be selected? An unmatched request needs clarification/redirection. Ordinary repository discovery does not.",
        "criteria": {"true": "Essential intent or target is missing, ambiguous, or outside this taxonomy.", "false": "A meaningful next playbook step can be selected without clarifying intent or target."},
    }
    questions["readiness_score"] = {
        "type": "score", "instructions": "Using `policy`, score how ready `request` is for selecting the next playbook step. This is not implementation readiness or permission to execute.",
        "criteria": contract["readiness_levels"],
    }
    # Every independent question needs the available taxonomy in its state.
    state["task_definitions"] = {k: contract[k] for k in ("work", "mode", "actions")}
    return {"state": state, "questions": questions}


def output_schema(contract):
    properties = {
        "work": {"type": "string", "enum": list(contract["work"])},
        "mode": {"type": "string", "enum": list(contract["mode"])},
        "actions": {"type": "array", "items": {"type": "string", "enum": list(contract["actions"])}},
        "needs_clarification": {"type": "boolean"},
        "readiness_score": {"type": "number", "minimum": 0, "maximum": 3},
    }
    return {"type": "object", "properties": properties, "required": list(properties), "additionalProperties": False}


def luna_prompt(pack):
    return (
        "Classify this software intake request. Do not execute the request or use tools. "
        "Answer the shared questions below using their state, instructions and criteria. "
        "Return one JSON object: work and mode are Choice option IDs; actions is the list "
        "of lifecycle IDs whose yes/no answers are yes; needs_clarification is a boolean; "
        "readiness_score is a number from 0 to 3 under the supplied ordered rubric. "
        "A fractional score is allowed. Do not invent probability or confidence values.\n" + encode(pack)
    )


def validate_profile(value, contract):
    if not isinstance(value, dict) or set(value) != set(output_schema(contract)["properties"]):
        raise ValueError("Profile fields mismatch")
    for field in ("work", "mode"):
        if not isinstance(value[field], str) or value[field] not in contract[field]:
            raise ValueError("Unknown choice")
    actions = value["actions"]
    if (not isinstance(actions, list) or any(not isinstance(a, str) or a not in contract["actions"] for a in actions)
            or len(actions) != len(set(actions))):
        raise ValueError("Unknown or duplicate action")
    if type(value["needs_clarification"]) is not bool or not finite_number(value["readiness_score"], 0, 3):
        raise ValueError("Invalid boolean or score")
    return {**value, "actions": sorted(actions)}


def normalize_jev(response, payload, contract):
    validate_response(response, payload)
    answers = response["answers"]
    return validate_profile({
        "work": answers["work"]["choice"], "mode": answers["mode"]["choice"],
        "actions": [a for a in contract["actions"] if answers[a]["noul"] >= 0.5],
        "needs_clarification": answers["needs_clarification"]["noul"] >= 0.5,
        "readiness_score": answers["readiness_score"]["score"],
    }, contract)


def grade(profile, expected):
    if profile is None:
        return {"profile_exact": False, **{k: False for k in ("work", "mode", "actions", "needs_clarification")}, "score_absolute_error": None}
    matches = {k: (set(profile[k]) == set(expected[k]) if k == "actions" else profile[k] == expected[k])
               for k in ("work", "mode", "actions", "needs_clarification")}
    return {**matches, "profile_exact": all(matches.values()),
            "score_absolute_error": abs(profile["readiness_score"] - expected["readiness_score"])}


def discover_skill_overrides():
    paths = set()
    for root in (Path.home() / ".codex/skills", Path.home() / ".agents/skills", Path.home() / ".codex/plugins/cache"):
        if root.exists():
            result = subprocess.run(["rg", "--files", "--hidden", "--follow", str(root)], capture_output=True, text=True, check=True)
            paths.update(p for p in result.stdout.splitlines() if p.endswith("/SKILL.md"))
    # Codex builds have accepted both file and directory identities. Disable both.
    return sorted(paths | {str(Path(p).parent) for p in paths})


def codex_command(directory, disabled_skills=()):
    cmd = ["codex", "exec", "--ignore-user-config", "--ephemeral", "--skip-git-repo-check",
           "--sandbox", "read-only", "--enable", "skip_host_skill_discovery"]
    for flag in DISABLED:
        cmd.extend(["--disable", flag])
    if disabled_skills:
        cmd.extend(["-c", "skills.config=[" + ",".join("{path=" + json.dumps(p) + ",enabled=false}" for p in disabled_skills) + "]"])
    return cmd + ["-c", 'web_search="disabled"', "-c", "project_doc_max_bytes=0",
                  "-m", LUNA_MODEL, "-c", f'model_reasoning_effort="{EFFORT}"',
                  "--json", "--output-schema", str(directory / "schema.json"),
                  "-o", str(directory / "response.json"), "-C", str(directory), "-"]


def run_process(cmd, prompt, timeout):
    """Keep all output, and kill the process group on a deadline."""
    started = time.perf_counter()
    process = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, text=True, start_new_session=True)
    status = "ok"
    try:
        stdout, stderr = process.communicate(prompt, timeout=timeout)
    except subprocess.TimeoutExpired:
        status = "timeout"
        os.killpg(process.pid, signal.SIGKILL)
        stdout, stderr = process.communicate()
    if status == "ok" and process.returncode:
        status = "process_error"
    return {"status": status, "elapsed_s": time.perf_counter() - started,
            "returncode": process.returncode, "stdout": stdout, "stderr": stderr}


def parse_events(stdout):
    events = [json.loads(line) for line in stdout.splitlines() if line.strip()]
    usage = None
    warnings = []
    for event in events:
        if event["type"] in ("turn.failed", "error"):
            raise ValueError("Codex turn failed")
        if event["type"] == "turn.completed":
            usage = event.get("usage")
        item = event.get("item", {})
        if item and item.get("type") not in ("agent_message", "reasoning", "error"):
            raise ValueError("Unexpected tool or action in Codex output")
        if item.get("type") == "error":
            warnings.append(item.get("message", ""))
            if "Skill descriptions were shortened" in item.get("message", ""):
                raise ValueError("Unexpected global skill catalog in model context")
    if not any(e["type"] == "turn.completed" for e in events):
        raise ValueError("No completed Codex turn")
    return usage, warnings


def run_luna(pack, contract, destination, disabled_skills):
    with tempfile.TemporaryDirectory(prefix="intake-classify-") as temporary:
        directory = Path(temporary)
        save(directory / "schema.json", output_schema(contract))
        cmd = codex_command(directory, disabled_skills)
        save(destination / "command.json", cmd)
        prompt = luna_prompt(pack)
        (destination / "prompt.txt").write_text(prompt)
        result = run_process(cmd, prompt, 180)
        (destination / "events.jsonl").write_text(result.pop("stdout"))
        (destination / "stderr.txt").write_text(result.pop("stderr"))
        if (directory / "response.json").exists():
            (destination / "response.json").write_text((directory / "response.json").read_text())
        if result["status"] == "ok":
            try:
                result["usage"], result["warnings"] = parse_events((destination / "events.jsonl").read_text())
                result["profile"] = validate_profile(json.loads((destination / "response.json").read_text()), contract)
            except (ValueError, KeyError, TypeError, OSError) as error:
                result.update(status="invalid_response", error_type=type(error).__name__)
        return result


def load_cases(path, contract, split):
    dataset = json.loads(path.read_text())
    cases = dataset["cases"]
    ids = [c["id"] for c in cases]
    if len(ids) != len(set(ids)) or any(not c["id"].replace("-", "").isalnum() for c in cases):
        raise ValueError("Duplicate or unsafe case ID")
    requests = [c["request"] for c in cases]
    if len(requests) != len(set(requests)) or any(not r.strip() for r in requests):
        raise ValueError("Duplicate or empty request")
    for case in cases:
        validate_profile(case["expected"], contract)
        if case["split"] not in ("dev", "evaluation") or not case["rationale"]:
            raise ValueError("Missing reference rationale or invalid split")
    selected = [c for c in cases if c["split"] == split]
    if not selected:
        raise ValueError("Empty split")
    return dataset, selected


def schedule(cases, repeat, seed):
    rng = random.Random(seed)
    jobs = []
    for repetition in range(1, repeat + 1):
        order = list(cases)
        rng.shuffle(order)
        for index, case in enumerate(order):
            providers = PROVIDERS if (index + repetition) % 2 else tuple(reversed(PROVIDERS))
            jobs.extend((case, repetition, provider) for provider in providers)
    return jobs


def ratio(top, bottom):
    return top / bottom if bottom else None


def summary(rows, actions):
    n = len(rows)
    valid = [r for r in rows if r["status"] == "ok"]
    metrics = {"attempts": n, "valid": len(valid), "errors": n - len(valid)}
    for field in ("profile_exact", "work", "mode", "actions", "needs_clarification"):
        metrics[field + "_accuracy"] = ratio(sum(r["grade"][field] for r in rows), n)
    metrics["score_mae_valid"] = statistics.mean(r["grade"]["score_absolute_error"] for r in valid) if valid else None
    metrics["score_observations"] = len(valid)
    elapsed = sorted(r["elapsed_s"] for r in rows)
    metrics.update(median_s=statistics.median(elapsed), p95_s=elapsed[math.ceil(.95 * n) - 1])
    for field in ("input_tokens", "cached_input_tokens", "output_tokens", "reasoning_output_tokens"):
        counts = [r.get("usage", {}).get(field) for r in rows if isinstance(r.get("usage"), dict)]
        counts = [x for x in counts if type(x) is int and x >= 0]
        metrics[field] = {"total_observed": sum(counts) if counts else None, "observations": len(counts)}
    per_action = {}
    for action in actions:
        tp = sum(action in r["profile"]["actions"] and action in r["expected"]["actions"] for r in valid)
        fp = sum(action in r["profile"]["actions"] and action not in r["expected"]["actions"] for r in valid)
        fn = sum(action in r["expected"]["actions"] and (r["status"] != "ok" or action not in r["profile"]["actions"]) for r in rows)
        per_action[action] = {"tp": tp, "fp": fp, "fn": fn, "f1": ratio(2 * tp, 2 * tp + fp + fn)}
    metrics["per_action"] = per_action
    tp, fp, fn = (sum(v[k] for v in per_action.values()) for k in ("tp", "fp", "fn"))
    metrics["action_micro_f1"] = ratio(2 * tp, 2 * tp + fp + fn)
    f1s = [v["f1"] for v in per_action.values() if v["f1"] is not None]
    metrics["action_macro_f1"] = statistics.mean(f1s) if f1s else None
    metrics["unrequested_actions"] = fp
    metrics["missed_clarifications"] = sum(r["expected"]["needs_clarification"] and (r["status"] != "ok" or not r["profile"]["needs_clarification"]) for r in rows)
    metrics["unneeded_clarifications"] = sum(r["profile"]["needs_clarification"] and not r["expected"]["needs_clarification"] for r in valid)
    groups = defaultdict(list)
    for row in rows:
        groups[row["case_id"]].append(row)
    repeated = [rs for rs in groups.values() if len(rs) > 1]
    metrics["repeat_stability"] = {
        "cases": len(repeated),
        "all_valid_same_profile": sum(all(r["status"] == "ok" for r in rs) and len({encode({k: r["profile"][k] for k in ("work", "mode", "actions", "needs_clarification")}) for r in rs}) == 1 for rs in repeated),
    }
    metrics["per_case"] = {key: {"attempts": len(rs), "exact": sum(r["grade"]["profile_exact"] for r in rs)} for key, rs in groups.items()}
    return metrics


def build_report(metadata, rows):
    expected_jobs = {(c["id"], r, p) for c in metadata["cases"] for r in range(1, metadata["repeat"] + 1) for p in PROVIDERS}
    actual_jobs = {(r["case_id"], r["repeat"], r["provider"]) for r in rows}
    if actual_jobs != expected_jobs or len(rows) != len(expected_jobs):
        raise ValueError("Incomplete or duplicated pairs; cannot report as a completed comparison")
    result = {p: summary([r for r in rows if r["provider"] == p], metadata["contract"]["actions"]) for p in PROVIDERS}
    pairs = defaultdict(dict)
    for row in rows:
        pairs[(row["case_id"], row["repeat"])][row["provider"]] = row
    result["paired"] = dict(Counter(
        "both_correct" if pair["jev"]["grade"]["profile_exact"] and pair["luna"]["grade"]["profile_exact"] else
        "jev_only" if pair["jev"]["grade"]["profile_exact"] else
        "luna_only" if pair["luna"]["grade"]["profile_exact"] else "both_wrong"
        for pair in pairs.values()))
    result["median_paired_latency_ratio_luna_over_jev"] = statistics.median(pair["luna"]["elapsed_s"] / pair["jev"]["elapsed_s"] for pair in pairs.values())
    # Resample cases, not individual repeats. Exploratory interval, not a promotion test.
    differences = []
    for case in metadata["cases"]:
        rs = [pair for (identity, _), pair in pairs.items() if identity == case["id"]]
        differences.append(statistics.mean(int(p["jev"]["grade"]["profile_exact"]) - int(p["luna"]["grade"]["profile_exact"]) for p in rs))
    rng = random.Random(1729)
    draws = sorted(statistics.mean(rng.choices(differences, k=len(differences))) for _ in range(2000))
    result["accuracy_difference_jev_minus_luna"] = {"mean": statistics.mean(differences), "case_bootstrap_95_percentile": [draws[49], draws[1949]], "unique_case_count": len(differences)}
    return result


def load_run(directory):
    metadata = json.loads((directory / "metadata.json").read_text())
    rows = [json.loads(line) for line in (directory / "samples.jsonl").read_text().splitlines()]
    references = {c["id"]: c for c in metadata["cases"]}
    for row in rows:
        case = references[row["case_id"]]
        if row["expected"] != case["expected"]:
            raise ValueError("Saved reference changed")
        sample_path = directory / f"{row['case_id']}-{row['provider']}-r{row['repeat']}"
        pack = json.loads((sample_path / "questions.json").read_text())
        if row["question_pack_sha256"] != digest(encode(pack)):
            raise ValueError("Saved question pack changed")
        profile = validate_profile(row["profile"], metadata["contract"]) if row["status"] == "ok" else None
        if row["grade"] != grade(profile, case["expected"]):
            raise ValueError("Saved grade changed")
    return metadata, build_report(metadata, rows)


def compare_runs(baseline_path, candidate_path, max_latency_ratio):
    baseline_meta, baseline = load_run(baseline_path)
    candidate_meta, candidate = load_run(candidate_path)
    for field in ("contract", "cases", "repeat", "split", "seed", "requested_models", "luna_effort", "codex_version", "disabled_skill_paths"):
        if baseline_meta[field] != candidate_meta[field]:
            raise ValueError("Incomparable runs: " + field)
    checks = {}
    for provider in PROVIDERS:
        before, after = baseline[provider], candidate[provider]
        checks[provider] = {
            "exact_accuracy_no_drop": after["profile_exact_accuracy"] >= before["profile_exact_accuracy"],
            "errors_no_increase": after["errors"] <= before["errors"],
            "unrequested_actions_no_increase": after["unrequested_actions"] <= before["unrequested_actions"],
            "missed_clarifications_no_increase": after["missed_clarifications"] <= before["missed_clarifications"],
            "median_latency_within_budget": after["median_s"] <= before["median_s"] * max_latency_ratio,
        }
    return {"passed": all(all(c.values()) for c in checks.values()), "checks": checks,
            "max_latency_ratio": max_latency_ratio,
            "policy": "Provisional development regression gate, not statistical evidence or a production promotion rule."}


def markdown_report(metadata, result, rows):
    text = ["# Intake classifier experiment", "", f"Run: {metadata['started_utc']}. {len(metadata['cases'])} synthetic {metadata['split']} cases × {metadata['repeat']} repeats per provider.", "",
            "Jev `jev-1.13.0` versus `codex exec -m gpt-5.6-luna -c model_reasoning_effort=\"max\"`. Same semantic questions and reference labels; different provider interfaces and system context.", "",
            "| Metric | Jev | Luna Max |", "|---|---:|---:|"]
    for label, field, fmt in (
        ("Exact profile incl. clarification", "profile_exact_accuracy", ".1%"),
        ("Work type", "work_accuracy", ".1%"), ("Execution mode", "mode_accuracy", ".1%"),
        ("Action set", "actions_accuracy", ".1%"), ("Clarification", "needs_clarification_accuracy", ".1%"),
        ("Readiness MAE (valid only)", "score_mae_valid", ".3f"),
        ("Median seconds (all attempts)", "median_s", ".3f"), ("p95 seconds (descriptive)", "p95_s", ".3f"),
        ("Errors", "errors", "d"), ("Unrequested actions", "unrequested_actions", "d"),
        ("Missed clarifications", "missed_clarifications", "d"),
    ):
        values = [format(result[p][field], fmt) if result[p][field] is not None else "unavailable" for p in PROVIDERS]
        text.append(f"| {label} | {values[0]} | {values[1]} |")
    text += ["", "Paired exact-profile outcomes: `" + encode(result["paired"]) + "`.", "",
             "Exploratory accuracy difference (Jev minus Luna), case-cluster bootstrap: `" + encode(result["accuracy_difference_jev_minus_luna"]) + "`.", "",
             "## Limits", "", "Labels and numeric readiness anchors are author-draft, not independently human-reviewed. This small synthetic set measures task classification, not localization, complete intake, or execution safety. Repeats are not independent new tasks. Neither model grades the other. No monetary cost is inferred from token counts or subscription access.", "",
             "Luna is requested at the pinned model/effort; the CLI event stream does not attest the server's resolved model. Each sample is a new ephemeral Codex process in an empty directory, with repository instructions and tools disabled and per-skill disable overrides. Raw warnings, exact commands, disabled-skill inventory and token usage are retained. Provider system instructions differ: this is a deployment-path comparison, not a context-identical model benchmark. Timing includes CLI startup or API round trip respectively, and excludes offline grading. Provider caching and network/service conditions are uncontrolled; order alternates and case order is seeded. Application retries=0; provider/CLI transport retries may occur internally.", "",
             "Jev uses Choice, independent Noul questions (fixed threshold 0.5), and Score in one batch. Luna returns the corresponding schema-constrained values from the same question pack. Native Jev confidence is preserved but no confidence gate is fitted on these evaluation results; Luna is not asked to fabricate comparable confidence. Readiness MAE compares a distribution-weighted Jev score with Luna's direct score against provisional ordinal anchors.", "",
             "## Errors and disagreements", ""]
    for row in rows:
        if row["status"] != "ok" or not row["grade"]["profile_exact"]:
            text.append(f"- {row['case_id']} repeat {row['repeat']} {row['provider']}: {row['status']}; expected `{encode(row['expected'])}`; observed `{encode(row.get('profile'))}`.")
    text += ["", "Full metrics and usage coverage: [summary.json](summary.json). Exact inputs, outputs and commands: per-sample directories. Frozen labels, contract and source hashes: [metadata.json](metadata.json).", ""]
    return "\n".join(text)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--compare", type=Path, nargs=2, metavar=("BASELINE", "CANDIDATE"))
    parser.add_argument("--max-latency-ratio", type=float, default=1.25)
    parser.add_argument("--split", choices=("dev", "evaluation"), default="dev")
    parser.add_argument("--repeat", type=int, default=2)
    parser.add_argument("--seed", type=int, default=1729)
    parser.add_argument("--cases", type=Path, default=HERE / "intake_cases.json")
    parser.add_argument("--contract", type=Path, default=HERE / "intake_contract.json")
    args = parser.parse_args()
    if args.compare:
        if not finite_number(args.max_latency_ratio, 1, 100):
            parser.error("--max-latency-ratio must be between 1 and 100")
        result = compare_runs(*args.compare, args.max_latency_ratio)
        print(json.dumps(result, indent=2))
        return int(not result["passed"])
    if args.output is None:
        parser.error("--output is required for a live run")
    if args.repeat < 1:
        parser.error("--repeat must be positive")
    contract = json.loads(args.contract.read_text())
    dataset, cases = load_cases(args.cases, contract, args.split)
    key = credential()
    cli_version = subprocess.check_output(["codex", "--version"], text=True).strip()
    disabled_skills = discover_skill_overrides()
    jobs = schedule(cases, args.repeat, args.seed)
    args.output.mkdir(parents=True, exist_ok=False)
    sources = [Path(__file__), HERE / "jev_judge.py", HERE / "measure_jev.py", args.contract, args.cases]
    metadata = {
        "version": VERSION, "started_utc": datetime.now(timezone.utc).isoformat(),
        "split": args.split, "repeat": args.repeat, "seed": args.seed,
        "contract": contract, "cases": cases, "label_status": dataset["label_status"],
        "requested_models": {"jev": JEV_MODEL, "luna": LUNA_MODEL}, "luna_effort": EFFORT,
        "codex_version": cli_version, "codex_command_template": codex_command(Path("<temporary>"), disabled_skills),
        "disabled_skill_paths": disabled_skills,
        "git_head": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
        "git_branch": subprocess.check_output(["git", "branch", "--show-current"], cwd=ROOT, text=True).strip(),
        "source_hashes": {p.name: digest(p.read_text()) for p in sources},
        "concurrency": 1, "application_retries": 0, "timeouts_s": {"jev": 45, "luna": 180},
        "gold_sent_to_models": False, "cost": None,
        "schedule": [{"case_id": c["id"], "repeat": r, "provider": p} for c, r, p in jobs],
    }
    save(args.output / "metadata.json", metadata)
    snapshot = args.output / "source"
    snapshot.mkdir()
    for source in sources:
        (snapshot / source.name).write_text(source.read_text())
    rows = []
    for case, repeat, provider in jobs:
        name = f"{case['id']}-{provider}-r{repeat}"
        directory = args.output / name
        directory.mkdir()
        pack = question_pack(case["request"], contract)
        save(directory / "questions.json", pack)
        started = time.perf_counter()
        try:
            if provider == "jev":
                payload = {"model": JEV_MODEL, **pack}
                save(directory / "request.json", payload)
                result = execute(payload, key)
                if "result" in result:
                    save(directory / "response.json", result["result"])
                    result["usage"] = result["result"].get("usage")
                if result["status"] == "ok":
                    try:
                        result["profile"] = normalize_jev(result["result"], payload, contract)
                    except (ValueError, KeyError, TypeError, AttributeError) as error:
                        result.update(status="invalid_response", error_type=type(error).__name__)
            else:
                result = run_luna(pack, contract, directory, disabled_skills)
        except (OSError, ValueError) as error:
            result = {"status": "adapter_error", "error_type": type(error).__name__, "elapsed_s": time.perf_counter() - started}
        row = {"case_id": case["id"], "repeat": repeat, "provider": provider,
               "expected": case["expected"], "question_pack_sha256": digest(encode(pack)), **result}
        row["grade"] = grade(row.get("profile") if row["status"] == "ok" else None, case["expected"])
        rows.append(row)
        with (args.output / "samples.jsonl").open("a") as output:
            output.write(encode(row) + "\n")
        print(encode({"sample": name, "status": row["status"], "elapsed_s": round(row["elapsed_s"], 3)}), flush=True)
    result = build_report(metadata, rows)
    save(args.output / "summary.json", result)
    (args.output / "report.md").write_text(markdown_report(metadata, result, rows))
    print("Report: " + str(args.output / "report.md"), flush=True)
    return int(any(r["status"] != "ok" for r in rows))


if __name__ == "__main__":
    raise SystemExit(main())
