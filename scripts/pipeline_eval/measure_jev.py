"""Local experiment runner for the intake judge, independent of the pipeline.

Example: python3 scripts/pipeline_eval/measure_jev.py --input samples.jsonl
    --output /tmp/jev-run --legacy-metadata old-run/metadata.json --repeat 2
"""
import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import shlex
import statistics
import time
import urllib.error
import urllib.request

import jev_judge as judge

ROOT = Path(__file__).resolve().parents[2]


def digest(value):
    return hashlib.sha256(value.encode()).hexdigest()


def credential():
    key = os.environ.get("TYPESAFE_API_KEY")
    if key:
        return key
    values = {}
    for line in (Path.home() / ".jevenv").read_text().splitlines():
        line = line.strip()
        if line.startswith("export "):
            line = line[7:].strip()
        name, sep, value = line.partition("=")
        if not sep or line.startswith("#"):
            continue
        parts = shlex.split(value, comments=True)
        if len(parts) == 1:
            values[name.strip()] = parts[0]
    key = values.get("TYPESAFE_API_KEY") or values.get("JEV_API_KEY")
    if not key:
        raise ValueError("Configure TYPESAFE_API_KEY or ~/.jevenv")
    return key


def cases_from_samples(path, controls):
    samples = [json.loads(line) for line in path.read_text().splitlines() if line.strip()]
    seen = set()
    cases = []
    for index, sample in enumerate(samples):
        identity = sample["id"]
        if identity in seen:
            raise ValueError("Duplicate sample ID")
        seen.add(identity)
        route = sample["response"]["route"]
        base = {"request": sample["request"], "expected": sample["expected"]}
        cases.append({**base, "id": identity, "proposed": route, "group": "observed"})
        if controls:
            # Both positive and negative controls use existing reference labels.
            # They are development mutations, never described as unseen cases.
            wrong = next(s["expected"] for s in samples[index + 1:] + samples[:index + 1]
                         if s["expected"] != sample["expected"])
            cases.extend([
                {**base, "id": identity + "-positive", "proposed": base["expected"], "group": "positive-control"},
                {**base, "id": identity + "-negative", "proposed": wrong, "group": "negative-control"},
            ])
    if not cases:
        raise ValueError("Input contains no cases")
    return cases


def legacy_request(case, sources, questions):
    return {"model": judge.MODEL, "state": {"request": case["request"],
            "observed_route": case["proposed"], "maps": sources}, "questions": questions}


def legacy_verdict(response):
    a = response["answers"]
    c, p, score = a["assessment"], a["route_supported"]["noul"], a["evidence_fit"]["score"]
    ranked = sorted(c["probabilities"].values(), reverse=True)
    decisive = c["confidence"] >= judge.POLICY["confidence"] and ranked[0] - ranked[1] >= judge.POLICY["margin"]
    if decisive and c["choice"] == "supported" and p >= judge.POLICY["support"] and score >= judge.POLICY["score"]:
        return {"verdict": "accept", "reason": "legacy questions under the same uncertainty thresholds"}
    if decisive and c["choice"] == "unsupported" and p <= 1 - judge.POLICY["support"] and score <= 1:
        return {"verdict": "reject", "reason": "legacy questions under the same uncertainty thresholds"}
    return {"verdict": "review", "reason": "uncertain, incomplete or conflicting judgments"}


def execute(payload, key):
    encoded = json.dumps(payload, sort_keys=True).encode()
    request = urllib.request.Request("https://api.typesafe.ai/v1/systemone", data=encoded,
                                    headers={"Authorization": "Bearer " + key, "Content-Type": "application/json"})
    started = time.perf_counter()
    try:
        with urllib.request.urlopen(request, timeout=45) as response:
            result = json.load(response)
        return {"status": "ok", "result": result, "elapsed_s": time.perf_counter() - started}
    except urllib.error.HTTPError as error:
        return {"status": "error", "http_status": error.code, "elapsed_s": time.perf_counter() - started}
    except Exception as error:
        # Exception strings can include request information. Persist only type.
        return {"status": "error", "error_type": type(error).__name__, "elapsed_s": time.perf_counter() - started}


def summarize(rows):
    ok = [r for r in rows if r["status"] == "ok"]
    positive = [r for r in rows if r["gold_correct"]]
    negative = [r for r in rows if not r["gold_correct"]]
    accepted = [r for r in ok if r["verdict"] == "accept"]
    rejected = [r for r in ok if r["verdict"] == "reject"]
    automatic = accepted + rejected
    n = len(rows)
    observed_tokens = []
    for row in rows:
        usage = row.get("result", {}).get("usage")
        value = usage.get("input_tokens") if isinstance(usage, dict) else None
        if type(value) is int and value >= 0:
            observed_tokens.append(value)
    return {
        "attempts": n, "valid_responses": len(ok), "errors": n - len(ok),
        "positive_samples": len(positive), "negative_samples": len(negative),
        "accepted": len(accepted), "rejected": len(rejected),
        "review": sum(r["verdict"] == "review" for r in ok),
        "automatic_coverage": len(automatic) / n,
        "automatic_accuracy": (sum((r["verdict"] == "accept") == r["gold_correct"] for r in automatic) / len(automatic)) if automatic else None,
        "false_acceptances": sum(not r["gold_correct"] for r in accepted),
        "false_rejections": sum(r["gold_correct"] for r in rejected),
        "raw_binary_correct": sum(r["raw_supported"] == r["gold_correct"] for r in ok if r["raw_supported"] is not None),
        "raw_binary_decided": sum(r["raw_supported"] is not None for r in ok),
        "choice_support_brier": statistics.mean((r["support_probability"] - int(r["gold_correct"])) ** 2 for r in ok) if ok else None,
        "clarification_label_accuracy": None,
        "median_elapsed_s_all_attempts": statistics.median(r["elapsed_s"] for r in rows),
        "p95_elapsed_s_descriptive": sorted(r["elapsed_s"] for r in rows)[math.ceil(.95 * n) - 1],
        "input_tokens_observed": sum(observed_tokens) if observed_tokens else None,
        "usage_observed_samples": len(observed_tokens),
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--legacy-metadata", type=Path)
    parser.add_argument("--repeat", type=int, default=2)
    parser.add_argument("--controls", action="store_true")
    args = parser.parse_args()
    if args.repeat < 1:
        parser.error("--repeat must be positive")
    sources = {name: (ROOT / name).read_text() for name in ("codemap/capabilities.yaml", "codemap/extension-points.yaml")}
    catalog = judge.catalog_from_sources(sources)
    cases = cases_from_samples(args.input, args.controls)
    legacy = json.loads(args.legacy_metadata.read_text())["questions"] if args.legacy_metadata else None
    versions = ["v1", judge.VERSION] if legacy else [judge.VERSION]
    # Validate all cases before consuming API budget or creating output.
    for case in cases:
        judge.build_request(case["request"], case["proposed"], catalog, sources)
        if case["expected"] not in catalog:
            raise ValueError("Reference answer absent from catalog")
    key = credential()
    args.output.mkdir(parents=True, exist_ok=False)
    metadata = {
        "version": judge.VERSION, "model": judge.MODEL, "policy": judge.POLICY,
        "source_hashes": {k: digest(v) for k, v in sources.items()},
        "judge_sha256": digest(Path(judge.__file__).read_text()),
        "questions_sha256": digest(Path(judge.__file__).with_name("questions.json").read_text()),
        "runner_sha256": digest(Path(__file__).read_text()),
        "input_sha256": digest(args.input.read_text()), "cases": cases,
        "legacy_questions": legacy, "repeat": args.repeat, "concurrency": 1,
        "retry_count": 0, "timeout_s": 45, "versions": versions,
        "policy_status": "provisional, fixed before this comparison; not calibrated",
        "scope": "Judge comparison on existing routing outputs and optional development mutations, not full intake",
        "cache": "provider managed, uncontrolled", "gold_sent_to_model": False,
    }
    (args.output / "metadata.json").write_text(json.dumps(metadata, indent=2))
    rows = []
    for repeat in range(args.repeat):
        for case in cases:
            # Alternate order to reduce consistent first/second-run cache bias.
            for version in versions if repeat % 2 == 0 else list(reversed(versions)):
                payload = (legacy_request(case, sources, legacy) if version == "v1"
                           else judge.build_request(case["request"], case["proposed"], catalog, sources))
                name = f"{case['id']}-{version}-r{repeat + 1}"
                (args.output / (name + ".request.json")).write_text(json.dumps(payload, indent=2))
                row = {"id": name, "case_id": case["id"], "group": case["group"], "version": version,
                       "repeat": repeat + 1, "gold_correct": case["proposed"] == case["expected"],
                       "reference_verdict": judge.reference_verdict(case["proposed"], case["expected"]),
                       "request_sha256": digest(json.dumps(payload, sort_keys=True)), **execute(payload, key)}
                if row["status"] == "ok":
                    try:
                        judge.validate_response(row["result"], payload)
                        row.update(legacy_verdict(row["result"]) if version == "v1"
                                   else judge.verdict(row["result"], payload, case["proposed"]))
                        a = row["result"]["answers"]
                        row["support_probability"] = a["assessment"]["probabilities"]["supported"]
                        row["raw_supported"] = (None if a["assessment"]["choice"] == "insufficient_evidence"
                                                else a["assessment"]["choice"] == "supported")
                    except (ValueError, TypeError, KeyError, AttributeError) as error:
                        row.update(status="invalid_response", error_type=type(error).__name__)
                rows.append(row)
                with (args.output / "samples.jsonl").open("a") as file:
                    file.write(json.dumps(row) + "\n")
                print(json.dumps({k: row[k] for k in ("id", "status", "elapsed_s", "verdict") if k in row}), flush=True)
    summary = {version: {group: summarize([r for r in rows if r["version"] == version and (group == "all" or r["group"] == group)])
                         for group in ["all"] + sorted({r["group"] for r in rows})} for version in versions}
    (args.output / "summary.json").write_text(json.dumps(summary, indent=2))
    print(json.dumps(summary, indent=2))
    return int(any(r["status"] != "ok" for r in rows))


if __name__ == "__main__":
    raise SystemExit(main())
