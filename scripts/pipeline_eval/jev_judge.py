"""Experimental intake judge. No pipeline changes or model calls on import.

Ground truth belongs to the caller and is never an argument to build_request.
See .agents/skills/typesafe-ai/SKILL.md and TypeSafe's citation-check cookbook.
"""

import math
import json
from pathlib import Path
import re

VERSION = "intake-judge-v5-guarded"
MODEL = "jev-1.13.0"
# Provisional development policy, frozen before comparison; not calibrated.
POLICY = {"confidence": 0.8, "support": 0.8, "score": 2.5, "margin": 0.2}


def catalog_from_sources(sources):
    """Split only the maps' explicit top-level rows; retain evidence verbatim.

    This is a bounded row scanner, not a general YAML parser. It rejects changed
    headers and duplicates rather than silently constructing an incomplete menu.
    """
    catalog = {}
    for path, source in sources.items():
        kind = "extension" if path.endswith("extension-points.yaml") else "capability"
        field = "kind" if kind == "extension" else "id"
        marks = list(re.finditer(r"^  - " + field + r": ([a-z][a-z0-9-]*)\s*$", source, re.M))
        all_rows = re.findall(r"^  - \S", source, re.M)
        if not marks or len(marks) != len(all_rows):
            raise ValueError("Unrecognized catalog row layout: " + path)
        for index, mark in enumerate(marks):
            route = mark.group(1)
            if route in catalog:
                raise ValueError("Duplicate route: " + route)
            end = marks[index + 1].start() if index + 1 < len(marks) else len(source)
            evidence = "\n".join(line for line in source[mark.start():end].splitlines()
                                 if not line.lstrip().startswith("#")).strip()
            catalog[route] = {"kind": kind, "source": path, "evidence": evidence}
    return catalog


def build_request(request, proposed_route, catalog, sources=None):
    if not isinstance(request, str) or not request.strip():
        raise ValueError("A nonempty request is required")
    if not isinstance(proposed_route, str) or proposed_route not in catalog:
        raise ValueError("Unknown proposed route")
    if sources is None:
        sources = {route: row["evidence"] for route, row in catalog.items()}
    questions = json.loads(Path(__file__).with_name("questions.json").read_text())
    return {"model": MODEL,
            "state": {"request": request, "observed_route": proposed_route, "maps": sources},
            "questions": questions}


def finite_number(value, low, high):
    return (type(value) in (int, float) and math.isfinite(value)
            and low <= value <= high)


def validate_response(response, payload):
    if response.get("model") != payload["model"]:
        raise ValueError("Model identity mismatch")
    answers = response.get("answers")
    if not isinstance(answers, dict) or answers.keys() != payload["questions"].keys():
        raise ValueError("Missing or extra answers")
    for name, question in payload["questions"].items():
        answer = answers[name]
        kind = question["type"]
        if not isinstance(answer, dict) or answer.get("type") != kind:
            raise ValueError("Answer type mismatch: " + name)
        if kind == "noul":
            if not finite_number(answer.get("noul"), 0, 1):
                raise ValueError("Invalid Noul")
            continue
        keys = (set(question["criteria"]) if kind == "choice" else
                {str(i) for i in range(len(question["criteria"]))})
        probs = answer.get("probabilities")
        if not isinstance(probs, dict) or set(probs) != keys:
            raise ValueError("Probability keys mismatch")
        if not all(finite_number(p, 0, 1) for p in probs.values()):
            raise ValueError("Invalid probability")
        # Provider outputs round probabilities to two decimals; preserve raw
        # values and allow only the accumulated rounding error, capped at .05.
        tolerance = min(0.05, len(keys) * 0.005 + 1e-9)
        if abs(sum(probs.values()) - 1) > tolerance:
            raise ValueError("Unnormalized probabilities")
        if not finite_number(answer.get("confidence"), 0, 1):
            raise ValueError("Invalid confidence")
        if kind == "choice":
            if answer.get("choice") not in keys:
                raise ValueError("Unknown choice")
            if probs[answer["choice"]] + 0.011 < max(probs.values()):
                raise ValueError("Choice contradicts distribution")
        else:
            if not finite_number(answer.get("score"), 0, len(keys) - 1):
                raise ValueError("Invalid score")
            if not isinstance(answer.get("legend"), dict) or set(answer["legend"]) != keys:
                raise ValueError("Invalid score legend")


def verdict(response, payload, proposed_route):
    if payload["state"]["observed_route"] != proposed_route:
        raise ValueError("Candidate identity mismatch")
    validate_response(response, payload)
    a = response["answers"]
    choice = a["assessment"]
    support = a["route_supported"]["noul"]
    score = a["evidence_fit"]["score"]
    ranked = sorted(choice["probabilities"].values(), reverse=True)
    margin = ranked[0] - ranked[1]
    decisive = choice["confidence"] >= POLICY["confidence"] and margin >= POLICY["margin"]
    if (decisive and choice["choice"] == "supported"
            and support >= POLICY["support"] and score >= POLICY["score"]):
        return {"verdict": "accept", "reason": "confident support with sufficient evidence"}
    if (decisive and choice["choice"] == "unsupported"
            and support <= 1 - POLICY["support"] and score <= 1):
        return {"verdict": "reject", "reason": "confident lack of support with weak evidence"}
    return {"verdict": "review", "reason": "uncertain, incomplete or conflicting judgments"}


def reference_verdict(proposed, expected):
    """Gold comparison is authoritative and independent of semantic judgments."""
    if expected is None:
        return "unlabeled"
    return "pass" if proposed == expected else "fail"
