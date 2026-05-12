#!/usr/bin/env python3
"""
Auto-classify a NeoHaskell feature into one of the five complexity tiers.

The tier drives the grounding loop in security/perf reviews: a `trivial`
feature must not accumulate hot-path or crypto findings; a
`security-critical` feature must not skip them.

Input: a JSON document on stdin OR via --input <path> with keys:
  - feature_name  (str)         human-readable feature name
  - issue_text    (str)         issue body / description (may be empty)
  - module_path   (str)         where the new module lives
  - touches       (list[str])   path globs the feature changes
  - adds_dep      (bool)        does it add a new dependency?
  - external_io   (bool)        does it perform network/file/process IO?
  - touches_secrets (bool)      does it produce/compare/store a secret?
  - touches_auth  (bool)        does it touch auth or multi-tenant authz?

Output: prints JSON {"tier": "...", "rationale": "...", "signals": {...}}.
Exit codes: 0 success, 1 input error.

The classifier uses a small ordered-rule cascade: the FIRST rule that
fires wins. Rules go from most-restrictive (security-critical) down to
least (trivial).
"""

from __future__ import annotations

import argparse
import json
import re
import sys

VALID_TIERS = ("trivial", "simple", "moderate", "complex", "security-critical")

# Keywords that, when present in feature_name + issue_text, raise the tier.
SECURITY_CRITICAL_KEYWORDS = [
    "auth", "oauth", "authn", "authz", "authoriz",
    "hmac", "jwt", "token", "session",
    "encrypt", "decrypt", "crypto",
    "signing", "signature", "verify",
    "secret", "credential", "api key", "password",
    "csrf", "xss", "injection",
    "mtls", "tls cert", "x509",
]

COMPLEX_KEYWORDS = [
    "event store", "eventstore", "projection",
    "schema migration", "migration",
    "replication", "consensus",
    "network protocol", "rpc",
    "concurrent", "stm-containers", "sharded",
    "queue", "channel",
]

MODERATE_KEYWORDS = [
    "query", "command", "decider",
    "integration", "adapter", "driver",
    "serializer", "encoder", "decoder",
    "http", "rest", "graphql",
    "database", "postgres", "sql",
]

TRIVIAL_KEYWORDS = [
    "mock", "stub", "fixture",
    "rename", "typo", "doc",
    "readme", "comment",
    "fmt", "format",
]


def haystack(payload: dict) -> str:
    """Lower-case concatenation of every text field we classify on."""
    parts = [
        payload.get("feature_name", "") or "",
        payload.get("issue_text", "") or "",
        payload.get("module_path", "") or "",
    ]
    return " ".join(parts).lower()


def any_kw(hay: str, kws: list[str]) -> tuple[bool, list[str]]:
    """Return (matched?, list-of-matched-keywords)."""
    matches = [kw for kw in kws if kw in hay]
    return (bool(matches), matches)


def classify(payload: dict) -> dict:
    """Apply the ordered-rule cascade and return tier + rationale + signals."""
    hay = haystack(payload)
    sec_hit, sec_kws = any_kw(hay, SECURITY_CRITICAL_KEYWORDS)
    complex_hit, complex_kws = any_kw(hay, COMPLEX_KEYWORDS)
    moderate_hit, moderate_kws = any_kw(hay, MODERATE_KEYWORDS)
    trivial_hit, trivial_kws = any_kw(hay, TRIVIAL_KEYWORDS)

    touches_secrets = bool(payload.get("touches_secrets"))
    touches_auth = bool(payload.get("touches_auth"))
    external_io = bool(payload.get("external_io"))
    adds_dep = bool(payload.get("adds_dep"))
    touches = payload.get("touches") or []

    signals = {
        "security_keywords": sec_kws,
        "complex_keywords": complex_kws,
        "moderate_keywords": moderate_kws,
        "trivial_keywords": trivial_kws,
        "touches_secrets": touches_secrets,
        "touches_auth": touches_auth,
        "external_io": external_io,
        "adds_dep": adds_dep,
        "file_count": len(touches),
    }

    # Rule 1: explicit security/auth boundary or matching keywords ⇒ critical.
    if touches_secrets or touches_auth or sec_hit:
        why = []
        if touches_secrets: why.append("produces or compares a secret value")
        if touches_auth: why.append("touches auth or multi-tenant authorization")
        if sec_hit: why.append(f"security keywords matched: {', '.join(sec_kws)}")
        return {
            "tier": "security-critical",
            "rationale": "; ".join(why),
            "signals": signals,
        }

    # Rule 2: networked subsystem / event-store / migration ⇒ complex.
    if complex_hit or (external_io and adds_dep):
        why = []
        if complex_hit: why.append(f"complex keywords matched: {', '.join(complex_kws)}")
        if external_io and adds_dep: why.append("introduces external IO and a new dependency")
        return {"tier": "complex", "rationale": "; ".join(why), "signals": signals}

    # Rule 3: explicit trivial markers (mock, stub, fixture, rename, doc, ...)
    # WIN over moderate-vocabulary matches. A "mock HTTP integration" is
    # trivial even though it contains the word "integration".
    if trivial_hit and not external_io and not adds_dep:
        return {
            "tier": "trivial",
            "rationale": f"trivial keywords matched: {', '.join(trivial_kws)}",
            "signals": signals,
        }

    # Rule 4: query/command/integration vocabulary ⇒ moderate.
    if moderate_hit or external_io or adds_dep:
        why = []
        if moderate_hit: why.append(f"moderate keywords matched: {', '.join(moderate_kws)}")
        if external_io and not adds_dep: why.append("introduces external IO")
        if adds_dep and not external_io: why.append("adds a new dependency")
        return {"tier": "moderate", "rationale": "; ".join(why), "signals": signals}

    # Rule 5: tiny diff with no IO/dep ⇒ trivial fallback.
    if len(touches) <= 2 and not (external_io or adds_dep):
        return {
            "tier": "trivial",
            "rationale": "small diff with no external IO and no new dependency",
            "signals": signals,
        }

    # Rule 6: default ⇒ simple (new pure type / module).
    return {
        "tier": "simple",
        "rationale": "default — new module or pure type with no IO/dep/security signals",
        "signals": signals,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--input", help="Path to a JSON file (default: stdin).")
    args = parser.parse_args()

    raw = open(args.input).read() if args.input else sys.stdin.read()
    if not raw.strip():
        print("ERROR: empty input — pass a JSON document on stdin or via --input.",
              file=sys.stderr)
        sys.exit(1)
    try:
        payload = json.loads(raw)
    except json.JSONDecodeError as exc:
        print(f"ERROR: invalid JSON: {exc}", file=sys.stderr)
        sys.exit(1)
    if not isinstance(payload, dict):
        print("ERROR: input must be a JSON object.", file=sys.stderr)
        sys.exit(1)

    result = classify(payload)
    assert result["tier"] in VALID_TIERS  # internal invariant
    json.dump(result, sys.stdout, indent=2, sort_keys=True)
    sys.stdout.write("\n")


if __name__ == "__main__":
    main()
