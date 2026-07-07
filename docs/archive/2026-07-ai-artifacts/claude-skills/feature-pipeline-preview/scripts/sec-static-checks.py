# ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.
# Superseded by codemap/ + root AGENTS.md. Deleted once pipeline verified (Phase 6).
# Manifest: docs/archive/2026-07-ai-artifacts/MANIFEST.md
#!/usr/bin/env python3
"""
Static security scan over Haskell files: emits machine-checkable findings
for the patterns that DO NOT need human judgement. Anything subtler is left
to the deep-audit step.

Usage:
  sec-static-checks.py <path> [<path> ...]
  sec-static-checks.py --paths-from <file>

Output: JSON array of findings to stdout. Each finding:
  { "id": "sec-static-<NNN>",
    "file": "...", "line": N, "severity": "high|medium|informational",
    "rule": "rule-name", "snippet": "...", "message": "..." }
Exit code 0 even when findings exist (CI gating happens in the grounding step).

Rules (regex on a per-line basis, anchored to whitespace boundaries):
  R1  deriving (Show)        on a type whose name matches /Secret|Token|Key|Password|Credential/i
  R2  Text.pack of a literal containing "password" / "secret" / "token" / "api_key"
  R3  System.Random           imported in a hot module
  R4  unsafePerformIO          without an adjacent NOINLINE pragma
  R5  unsafeCoerce             without a safety comment
  R6  ==                      used between two values whose names match /token|hmac|signature|mac/i
  R7  Aeson.toJSON / Aeson.encode for a type whose name matches the secret regex
  R8  curl|sh | bash <(curl   in any *.sh / *.nix / *.cabal
  R9  String                  appearing in a hot-path module name
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys

SECRET_TYPE_RE = re.compile(r"\b(?:[A-Z][A-Za-z0-9]*)?(Secret|Token|Key|Password|Credential)\b")
SECRET_LITERAL_RE = re.compile(r"\"[^\"]*(password|secret|token|api[_-]?key)[^\"]*\"", re.I)
TOKEN_NAME_RE = re.compile(r"\b\w*(token|hmac|signature|mac)\w*\b", re.I)


RULES = [
    {
        "id": "R1",
        "rule": "deriving-show-on-secret-type",
        "severity": "high",
        "pattern": re.compile(r"deriving\s*\(?\s*[^)]*\bShow\b"),
        "predicate_line_window": 3,  # look up to 3 lines back for the data decl
        "message": "`deriving (Show)` on a type whose name contains Secret/Token/Key/Password/Credential will print the secret. Use a hand-written Show that prints `<redacted>`.",
    },
    {
        "id": "R2",
        "rule": "secret-literal-in-source",
        "severity": "high",
        "pattern": SECRET_LITERAL_RE,
        "predicate_line_window": 0,
        "message": "String literal contains a secret-shaped word. Move the value to env/secret manager.",
    },
    {
        "id": "R3",
        "rule": "system-random-in-source",
        "severity": "medium",
        "pattern": re.compile(r"\bimport\s+(qualified\s+)?System\.Random\b"),
        "predicate_line_window": 0,
        "message": "System.Random is not a CSPRNG. For security-sensitive randomness use Crypto.Random.getRandomBytes.",
    },
    {
        "id": "R4",
        "rule": "unsafePerformIO-without-noinline",
        "severity": "high",
        "pattern": re.compile(r"\bunsafePerformIO\b"),
        "predicate_line_window": 5,  # check 5 lines before for NOINLINE
        "message": "unsafePerformIO without a NOINLINE pragma can be inlined and lose ordering guarantees.",
    },
    {
        "id": "R5",
        "rule": "unsafeCoerce-without-comment",
        "severity": "high",
        "pattern": re.compile(r"\bunsafeCoerce\b"),
        "predicate_line_window": 3,
        "message": "unsafeCoerce without a safety comment proving representation compatibility.",
    },
    {
        "id": "R6",
        "rule": "non-constant-time-token-compare",
        "severity": "high",
        "pattern": re.compile(r"==\s*\w+"),
        "predicate_line_window": 0,
        "message": "Possible non-constant-time comparison on a token/HMAC/signature value. Use constEq.",
    },
    {
        "id": "R7",
        "rule": "json-instance-on-secret-type",
        "severity": "high",
        "pattern": re.compile(r"deriving\s*\(?\s*[^)]*\b(ToJSON|FromJSON|Generic)\b"),
        "predicate_line_window": 3,
        "message": "JSON / Generic derivation on a secret-shaped type will serialise the secret. Hand-write the instance or omit it.",
    },
    {
        "id": "R8",
        "rule": "curl-pipe-shell",
        "severity": "high",
        "pattern": re.compile(r"curl[^|]*\|\s*(sh|bash)|bash\s*<\(\s*curl"),
        "predicate_line_window": 0,
        "message": "Untrusted shell-pipe install — pin the upstream by hash and verify before executing.",
    },
    {
        "id": "R9",
        "rule": "string-in-hot-module",
        "severity": "informational",
        "pattern": re.compile(r"::\s*String\b"),
        "predicate_line_window": 0,
        "message": "`String` in source — nhcore uses `Text`. Confirm this module is not on a hot path.",
    },
]


def line_matches_secret_context(lines: list[str], idx: int, window: int) -> bool:
    """For rules that need to see a nearby `data <SecretName>` or NOINLINE.

    Rules R1/R7 require a recent `data <SecretName>` decl; R4 requires a
    NOINLINE pragma in the window; R5 requires a comment `-- safe ...`.
    """
    start = max(0, idx - window)
    window_text = "\n".join(lines[start:idx + 1])
    return bool(SECRET_TYPE_RE.search(window_text))


def has_noinline_pragma(lines: list[str], idx: int, window: int) -> bool:
    start = max(0, idx - window)
    return any("{-# NOINLINE" in l for l in lines[start:idx + 1])


def has_safety_comment(lines: list[str], idx: int, window: int) -> bool:
    start = max(0, idx - window)
    return any(re.search(r"--\s*safe\b", l, re.I) for l in lines[start:idx + 1])


def line_uses_token_name(line: str) -> bool:
    """For R6 — only flag `==` when at least one side mentions a token-ish name."""
    return bool(TOKEN_NAME_RE.search(line))


def scan_file(path: str) -> list[dict]:
    """Apply every rule to every line. Returns finding dicts (without ids)."""
    findings: list[dict] = []
    try:
        with open(path, encoding="utf-8", errors="replace") as f:
            lines = f.readlines()
    except OSError as exc:
        return [{
            "file": path, "line": 0, "severity": "informational",
            "rule": "file-unreadable", "snippet": "",
            "message": f"Could not read: {exc}",
        }]

    for idx, line in enumerate(lines):
        for rule in RULES:
            if not rule["pattern"].search(line):
                continue

            # Rule-specific predicate filters cut false positives.
            if rule["id"] in {"R1", "R7"}:
                if not line_matches_secret_context(
                    lines, idx, rule["predicate_line_window"]
                ):
                    continue
            elif rule["id"] == "R4":
                if has_noinline_pragma(lines, idx, rule["predicate_line_window"]):
                    continue
            elif rule["id"] == "R5":
                if has_safety_comment(lines, idx, rule["predicate_line_window"]):
                    continue
            elif rule["id"] == "R6":
                if not line_uses_token_name(line):
                    continue

            findings.append({
                "file": path,
                "line": idx + 1,
                "severity": rule["severity"],
                "rule": rule["rule"],
                "snippet": line.rstrip("\n"),
                "message": rule["message"],
            })
    return findings


def gather_paths(args: argparse.Namespace) -> list[str]:
    paths: list[str] = list(args.paths)
    if args.paths_from:
        with open(args.paths_from) as f:
            paths.extend(p.strip() for p in f if p.strip())
    # Only scan files that actually exist and look like sources we want.
    # Deduplicate so a path that arrives via both --paths-from and a CLI
    # argument is scanned once; otherwise downstream finding IDs collide.
    out: list[str] = []
    seen: set[str] = set()
    for p in paths:
        if not os.path.isfile(p):
            continue
        if p in seen:
            continue
        if p.endswith((".hs", ".lhs", ".sh", ".nix", ".cabal")):
            seen.add(p)
            out.append(p)
    return out


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="*")
    parser.add_argument("--paths-from", help="Read newline-delimited paths from a file.")
    args = parser.parse_args()

    paths = gather_paths(args)
    findings: list[dict] = []
    for p in paths:
        findings.extend(scan_file(p))

    # Assign stable IDs once the full list is known.
    for i, f in enumerate(findings, 1):
        f["id"] = f"sec-static-{i:03d}"

    json.dump(findings, sys.stdout, indent=2)
    sys.stdout.write("\n")


if __name__ == "__main__":
    main()
