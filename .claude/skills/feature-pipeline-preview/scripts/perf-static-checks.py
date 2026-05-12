#!/usr/bin/env python3
"""
Static performance scan over Haskell files: emits machine-checkable
findings for the patterns reviewers should flag without judgement. The
deep-audit step (a sonnet leaf) handles everything subtler.

Usage:
  perf-static-checks.py <path> [<path> ...]
  perf-static-checks.py --paths-from <file>

Output: JSON array of findings to stdout (same shape as sec-static-checks.py).
Exit 0 regardless of findings — gating happens in the grounding step.

Rules (regex on a per-line basis):
  R1  toJSON-only instance     no `toEncoding =` within 5 lines of `toJSON =`
  R2  String type annotation   `:: String`
  R3  pack . unpack chain      `Text.pack` and `Text.unpack` in same line
  R4  encodeUtf8 . decodeUtf8  same line round-trip
  R5  [fmt|...|] in a loop     `[fmt|` inside a line ending with `where|do|let|case`
                               (heuristic: hard to nail without a parser)
  R6  Array.map . Array.map    two consecutive `Array.map`
  R7  System.Random in module  imported for non-test code (cf. sec but flagged perf-side too)
  R8  ! on a field             `! ` after a record-field name (`Strict` makes this redundant)
  R9  ~ on a field             tilde laziness in a hot-path module without justifying comment
  R10 TVar (Map / HashMap)     contention footgun
  R11 INLINE on a long function approximate: `INLINE` pragma immediately followed
                                by a function whose body is > 25 source lines
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys

RULES = [
    {
        "id": "P1",
        "rule": "tojson-without-toencoding",
        "severity": "medium",
        "pattern": re.compile(r"^\s*toJSON\s*="),
        "message": "ToJSON instance defines `toJSON` but check that `toEncoding` is also defined for hot-path codecs.",
    },
    {
        "id": "P2",
        "rule": "string-type-annotation",
        "severity": "informational",
        "pattern": re.compile(r"::\s*\[?Char\]?|::\s*String\b"),
        "message": "`String` in source — nhcore exports `Text`. Confirm this is not a hot path.",
    },
    {
        "id": "P3",
        "rule": "pack-unpack-roundtrip",
        "severity": "medium",
        "pattern": re.compile(r"Text\.pack[\s\S]*?Text\.unpack|Text\.unpack[\s\S]*?Text\.pack"),
        "message": "`Text.pack`/`Text.unpack` round-trip on one line is O(n) allocation — eliminate the round-trip.",
    },
    {
        "id": "P4",
        "rule": "utf8-roundtrip",
        "severity": "medium",
        "pattern": re.compile(r"encodeUtf8[\s\S]*?decodeUtf8|decodeUtf8[\s\S]*?encodeUtf8"),
        "message": "UTF-8 encode/decode round-trip — keep one representation through the hot path.",
    },
    {
        "id": "P5",
        "rule": "fmt-in-recursive-context",
        "severity": "informational",
        "pattern": re.compile(r"\[fmt\|"),
        "message": "`[fmt|...|]` allocates a builder per invocation. Confirm this is not inside a tight loop.",
    },
    {
        "id": "P6",
        "rule": "double-map",
        "severity": "medium",
        "pattern": re.compile(r"Array\.map[^|]+\|>\s*Array\.map"),
        "message": "`Array.map ... |> Array.map ...` allocates an intermediate. Fuse to a single map.",
    },
    {
        "id": "P7",
        "rule": "system-random-perf",
        "severity": "informational",
        "pattern": re.compile(r"\bimport\s+(qualified\s+)?System\.Random\b"),
        "message": "System.Random imported — security side already flags this; perf side notes splitmix is fine for jitter.",
    },
    {
        "id": "P8",
        "rule": "redundant-bang-field",
        "severity": "informational",
        "pattern": re.compile(r"^\s*,?\s*\w+\s*::\s*!"),
        "message": "`!` on a field is redundant under the global `Strict` extension.",
    },
    {
        "id": "P9",
        "rule": "tilde-on-field",
        "severity": "medium",
        "pattern": re.compile(r"^\s*,?\s*\w+\s*::\s*~"),
        "message": "`~` opts out of strictness. Confirm this is intentional and justified by a comment.",
    },
    {
        "id": "P10",
        "rule": "tvar-map-contention",
        "severity": "medium",
        "pattern": re.compile(r"TVar\s*\(\s*(Map|HashMap|Set)\b"),
        "message": "`TVar (Map/HashMap/Set)` serialises all writers. Consider `stm-containers` if multi-writer.",
    },
    {
        "id": "P11",
        "rule": "inline-pragma-emitted",
        "severity": "informational",
        "pattern": re.compile(r"\{-#\s*INLINE\b"),
        "message": "INLINE pragma — verify the target function is small (< ~10 lines) and on a hot path.",
    },
]


def gather_paths(args: argparse.Namespace) -> list[str]:
    paths: list[str] = list(args.paths)
    if args.paths_from:
        with open(args.paths_from) as f:
            paths.extend(p.strip() for p in f if p.strip())
    return [p for p in paths if os.path.isfile(p) and p.endswith((".hs", ".lhs"))]


def scan_file(path: str) -> list[dict]:
    try:
        with open(path, encoding="utf-8", errors="replace") as f:
            content = f.read()
    except OSError as exc:
        return [{
            "file": path, "line": 0, "severity": "informational",
            "rule": "file-unreadable", "snippet": "",
            "message": f"Could not read: {exc}",
        }]
    lines = content.splitlines()
    findings: list[dict] = []

    # Single-line rules.
    single_line = [r for r in RULES if r["id"] not in {"P1"}]
    for idx, line in enumerate(lines):
        for rule in single_line:
            if rule["pattern"].search(line):
                findings.append({
                    "file": path,
                    "line": idx + 1,
                    "severity": rule["severity"],
                    "rule": rule["rule"],
                    "snippet": line.rstrip(),
                    "message": rule["message"],
                })

    # P1: toJSON defined but no toEncoding within 5 lines.
    for idx, line in enumerate(lines):
        if not re.search(r"^\s*toJSON\s*=", line):
            continue
        window = "\n".join(lines[idx: min(len(lines), idx + 6)])
        if "toEncoding" not in window:
            findings.append({
                "file": path,
                "line": idx + 1,
                "severity": "medium",
                "rule": "tojson-without-toencoding",
                "snippet": line.rstrip(),
                "message": "`toJSON` defined without `toEncoding`. Add `toEncoding` for hot-path codecs (>2x faster).",
            })

    return findings


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="*")
    parser.add_argument("--paths-from", help="Read newline-delimited paths from a file.")
    args = parser.parse_args()

    paths = gather_paths(args)
    findings: list[dict] = []
    for p in paths:
        findings.extend(scan_file(p))

    for i, f in enumerate(findings, 1):
        f["id"] = f"perf-static-{i:03d}"

    json.dump(findings, sys.stdout, indent=2)
    sys.stdout.write("\n")


if __name__ == "__main__":
    main()
