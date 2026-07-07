#!/usr/bin/env python3
"""PreToolUse dialect guard (pipeline Phase 2).

Rejects vanilla-Haskell patterns in Write/Edit/MultiEdit payloads targeting
*.hs files, ~50ms after the edit is attempted — long before hlint (seconds)
or GHC (the build). The teaching layer: every rejection quotes the rule and
the escape hatch.

Precision rules:
- Only ADDED lines are checked (Edit: lines in new_string not in old_string),
  so editing near existing grandfathered code never trips the guard.
- Comment-only lines are skipped ("returns the result" prose is fine).
- `module|class|instance|data|newtype ... where` is legal Haskell structure,
  not a where-clause.
- A line containing HOOK-ALLOW is skipped (escape hatch for false positives;
  hlint and GHC remain the real gates).

Exit 2 + {"block": true} on stderr = rejection (Claude Code hook contract).
"""

import json
import os
import re
import sys

RULES = [
    (re.compile(r"(^|[^$])\$\s|\s\$$"),
     "`$` is banned — use `x |> f` pipelines. (Style table, AGENTS.md)"),
    (re.compile(r"\bwhere\b"),
     "`where` clauses are banned — use `do let`. (module/class/instance/data headers are fine)"),
    (re.compile(r"\bEither\b"),
     "`Either` is banned — use `Result err val`."),
    (re.compile(r"^\s*import\s+(qualified\s+)?(Data|GHC\.Base|System|Control\.Monad)\b"),
     "Vanilla import — use the Core wrapper (Text/Array/Map/Char/File/...). "
     "No wrapper exists? Register an exception in .hlint.yaml `within:` with a "
     "justification + `belongs-in:` note (rule of three promotes a primitive)."),
    (re.compile(r"^\s*import\s+[A-Z][A-Za-z0-9.]*\s*$"),
     "Unqualified open import — use `import Foo (Foo); import Foo qualified`."),
    (re.compile(r"\b(pure|return)\b(?!\s*=)"),
     "`pure`/`return` are banned in NeoHaskell code — use `Task.yield`. "
     "(Defining them in instances is fine: `pure = ...`.)"),
]

CASE_BOOL = re.compile(r"case[^\n]*\bof\b[^\n]*\n\s*(True|False)\s*->")
STRUCTURE_WHERE = re.compile(r"^\s*(module|class|instance|data|newtype)\b|deriving")
COMMENT_LINE = re.compile(r"^\s*--")


def added_lines(tool: str, payload: dict):
    if tool == "Write":
        return payload.get("content", "").splitlines()
    if tool == "Edit":
        old = set(payload.get("old_string", "").splitlines())
        return [l for l in payload.get("new_string", "").splitlines() if l not in old]
    if tool == "MultiEdit":
        out = []
        for e in payload.get("edits", []):
            old = set(e.get("old_string", "").splitlines())
            out += [l for l in e.get("new_string", "").splitlines() if l not in old]
        return out
    return []


def new_blobs(tool: str, payload: dict):
    if tool == "Write":
        return [payload.get("content", "")]
    if tool == "Edit":
        return [payload.get("new_string", "")]
    if tool == "MultiEdit":
        return [e.get("new_string", "") for e in payload.get("edits", [])]
    return []


def main() -> int:
    tool = os.environ.get("CLAUDE_TOOL_NAME", "")
    raw = os.environ.get("CLAUDE_TOOL_INPUT", "")
    try:
        payload = json.loads(raw) if raw else {}
    except json.JSONDecodeError:
        return 0  # malformed input is not this guard's problem

    path = payload.get("file_path", "")
    if not path.endswith(".hs") or "docs/archive/" in path:
        return 0

    violations = []
    for line in added_lines(tool, payload):
        if COMMENT_LINE.match(line) or "HOOK-ALLOW" in line:
            continue
        for rx, msg in RULES:
            if rx.search(line):
                if "where" in msg and STRUCTURE_WHERE.search(line):
                    continue
                if "Either" in msg and path.endswith("core/core/Result.hs"):
                    continue
                violations.append(f"  {msg}\n    ↳ {line.strip()[:100]}")
                break  # one rule per line is enough

    for blob in new_blobs(tool, payload):
        if "HOOK-ALLOW" in blob:
            continue
        if CASE_BOOL.search(blob):
            violations.append("  `case cond of True/False` is an anti-pattern — use if-then-else.")
            break

    if violations:
        seen = set()
        unique = [v for v in violations if not (v in seen or seen.add(v))][:6]
        msg = (
            "NeoHaskell dialect guard — edit rejected:\n"
            + "\n".join(unique)
            + "\n\nFix the pattern, or if this is a false positive/deliberate exception, "
            "add `-- HOOK-ALLOW: <reason>` on the line (hlint + GHC remain the real gates)."
        )
        print(json.dumps({"block": True, "message": msg}), file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
