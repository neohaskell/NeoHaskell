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

Payload arrives as stdin JSON ({tool_name, tool_input}) per the official hook
contract; CLAUDE_TOOL_* env vars are read as a fallback for older runtimes.
Exit 2 = rejection; stderr is fed back to the model (the teaching moment).

THE LAW/TEACHER SPLIT: this hook is the fast teacher, .hlint.yaml is the law.
Each rule below names its canonical gate — on disagreement the gate wins and
this file gets corrected (that's what HOOK-ALLOW is for).
"""

import json
import os
import re
import sys

RULES = [
    # canonical gate: .hlint.yaml "NeoHaskell: use |> pipelines" rewrite hint
    (re.compile(r"(^|[^$])\$\s|\s\$$"),
     "`$` is banned — use `x |> f` pipelines. (Style table, AGENTS.md)"),
    # NOT expressible in hlint (declaration syntax); canonical gate: review
    (re.compile(r"\bwhere\b"),
     "`where` clauses are banned — use `do let`. (module/class/instance/data headers are fine)"),
    # NOT expressible in hlint (type name); backstop: Data.Either module restriction
    (re.compile(r"\bEither\b"),
     "`Either` is banned — use `Result err val`."),
    # canonical gate: .hlint.yaml modules: restrictions (grandfathered within:)
    (re.compile(r"^\s*import\s+(qualified\s+)?(Data|GHC\.Base|System|Control\.Monad)\b"),
     "Vanilla import — use the Core wrapper (Text/Array/Map/Char/File/...). "
     "No wrapper exists? Register an exception in .hlint.yaml `within:` with a "
     "justification + `belongs-in:` note (rule of three promotes a primitive)."),
    # NOT expressible in hlint generically; canonical gate: review
    (re.compile(r"^\s*import\s+[A-Z][A-Za-z0-9.]*\s*$"),
     "Unqualified open import — use `import Foo (Foo); import Foo qualified`."),
    # NOT in hlint (usage-vs-definition undecidable there); backstop: GHC/review.
    # Negative lookahead exempts definitions: `pure = ...` AND `pure x = ...`
    # (but not `==` comparisons).
    (re.compile(r"\b(pure|return)\b(?!\s*\w*\s*=(?!=))"),
     "`pure`/`return` are banned in NeoHaskell code — use `Task.yield`. "
     "(Defining them in instances is fine: `pure = ...` / `pure x = ...`.)"),
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


def read_event():
    """Official contract: JSON on stdin ({tool_name, tool_input}).
    Fallback: CLAUDE_TOOL_* env vars (older runtimes / manual testing)."""
    tool, payload = "", {}
    if not sys.stdin.isatty():
        try:
            event = json.loads(sys.stdin.read() or "{}")
            tool = event.get("tool_name", "")
            payload = event.get("tool_input", {}) or {}
        except json.JSONDecodeError:
            pass
    if not tool:
        tool = os.environ.get("CLAUDE_TOOL_NAME", "")
        try:
            payload = json.loads(os.environ.get("CLAUDE_TOOL_INPUT", "") or "{}")
        except json.JSONDecodeError:
            payload = {}
    return tool, payload


def main() -> int:
    tool, payload = read_event()

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
        print(msg, file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
