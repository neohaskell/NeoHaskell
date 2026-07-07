#!/usr/bin/env python3
"""PreToolUse dialect guard (pipeline Phase 2).

Rejects vanilla-Haskell patterns in Write/Edit/MultiEdit payloads targeting
*.hs files, ~50ms after the edit is attempted — long before hlint (seconds)
or GHC (the build). The teaching layer: every rejection quotes the rule and
the escape hatch.

THE LAW/TEACHER SPLIT: this hook is the fast teacher, .hlint.yaml is the law.
Each rule names its canonical gate — on disagreement the gate wins and this
file gets corrected (that's what HOOK-ALLOW is for).

Precision rules:
- Only ADDED lines are checked (Edit: lines in new_string not in old_string),
  so editing near existing grandfathered code never trips the guard.
- Comment-only lines are skipped ("returns the result" prose is fine).
- `where` is banned only as a let-substitute (function-level where-clause);
  it is legal as part of module/class/instance/data/newtype/GADT/closed
  type family declarations.
- A line containing HOOK-ALLOW is skipped (escape hatch for false positives;
  hlint and GHC remain the real gates).

EXTENDING THIS HOOK — see the `neohaskell-dialect-rules` skill. Contract:
every rule has an `id`, and .claude/hooks/dialect-guard-cases.json MUST
contain at least one blocking case (`expect_rules` includes the id) and one
passing case (`pass_rules` includes the id). `--self-test` enforces this and
runs in CI via `./dev doctor`.

Payload: stdin JSON ({tool_name, tool_input}) per the official hook contract;
CLAUDE_TOOL_* env vars as fallback. Exit 2 = rejection; stderr is fed back
to the model.
"""

import json
import os
import re
import sys
from pathlib import Path

# (id, compiled regex, message). Comment above each rule = its canonical gate.
RULES = [
    # gate: .hlint.yaml "NeoHaskell: use |> pipelines" rewrite hint
    ("no-dollar",
     re.compile(r"(^|[^$])\$\s|\s\$$"),
     "`$` is banned — use `x |> f` pipelines. (Style table, AGENTS.md)"),
    # NOT expressible in hlint (declaration syntax); gate: review.
    # Exemption logic: STRUCTURE_WHERE below.
    ("no-where-clause",
     re.compile(r"\bwhere\b"),
     "`where` is banned as a let-substitute — use `do let`. "
     "(where in module/class/instance/data/GADT/type-family declarations is fine)"),
    # NOT expressible in hlint (type name); backstop: Data.Either module restriction
    ("no-either",
     re.compile(r"\bEither\b"),
     "`Either` is banned — use `Result err val`."),
    # gate: .hlint.yaml modules: restrictions (grandfathered within:)
    ("no-vanilla-import",
     re.compile(r"^\s*import\s+(qualified\s+)?(Data|GHC\.Base|System|Control\.Monad)\b"),
     "Vanilla import — use the Core wrapper (Text/Array/Map/Char/File/...). "
     "No wrapper exists? Register an exception in .hlint.yaml `within:` with a "
     "justification + `belongs-in:` note (rule of three promotes a primitive)."),
    # NOT expressible in hlint generically; gate: review
    ("no-open-import",
     re.compile(r"^\s*import\s+[A-Z][A-Za-z0-9.]*\s*$"),
     "Unqualified open import — use `import Foo (Foo); import Foo qualified`."),
    # NOT in hlint (usage-vs-definition undecidable there); backstop: GHC/review.
    # Definition exemption lives in exempt() — covers point-free, plain-arg,
    # and pattern-matching instance methods (`pure (Just x) = …`, `pure [] = …`).
    ("no-pure-return",
     re.compile(r"\b(pure|return)\b"),
     "`pure`/`return` are banned in NeoHaskell code — use `Task.yield`. "
     "(Defining them in instances is fine: `pure = ...` / `pure x = ...`.)"),
]

# Instance-method definition shapes for pure/return: point-free, single var,
# constructor, parenthesized pattern, list pattern — followed by `=` (not `==`).
PURE_DEF = re.compile(r"^\s*(pure|return)\s*(\w+|\(.*?\)|\[\s*\])?\s*=(?!=)")

# Multi-line pseudo-rule (checked per blob, not per line).
CASE_BOOL_ID = "no-case-bool"
CASE_BOOL = re.compile(r"case[^\n]*\bof\b[^\n]*\n\s*(True|False)\s*->")

# Declaration heads where `where` is legal Haskell structure. Includes `type`
# (closed type families: `type family F a where`). Known limitation: a
# multi-line declaration head whose continuation line carries the `where`
# (e.g. constraints spanning lines) needs HOOK-ALLOW — documented in the skill.
STRUCTURE_WHERE = re.compile(r"^\s*(module|class|instance|data|newtype|type)\b|deriving")
COMMENT_LINE = re.compile(r"^\s*--")

CASES_FILE = Path(__file__).parent / "dialect-guard-cases.json"


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


def exempt(rule_id: str, line: str, path: str) -> bool:
    """Per-rule exemption logic — keyed by rule id, never by message text."""
    if rule_id == "no-where-clause" and STRUCTURE_WHERE.search(line):
        return True
    if rule_id == "no-either" and path.endswith("core/core/Result.hs"):
        return True
    if rule_id == "no-pure-return" and PURE_DEF.match(line):
        return True
    return False


def check(tool: str, payload: dict):
    """Returns [(rule_id, message, offending_line)] — pure, testable."""
    path = payload.get("file_path", "")
    if not path.endswith(".hs") or "docs/archive/" in path:
        return []

    violations = []
    for line in added_lines(tool, payload):
        if COMMENT_LINE.match(line) or "HOOK-ALLOW" in line:
            continue
        for rule_id, rx, msg in RULES:
            if rx.search(line) and not exempt(rule_id, line, path):
                violations.append((rule_id, msg, line.strip()[:100]))
                break  # one rule per line is enough

    for blob in new_blobs(tool, payload):
        for m in CASE_BOOL.finditer(blob):
            # Scope the escape hatch to the matched construct's own lines —
            # a stray HOOK-ALLOW elsewhere in the edit must not silence this.
            start = blob.rfind("\n", 0, m.start()) + 1
            end_nl = blob.find("\n", m.end())
            span = blob[start:(end_nl if end_nl != -1 else len(blob))]
            if "HOOK-ALLOW" not in span:
                violations.append((CASE_BOOL_ID,
                                   "`case cond of True/False` is an anti-pattern — use if-then-else.", ""))
                return violations
    return violations


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


def self_test() -> int:
    """Run the committed case file AND enforce rule coverage:
    every rule id needs >=1 blocking case (expect_rules) and >=1 passing
    case (pass_rules). Adding a rule without cases fails CI."""
    cases = json.loads(CASES_FILE.read_text())
    all_ids = {r[0] for r in RULES} | {CASE_BOOL_ID}
    blocked_cov, pass_cov, fails = set(), set(), 0

    for case in cases:
        fired = {v[0] for v in check(case["tool"], case["tool_input"])}
        expect = set(case.get("expect_rules", []))
        if expect:
            if not expect <= fired:
                print(f"self-test FAIL {case['name']}: expected {sorted(expect)}, fired {sorted(fired)}")
                fails += 1
            blocked_cov |= expect
        else:
            if fired:
                print(f"self-test FAIL {case['name']}: expected pass, fired {sorted(fired)}")
                fails += 1
            pass_cov |= set(case.get("pass_rules", []))

    for missing, kind in ((all_ids - blocked_cov, "blocking"), (all_ids - pass_cov, "passing")):
        for rule_id in sorted(missing):
            print(f"self-test FAIL coverage: rule '{rule_id}' has no {kind} case in {CASES_FILE.name}")
            fails += 1

    if fails == 0:
        print(f"dialect-guard self-test: OK — {len(cases)} cases, {len(all_ids)} rules fully covered")
    return 1 if fails else 0


def main() -> int:
    if len(sys.argv) > 1 and sys.argv[1] == "--self-test":
        return self_test()

    tool, payload = read_event()
    violations = check(tool, payload)

    if violations:
        seen = set()
        lines = []
        for rule_id, msg, offending in violations:
            key = (rule_id, offending)
            if key in seen:
                continue
            seen.add(key)
            lines.append(f"  [{rule_id}] {msg}" + (f"\n    ↳ {offending}" if offending else ""))
        msg = (
            "NeoHaskell dialect guard — edit rejected:\n"
            + "\n".join(lines[:6])
            + "\n\nFix the pattern, or if this is a false positive/deliberate exception, "
            "add `-- HOOK-ALLOW: <reason>` on the line (hlint + GHC remain the real gates)."
        )
        print(msg, file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
