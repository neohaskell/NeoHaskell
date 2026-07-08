#!/usr/bin/env python3
"""PreToolUse expectation guard (pipeline Phase 5, task 6).

Mechanizes the standing rule "never modify existing test expectations without
maintainer approval" (AGENTS.md, Non-negotiable): an Edit/Write/MultiEdit that
REMOVES or CHANGES an existing expectation line in a test file is blocked
unless the run carries an explicit approval marker.

What counts:
- test files: *Spec.hs, files under test dirs (core/test*, testbed/test,
  integrations/**/tests), and .hurl acceptance files
- expectation lines: hspec matchers (shouldBe & friends) and hurl assertions
- a violation is an expectation line present in the OLD text and absent from
  the NEW text (whitespace-normalized). Moving a line or adding new
  expectations never trips the guard; deleting or rewording one does.

Approval marker: `.pipeline/allow-expectation-edits` — a file the MAINTAINER
creates (or explicitly tells the agent to create), naming who approved and
why. It is inside gitignored `.pipeline/`, so approval is per-checkout and
per-run, never committed, never ambient. Delete it when the approved change
lands. There is deliberately NO inline escape hatch (HOOK-ALLOW) — changing
recorded expectations is exactly the action that must go through a human.

Contract mirrors dialect-guard: stdin JSON payload, exit 2 = rejection,
--self-test enforces case coverage from expectation-guard-cases.json
(runs in CI via ./dev doctor).
"""

import json
import os
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
APPROVAL_FILE = REPO_ROOT / ".pipeline" / "allow-expectation-edits"
CASES_FILE = Path(__file__).parent / "expectation-guard-cases.json"

TEST_FILE = re.compile(r"(Spec\.hs$|\.hurl$|(^|/)(core/test[^/]*|testbed/test|tests)/)")
EXPECTATION = re.compile(
    r"\bshould(Be|Return|Satisfy|MatchList|Contain|StartWith|EndWith|Throw|NotBe)\b"
    r"|\bexpectationFailure\b"
    r"|^HTTP\b|^\[Asserts\]|^jsonpath\s|^status\s|^header\s|^body\s")


def norm(line: str) -> str:
    return " ".join(line.split())


def expectation_lines(text: str):
    """Multiset of normalized expectation lines (count matters: rewriting one
    of two identical assertions is still a change)."""
    out = {}
    for line in text.splitlines():
        if EXPECTATION.search(line):
            key = norm(line)
            out[key] = out.get(key, 0) + 1
    return out


def removed_expectations(old: str, new: str):
    before, after = expectation_lines(old), expectation_lines(new)
    return sorted(k for k, n in before.items() if after.get(k, 0) < n)


def old_new_pairs(tool: str, payload: dict, read_disk):
    if tool == "Edit":
        return [(payload.get("old_string", ""), payload.get("new_string", ""))]
    if tool == "MultiEdit":
        return [(e.get("old_string", ""), e.get("new_string", ""))
                for e in payload.get("edits", [])]
    if tool == "Write":
        # overwrite: the old text is the file's current content
        return [(read_disk(payload.get("file_path", "")), payload.get("content", ""))]
    return []


def check(tool: str, payload: dict, approved: bool, read_disk=None):
    """Returns [removed expectation lines] — pure, testable."""
    path = payload.get("file_path", "")
    if not TEST_FILE.search(path) or "docs/archive/" in path:
        return []
    if approved:
        return []
    if read_disk is None:
        def read_disk(p):
            try:
                return Path(p).read_text()
            except OSError:
                return ""
    removed = []
    for old, new in old_new_pairs(tool, payload, read_disk):
        removed += removed_expectations(old, new)
    return removed


def read_event():
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
    """Every case runs through check() with approved=False unless the case
    sets `approved`; coverage: >=1 blocking and >=1 passing case per
    scenario tag."""
    cases = json.loads(CASES_FILE.read_text())
    tags_blocked, tags_passed, fails = set(), set(), 0
    for case in cases:
        removed = check(case["tool"], case["tool_input"],
                        case.get("approved", False),
                        read_disk=lambda _p, c=case: c.get("disk_content", ""))
        want_block = case["expect_block"]
        if bool(removed) != want_block:
            print(f"self-test FAIL {case['name']}: expected "
                  f"{'block' if want_block else 'pass'}, removed={removed}")
            fails += 1
        (tags_blocked if want_block else tags_passed).add(case["tag"])
    for tag in sorted(tags_blocked - tags_passed):
        print(f"self-test FAIL coverage: tag '{tag}' has no passing counter-case")
        fails += 1
    if fails == 0:
        print(f"expectation-guard self-test: OK — {len(cases)} cases, "
              f"{len(tags_blocked | tags_passed)} scenarios covered")
    return 1 if fails else 0


def main() -> int:
    if len(sys.argv) > 1 and sys.argv[1] == "--self-test":
        return self_test()

    tool, payload = read_event()
    removed = check(tool, payload, approved=APPROVAL_FILE.exists())
    if removed:
        print(
            "Expectation guard — edit rejected: this change removes or rewrites "
            "existing test expectations:\n"
            + "\n".join(f"  ↳ {line[:120]}" for line in removed[:6])
            + "\n\nExisting expectations are the recorded contract — changing them "
            "requires MAINTAINER approval (AGENTS.md, Non-negotiable). If the "
            "maintainer has approved this in writing, record it:\n"
            "  echo 'approved by <name>: <reason / spec ref>' > "
            ".pipeline/allow-expectation-edits\n"
            "then retry, and delete the marker when the change lands. "
            "Adding NEW tests never needs this.",
            file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
