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

Two layers, because a local hook alone is not a gate:
- This hook is the fast LOCAL teacher. Approval marker
  `.pipeline/allow-expectation-edits` — a per-checkout, gitignored file the
  MAINTAINER creates (or tells the agent to). No inline escape hatch. It fails
  LOUD-open on unparseable stdin (a harness schema change must be visible, not
  a silently-disabled guard).
- `--pr-diff BASE` is the ENFORCED CI backstop (checks.yml `expectations`
  job): it census-diffs the committed test files against the merge base and
  blocks a net-removed/reworded expectation unless a maintainer applied the
  `expectations-approved` PR label. Operating on the committed result (not an
  edit payload) covers the rename / non-test-path / Bash-mutation vectors the
  local hook's path+payload matcher cannot; the label, unlike the local
  marker, is not agent-writable.

Contract mirrors dialect-guard: stdin JSON payload, exit 2 = rejection,
--self-test enforces coverage (runs in CI via ./dev doctor).
"""

import json
import os
import re
import subprocess
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


# ── CI census gate (--pr-diff) ────────────────────────────────────────────────

def removed_from_census(before, after):
    """Pure: normalized expectation lines whose count dropped from before→after.
    Census-over-files (not per-file diff) nets moves to zero and catches
    reworded/deleted expectations regardless of which file or tool touched them."""
    return sorted(k for k, n in before.items() if after.get(k, 0) < n)


def _git(*args):
    return subprocess.run(["git", *args], capture_output=True, text=True)


def changed_test_files(base):
    # --no-renames so a rename surfaces as delete(old)+add(new): the census then
    # sees the old file's expectations leave and the new file's arrive.
    proc = _git("diff", "--name-only", "--no-renames", "--diff-filter=ACMD",
                f"{base}...HEAD")
    if proc.returncode != 0:
        print(f"expectation-guard: git diff against `{base}` failed — bad base ref? "
              f"({proc.stderr.strip()})", file=sys.stderr)
        sys.exit(2)
    return [p for p in proc.stdout.splitlines()
            if TEST_FILE.search(p) and "docs/archive/" not in p]


def pr_diff(base, approved):
    """CI backstop: fail on a net-removed/reworded expectation across the changed
    test files unless a maintainer approved it (the `expectations-approved`
    label). Returns an exit code."""
    before, after = {}, {}
    for path in changed_test_files(base):
        shown = _git("show", f"{base}:{path}")
        for k, n in expectation_lines(shown.stdout if shown.returncode == 0 else "").items():
            before[k] = before.get(k, 0) + n
        try:
            head_text = Path(path).read_text()
        except OSError:
            head_text = ""  # deleted at HEAD
        for k, n in expectation_lines(head_text).items():
            after[k] = after.get(k, 0) + n
    removed = removed_from_census(before, after)
    if not removed:
        print("expectation-guard: OK — no test expectations removed or reworded")
        return 0
    if approved:
        print(f"expectation-guard: {len(removed)} expectation change(s) allowed "
              "(maintainer-approved via the `expectations-approved` label)")
        return 0
    print("expectation-guard — PR blocked: removes or rewrites existing test "
          "expectations:\n" + "\n".join(f"  ↳ {k[:120]}" for k in removed[:12])
          + "\n\nExisting expectations are the recorded contract (AGENTS.md, "
          "Non-negotiable). A maintainer who approved this change must apply the "
          "`expectations-approved` label to the PR. Adding NEW tests never trips "
          "this.", file=sys.stderr)
    return 1


def parse_event(stdin_text, env):
    """Pure: (tool, payload, parse_error) from a stdin payload + env fallback.
    parse_error is True only when stdin held real content that did NOT parse as
    the expected JSON event — an empty/absent payload is not an error."""
    tool, payload, parse_error = "", {}, False
    stripped = (stdin_text or "").strip()
    if stripped and stripped != "{}":
        try:
            event = json.loads(stripped)
            tool = event.get("tool_name", "")
            payload = event.get("tool_input", {}) or {}
        except (json.JSONDecodeError, AttributeError):
            parse_error = True
    if not tool:
        tool = env.get("CLAUDE_TOOL_NAME", "")
        try:
            payload = json.loads(env.get("CLAUDE_TOOL_INPUT", "") or "{}")
        except json.JSONDecodeError:
            payload = {}
    return tool, payload, parse_error


def read_event():
    stdin_text = "" if sys.stdin.isatty() else sys.stdin.read()
    tool, payload, parse_error = parse_event(stdin_text, os.environ)
    if parse_error and not tool:
        # fail loud-open: we cannot inspect this edit, but the CI census gate is
        # the real backstop — surface it rather than silently allowing forever
        print("expectation-guard: WARNING — stdin present but unparseable as a tool "
              "event; cannot inspect this edit (harness schema change?). Allowing it "
              "— the CI `expectations` gate is the enforced backstop.", file=sys.stderr)
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

    # Fix 3: malformed stdin fails LOUD (parse_error set), not silently open
    _, _, err = parse_event("{not json", {})
    if not err:
        print("self-test FAIL parse-event: malformed stdin must set parse_error")
        fails += 1
    tool, _, err2 = parse_event('{"tool_name": "Edit", "tool_input": {}}', {})
    if tool != "Edit" or err2:
        print("self-test FAIL parse-event: a valid event must parse without error")
        fails += 1
    _, _, err3 = parse_event("", {})  # no stdin is normal, not an error
    if err3:
        print("self-test FAIL parse-event: empty stdin must not be an error")
        fails += 1

    # Fix 3: census-diff detects a net-removed/reworded expectation across files
    before = {"x `shouldBe` 1": 2, "y `shouldBe` 2": 1}
    if removed_from_census(before, {"x `shouldBe` 1": 2}) != ["y `shouldBe` 2"]:
        print("self-test FAIL census: a net removal must be detected")
        fails += 1
    if removed_from_census(before, before) != []:
        print("self-test FAIL census: an unchanged census must be clean")
        fails += 1

    if fails == 0:
        print(f"expectation-guard self-test: OK — {len(cases)} cases, "
              f"{len(tags_blocked | tags_passed)} scenarios, parse+census covered")
    return 1 if fails else 0


def main() -> int:
    if len(sys.argv) > 1 and sys.argv[1] == "--self-test":
        return self_test()
    if len(sys.argv) > 1 and sys.argv[1] == "--pr-diff":
        base = sys.argv[2] if len(sys.argv) > 2 else "origin/main"
        approved = os.environ.get("EXPECTATIONS_APPROVED", "").strip().lower() == "true"
        return pr_diff(base, approved)

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
