#!/usr/bin/env python3
"""Lint Haskell test files for anti-patterns the feature pipeline must refuse.

Usage:
    lint-test-patterns.py path/to/Spec.hs [path/to/another.hs ...]

Exits nonzero if any anti-pattern is found. Output format:

    path:line: rule: snippet

Rules:
    swallow-infra-failure
        case ... Err (ConnectionFailed _) -> pass
        case ... Err _ -> pass
        on a fixture/setup call (one of: mkStore, createTestStore, InMemory.new,
        Subscriber.new). Tests must not absorb infrastructure failures.

    name-body-mismatch
        Test name contains a side-effect verb (emits/reads in chunks/logs/
        deletes/writes/updates/replays/resumes) but body's assertion lines
        do not mention the same primitive. Heuristic; may have false positives.

    trivial-error-path
        it "fails with <X>" block whose setup is the canonical
        `Subscriber.new <something>.new Registry.empty` (or equivalent
        empty fixture) and whose body asserts Err (<X> _) -> pass.

    panicky-let
        Test body is just `let _x = builder ...` followed by `pass`. No
        meaningful assertion.

This is a guide, not a hard gate by itself — the pipeline orchestrator reads
the output and decides. False positives are acceptable as long as the rate
is low and the rules catch the bulk of real anti-patterns.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path


SIDE_EFFECT_VERBS = [
    "emits",
    "reads in chunks",
    "logs",
    "deletes",
    "writes",
    "updates",
    "replays",
    "resumes",
]

# Recognised fixture/setup-call names. If an Err _ -> pass appears within
# a `case` whose scrutinee references one of these, it counts as swallow.
SETUP_FIXTURE_NAMES = [
    "mkStore",
    "createTestStore",
    "createQueryObjectStore",
    "newFromConfig",
    "InMemory.new",
    "Subscriber.new",
    "Postgres.acquire",
]


def find_it_blocks(text: str) -> list[tuple[int, str, str]]:
    """Find every `it "<name>" \\_ -> do ...` block.

    Returns list of (start_line, test_name, body_lines_concatenated).
    Body extends until the next `it "` at the same indentation or end of file.
    """
    lines = text.splitlines()
    out: list[tuple[int, str, str]] = []
    i = 0
    while i < len(lines):
        m = re.match(r'^(\s*)it "([^"]+)"\s*\\_', lines[i])
        if m:
            indent = len(m.group(1))
            name = m.group(2)
            start = i + 1  # 1-indexed
            body_start = i + 1
            j = i + 1
            while j < len(lines):
                m2 = re.match(r"^(\s*)it \"", lines[j])
                if m2 and len(m2.group(1)) <= indent:
                    break
                j += 1
            body = "\n".join(lines[body_start:j])
            out.append((start, name, body))
            i = j
        else:
            i += 1
    return out


def lint_file(path: Path) -> list[tuple[Path, int, str, str]]:
    findings: list[tuple[Path, int, str, str]] = []
    text = path.read_text(encoding="utf-8", errors="replace")
    lines = text.splitlines()

    # Rule: swallow-infra-failure
    # The `Err _ -> pass` shape is legitimate when it's asserting an expected
    # error from a function-under-test (the variable was bound from e.g.
    # `Subscriber.rebuildFrom ... |> Task.asResult`). It's a swallow when the
    # variable was bound from a setup-fixture call (`mkStore`, `createTestStore`,
    # `InMemory.new`, etc.).
    #
    # To tell them apart: find the `case <var> of` line above the suspect line,
    # then find the most recent `<var> <-` binding above that, and check
    # whether its RHS (next 1-3 lines) references a setup-fixture name.
    setup_re = re.compile(
        r"\b(" + "|".join(re.escape(s) for s in SETUP_FIXTURE_NAMES) + r")\b"
    )
    err_pass_re = re.compile(r"Err\s*(\(\w+\s*_\)|_)\s*->\s*pass")
    case_of_re = re.compile(r"\bcase\s+(\w+)\s+of\b")
    enclosing_it_re = re.compile(r'^\s*it\s+"([^"]+)"')

    for idx, line in enumerate(lines, start=1):
        if not err_pass_re.search(line):
            continue
        # If the enclosing `it "<name>"` says "fails with <Variant>" and the
        # asserted Err variant matches, this is the test's whole point.
        err_variant_match = re.search(r"Err\s*\((\w+)\s*_\)\s*->\s*pass", line)
        asserted_variant = err_variant_match.group(1) if err_variant_match else None
        enclosing_it_name = None
        for j in range(idx - 1, max(0, idx - 40), -1):
            m_it = enclosing_it_re.match(lines[j])
            if m_it:
                enclosing_it_name = m_it.group(1)
                break
        if (
            enclosing_it_name
            and asserted_variant
            and ("fails with " + asserted_variant) in enclosing_it_name
        ):
            continue
        # walk up to find the `case <var> of` for this branch
        case_var: str | None = None
        case_line_no = -1
        for j in range(idx - 1, max(0, idx - 30), -1):
            m = case_of_re.search(lines[j])
            if m:
                case_var = m.group(1)
                case_line_no = j
                break
        if case_var is None:
            continue
        # walk up from the case line to find `<var> <-`
        bind_re = re.compile(rf"^\s*{re.escape(case_var)}\s*<-")
        bind_line_no = -1
        for j in range(case_line_no - 1, max(0, case_line_no - 30), -1):
            if bind_re.match(lines[j]):
                bind_line_no = j
                break
        if bind_line_no < 0:
            continue
        # check the next 1-4 lines after the binding for a setup-fixture name
        rhs = "\n".join(lines[bind_line_no : min(len(lines), bind_line_no + 4)])
        # Heuristic refinement: also skip if a "function under test" name is on
        # the same RHS (it overrides). Common function-under-test patterns:
        function_under_test_re = re.compile(
            r"\b(rebuildFrom|rebuildAllAsync|readinessOf|readinessOfQuery|"
            r"rebuildAll|start|stop|handleReadinessRequest|atomicUpdate|get|getAll)\b"
        )
        if function_under_test_re.search(rhs):
            continue
        if setup_re.search(rhs):
            findings.append(
                (
                    path,
                    idx,
                    "swallow-infra-failure",
                    line.strip(),
                )
            )

    # Rule: name-body-mismatch and trivial-error-path and panicky-let
    for start, name, body in find_it_blocks(text):
        name_lower = name.lower()

        # name-body-mismatch
        for verb in SIDE_EFFECT_VERBS:
            if verb in name_lower:
                # crude: check whether any line in body mentions the verb
                # or a closely-related primitive (chunkSize, log, delete, write, etc.)
                signal_words = {
                    "emits": ["emit", "Log."],
                    "reads in chunks": ["chunkSize", "chunk"],
                    "logs": ["Log.", "log"],
                    "deletes": ["delete", "Delete", "DROP"],
                    "writes": ["write", "atomicUpdate", "INSERT"],
                    "updates": ["update", "atomicUpdate", "UPDATE"],
                    "replays": ["replay", "rebuildFrom"],
                    "resumes": ["resume", "resumeFromCheckpoint", "startPosition"],
                }.get(verb, [verb])
                if not any(w in body for w in signal_words):
                    findings.append(
                        (
                            path,
                            start,
                            "name-body-mismatch",
                            f'it "{name}" — body does not reference {verb}',
                        )
                    )
                break

        # trivial-error-path
        fails_with = re.match(r"fails with ([A-Z]\w+)", name)
        if fails_with:
            variant = fails_with.group(1)
            empty_fixture = (
                "Registry.empty" in body
                or re.search(r"\bSubscriber\.new \w+ Registry\.empty\b", body)
            )
            asserts_variant = (
                re.search(rf"Err \({variant}\s+_\)\s*->\s*pass", body)
                is not None
            )
            mentions_fixture_double = any(
                kw in body
                for kw in [
                    "QueryUpdater {",
                    "{ readAllEventsForwardFrom",
                    "{ get =",
                    "CheckpointStore",
                    "Task.throw",
                ]
            )
            if asserts_variant and empty_fixture and not mentions_fixture_double:
                findings.append(
                    (
                        path,
                        start,
                        "trivial-error-path",
                        f'it "{name}" — empty fixture cannot reach {variant} branch',
                    )
                )

        # panicky-let
        body_lines = [
            ln
            for ln in (line.strip() for line in body.splitlines())
            if ln and not ln.startswith("--")
        ]
        if body_lines and all(
            (ln.startswith("let _") or ln.startswith("|>") or ln == "pass" or ln.startswith("Application."))
            for ln in body_lines
        ):
            if body_lines.count("pass") == 1 and any(
                "let _" in ln for ln in body_lines
            ):
                findings.append(
                    (
                        path,
                        start,
                        "panicky-let",
                        f'it "{name}" — body is just let _x = builder; pass',
                    )
                )

    return findings


def main(argv: list[str]) -> int:
    if len(argv) < 2:
        print(__doc__, file=sys.stderr)
        return 2
    all_findings: list[tuple[Path, int, str, str]] = []
    for p in argv[1:]:
        path = Path(p)
        if not path.exists():
            print(f"{p}: file not found", file=sys.stderr)
            return 2
        all_findings.extend(lint_file(path))
    for p, line, rule, snippet in all_findings:
        print(f"{p}:{line}: {rule}: {snippet}")
    return 1 if all_findings else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
