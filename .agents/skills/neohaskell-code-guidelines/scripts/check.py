#!/usr/bin/env python3
"""Run parsed HLint rules and reject runtime source dependencies on agent workspaces."""

import argparse
from collections import Counter
from dataclasses import dataclass
import json
from pathlib import Path
import subprocess
import sys


OWNER = Path(__file__).resolve().parents[1]
REPO = OWNER.parents[2]
SOURCE_SUFFIXES = {".hs", ".lhs", ".py", ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs", ".sh", ".bash", ".c", ".h", ".rs", ".go"}
OWNERSHIP_NOTE = "NeoHaskell ownership."


@dataclass(frozen=True, order=True)
class Finding:
    path: str
    declaration: str
    rule: str
    source: str
    context: str


def run(arguments, root):
    return subprocess.run(arguments, cwd=root, text=True, capture_output=True, check=False)


def tracked_sources(root):
    result = run(["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"], root)
    if result.returncode:
        raise ValueError(result.stderr.strip())
    paths = {Path(name) for name in result.stdout.split("\0") if name}
    return sorted(path for path in paths if path.suffix in SOURCE_SUFFIXES and path.parts[0] not in {".agents", ".github"} and (root / path).is_file())


def workspace_findings(root, paths):
    findings = []
    for path in paths:
        for line in (root / path).read_text().splitlines():
            count = line.count(".agents-state")
            finding = Finding(path.as_posix(), "", "Agent workspace in runtime source", ".agents-state", line.strip())
            findings.extend([finding] * count)
    return findings


def hlint_findings(root, executable, paths):
    version = run([executable, "--numeric-version"], root)
    if version.returncode or version.stdout.strip() != "3.10":
        raise ValueError("Use HLint 3.10, matching the pinned CI gate.")
    if not paths:
        return []
    result = run([executable, "--json", "--show", "-j2", *[str(path) for path in paths]], root)
    if result.returncode not in (0, 1):
        raise ValueError(f"HLint failed ({result.returncode}): {result.stderr.strip()}")
    try:
        ideas = json.loads(result.stdout)
    except json.JSONDecodeError as error:
        raise ValueError(f"HLint did not return JSON: {result.stderr.strip()}") from error
    if not isinstance(ideas, list):
        raise ValueError("HLint returned an invalid finding list.")
    if result.returncode and not ideas:
        raise ValueError(f"HLint failed without findings: {result.stderr.strip()}")
    findings = []
    for idea in ideas:
        rule = idea["hint"]
        if rule.startswith("Parse error"):
            raise ValueError(f"{rule} in {idea['file']} at line {idea['startLine']}.")
        ownership = rule in {"Avoid restricted module", "Avoid restricted function"} and any(note.startswith(OWNERSHIP_NOTE) for note in idea["note"])
        enforced = ownership or rule == "NeoHaskell: use Task.mapError"
        if idea["severity"] == "Ignore" and not enforced:
            continue
        path = Path(idea["file"])
        if path.is_absolute():
            path = path.relative_to(root)
        lines = (root / path).read_text().splitlines()
        context = lines[idea["startLine"] - 1].strip()
        findings.append(Finding(path.as_posix(), ".".join(idea["decl"]), rule, idea["from"], context))
    return findings


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--hlint", default="hlint")
    args = parser.parse_args()
    try:
        paths = tracked_sources(REPO)
        haskell = [path for path in paths if path.suffix in {".hs", ".lhs"}]
        findings = Counter(hlint_findings(REPO, args.hlint, haskell) + workspace_findings(REPO, paths))
        for finding, count in sorted(findings.items()):
            print(f"FAIL {finding.path} [{finding.declaration}] {finding.rule} ({count}): {finding.context}")
        if findings:
            print(f"FAIL. {sum(findings.values())} violations across {len(findings)} findings. Fix every reported violation.")
            return 1
        print(f"PASS. {len(haskell)} Haskell files and {len(paths)} source files checked.")
        return 0
    except (OSError, ValueError, KeyError, TypeError) as error:
        print(f"FAIL. {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
