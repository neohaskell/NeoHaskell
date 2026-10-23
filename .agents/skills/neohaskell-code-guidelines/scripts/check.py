#!/usr/bin/env python3
"""Run parsed HLint rules and reject new tracked runtime workspace references."""

import argparse
from collections import Counter
from dataclasses import asdict, dataclass
import json
from pathlib import Path
import subprocess
import sys


OWNER = Path(__file__).resolve().parents[1]
REPO = OWNER.parents[2]
DEBT = OWNER / "references/lint-debt.json"
SOURCE_SUFFIXES = {".hs", ".lhs", ".py", ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs", ".sh", ".bash", ".c", ".h", ".rs", ".go"}
ENFORCED_HINTS = {"Avoid restricted module", "Avoid restricted function", "NeoHaskell: use Task.mapError"}


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
        if idea["severity"] == "Ignore" and rule not in ENFORCED_HINTS:
            continue
        path = Path(idea["file"])
        if path.is_absolute():
            path = path.relative_to(root)
        lines = (root / path).read_text().splitlines()
        context = lines[idea["startLine"] - 1].strip()
        findings.append(Finding(path.as_posix(), ".".join(idea["decl"]), rule, idea["from"], context))
    return findings


def read_debt(path):
    document = json.loads(path.read_text())
    if document.get("schema") != 1 or document.get("hlint") != "3.10":
        raise ValueError("Unsupported lint debt format or HLint version.")
    findings = Counter()
    for entry in document["findings"]:
        count = entry["count"]
        if not isinstance(count, int) or isinstance(count, bool) or count < 1:
            raise ValueError("Lint debt counts must be positive integers.")
        finding = Finding(**{key: entry[key] for key in Finding.__dataclass_fields__})
        if finding in findings:
            raise ValueError("Duplicate lint debt entry.")
        findings[finding] = count
    return document, findings


def write_debt(path, document, findings):
    document["findings"] = [{**asdict(finding), "count": count} for finding, count in sorted(findings.items())]
    header = {key: value for key, value in document.items() if key != "findings"}
    text = json.dumps(header, indent=2)[:-2] + ',\n  "findings": [\n'
    text += ",\n".join("    " + json.dumps(entry, ensure_ascii=False) for entry in document["findings"])
    path.write_text(text + "\n  ]\n}\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--hlint", default="hlint")
    parser.add_argument("--prune-debt", action="store_true", help="Remove resolved entries only. New findings still fail.")
    parser.add_argument("--report", type=Path, help="Write current findings as JSON for review.")
    args = parser.parse_args()
    try:
        if args.report and args.report.resolve() == DEBT:
            raise ValueError("A findings report cannot overwrite the accepted debt inventory.")
        paths = tracked_sources(REPO)
        haskell = [path for path in paths if path.suffix in {".hs", ".lhs"}]
        current = Counter(hlint_findings(REPO, args.hlint, haskell) + workspace_findings(REPO, paths))
        document, previous = read_debt(DEBT)
        if args.report:
            write_debt(args.report, document.copy(), current)
        added = current - previous
        removed = previous - current
        for finding, count in sorted(added.items()):
            print(f"NEW {finding.path} [{finding.declaration}] {finding.rule} ({count}): {finding.context}")
        if added:
            print("FAIL. Fix new findings. The debt inventory cannot approve new violations.")
            return 1
        if removed and args.prune_debt:
            write_debt(DEBT, document, current)
            print(f"Removed {sum(removed.values())} resolved debt occurrences.")
        elif removed:
            print(f"FAIL. {sum(removed.values())} stale debt occurrences. Run with --prune-debt and review the deletion.")
            return 1
        print(f"PASS. {len(haskell)} Haskell files and {len(paths)} source files checked. {sum(current.values())} exact debt occurrences remain.")
        return 0
    except (OSError, ValueError, KeyError, TypeError) as error:
        print(f"FAIL. {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
