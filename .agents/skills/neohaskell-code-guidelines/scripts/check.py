#!/usr/bin/env python3
"""Run parsed HLint rules and reject runtime source dependencies on agent workspaces."""

import argparse
from collections import Counter
from dataclasses import dataclass
import json
from pathlib import Path
import subprocess
import sys
import tempfile

from cabal_extensions import parser_source, profiles
from source_rules import SourceNames, candidate_rule, code_source, signature_violations


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


def tracked_files(root):
    result = run(["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"], root)
    if result.returncode:
        raise ValueError(result.stderr.strip())
    paths = {Path(name) for name in result.stdout.split("\0") if name}
    return sorted(path for path in paths if path.parts[0] not in {".agents", ".github"} and (root / path).is_file())


def workspace_findings(root, paths):
    findings = []
    for path in paths:
        for line in (root / path).read_text().splitlines():
            count = line.count(".agents-state")
            finding = Finding(path.as_posix(), "", "Agent workspace in runtime source", ".agents-state", line.strip())
            findings.extend([finding] * count)
    return findings


def hlint_ideas(root, executable, paths, tracked):
    settings = profiles(root, paths, tracked)
    groups = {}
    with tempfile.TemporaryDirectory(prefix="neohaskell-lint-") as temporary:
        for path in paths:
            source = parser_source((root / path).read_text(), path, settings[path])
            target = root / path if path.suffix == ".lhs" else Path(temporary) / path
            if path.suffix != ".lhs":
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_text(source)
            groups.setdefault(settings[path], []).append(str(target))
        ideas = []
        for extensions, targets in groups.items():
            result = run([executable, "--json", "--show", "-j2", *["-X" + value for value in extensions], *targets], root)
            if result.returncode not in (0, 1):
                raise ValueError(f"HLint failed ({result.returncode}): {result.stderr.strip()}")
            try:
                batch = json.loads(result.stdout)
            except json.JSONDecodeError as error:
                raise ValueError(f"HLint did not return JSON: {result.stderr.strip()}") from error
            if not isinstance(batch, list):
                raise ValueError("HLint returned an invalid finding list.")
            if result.returncode and not batch:
                raise ValueError(f"HLint failed without findings: {result.stderr.strip()}")
            ideas.extend(batch)
        return ideas


def hlint_findings(root, executable, paths, tracked):
    version = run([executable, "--numeric-version"], root)
    if version.returncode or version.stdout.strip() != "3.10":
        raise ValueError("Use HLint 3.10, matching the pinned CI gate.")
    if not paths:
        return []
    ideas = hlint_ideas(root, executable, paths, tracked)
    sources = {path: (root / path).read_text() for path in paths}
    names = {path: SourceNames(code_source(source, path)) for path, source in sources.items()}
    findings = []
    for idea in ideas:
        rule = idea["hint"]
        if rule.startswith("Parse error"):
            raise ValueError(f"{rule} in {idea['file']} at line {idea['startLine']}.")
        path = Path(idea["file"])
        if path.is_absolute():
            path = path.relative_to(root)
        rule = candidate_rule(idea, names[path])
        if rule is None:
            continue
        ownership = rule in {"Avoid restricted module", "Avoid restricted function"} and any(note.startswith(OWNERSHIP_NOTE) for note in idea["note"])
        enforced = ownership or rule == "NeoHaskell: use Task.mapError" or rule.endswith("for yielded unit") or rule != idea["hint"]
        if idea["severity"] == "Ignore" and not enforced:
            continue
        lines = sources[path].splitlines()
        context = lines[idea["startLine"] - 1].strip()
        findings.append(Finding(path.as_posix(), ".".join(idea["decl"]), rule, idea["from"], context))
    for path, source in sources.items():
        for declaration, line, signature in signature_violations(source, path.as_posix()):
            findings.append(Finding(path.as_posix(), declaration, "NeoHaskell: use Text parameters", signature, source.splitlines()[line - 1].strip()))
    return findings


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--hlint", default="hlint")
    args = parser.parse_args()
    try:
        tracked = tracked_files(REPO)
        paths = [path for path in tracked if path.suffix in SOURCE_SUFFIXES]
        haskell = [path for path in paths if path.suffix in {".hs", ".lhs"}]
        findings = Counter(hlint_findings(REPO, args.hlint, haskell, tracked) + workspace_findings(REPO, paths))
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
