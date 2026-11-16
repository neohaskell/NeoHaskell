#!/usr/bin/env python3
"""Classify installer inputs and require exact local evidence for pushed revisions."""

from __future__ import annotations

import argparse
import fnmatch
import hashlib
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import tarfile
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[2]
POLICY_DIR = Path(__file__).resolve().parent
EXPECTED_LANES = {"linux-x86_64", "linux-arm64", "macos-arm64", "wsl2-x86_64", "wsl2-arm64"}
ZERO_OID = re.compile(r"^0+$")
OID = re.compile(r"^(?:[0-9a-f]{40}|[0-9a-f]{64})$")
HEX = re.compile(r"^[0-9a-f]{64}$")
PROTOCOL = 1
ADAPTER_PATHS = {lane: f"installer/verification/adapters/{lane}/run.ps1" if lane.startswith("wsl2-") else f"installer/verification/adapters/{lane}/run.py" for lane in EXPECTED_LANES}


class VerificationError(Exception):
    pass


@dataclass(frozen=True)
class InputEntry:
    family: str
    path: str
    mode: str
    digest: str


@dataclass(frozen=True)
class InputIdentity:
    policy_digest: str
    entries: tuple[InputEntry, ...]

    def as_json(self) -> dict[str, Any]:
        return {
            "policy_digest": self.policy_digest,
            "entries": [entry.__dict__ for entry in self.entries],
            "digest": digest_json({"policy_digest": self.policy_digest, "entries": [entry.__dict__ for entry in self.entries]}),
        }


@dataclass(frozen=True)
class ArtifactIdentity:
    name: str
    digest: str
    size: int


@dataclass(frozen=True)
class CandidateIdentity:
    manifest_digest: str
    version: str
    source_commit: str
    artifacts: tuple[ArtifactIdentity, ...]
    previous_version: str
    previous_artifacts: tuple[ArtifactIdentity, ...]

    def as_json(self) -> dict[str, Any]:
        body = {
            "manifest_digest": self.manifest_digest,
            "version": self.version,
            "source_commit": self.source_commit,
            "artifacts": [item.__dict__ for item in self.artifacts],
            "previous_pair": {"version": self.previous_version, "artifacts": [item.__dict__ for item in self.previous_artifacts]},
        }
        return {**body, "digest": digest_json(body)}


@dataclass(frozen=True)
class SuiteIdentity:
    suite_id: str
    policy_digest: str
    verifier_digest: str
    coverage_digest: str
    fixtures_digest: str
    runner_contract_digest: str

    def as_json(self) -> dict[str, Any]:
        body = self.__dict__
        return {**body, "digest": digest_json(body)}


def digest_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def canonical_json(value: Any) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode()


def digest_json(value: Any) -> str:
    return digest_bytes(canonical_json(value))


def strict_json(value: bytes, label: str) -> Any:
    def pairs(items: list[tuple[str, Any]]) -> dict[str, Any]:
        result: dict[str, Any] = {}
        for key, item in items:
            if key in result:
                raise VerificationError(f"{label} contains duplicate JSON key {key!r}")
            result[key] = item
        return result

    try:
        return json.loads(value, object_pairs_hook=pairs)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise VerificationError(f"cannot parse {label}: {error}") from error


def git(repo: Path, *args: str, input_bytes: bytes | None = None, check: bool = True) -> subprocess.CompletedProcess[bytes]:
    result = subprocess.run(["git", "-C", str(repo), *args], input=input_bytes, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if check and result.returncode:
        detail = result.stderr.decode(errors="replace").strip()
        raise VerificationError(f"git {' '.join(args)} failed: {detail}")
    return result


def git_text(repo: Path, *args: str, input_bytes: bytes | None = None, check: bool = True) -> str:
    return git(repo, *args, input_bytes=input_bytes, check=check).stdout.decode(errors="strict").strip()


def object_commit(repo: Path, reference: str) -> str:
    if not OID.fullmatch(reference):
        raise VerificationError(f"expected a full Git object id, got {reference!r}")
    return git_text(repo, "rev-parse", "--verify", f"{reference}^{{commit}}")


def read_policy(policy_dir: Path) -> tuple[dict[str, Any], dict[str, Any], str]:
    inputs_raw = (policy_dir / "inputs.json").read_bytes()
    coverage_raw = (policy_dir / "coverage.json").read_bytes()
    inputs = strict_json(inputs_raw, "inputs.json")
    coverage = strict_json(coverage_raw, "coverage.json")
    if inputs.get("schema_version") != 1 or not isinstance(inputs.get("rules"), list):
        raise VerificationError("inputs.json has an unsupported schema")
    projection = inputs.get("flake_projection")
    if not isinstance(projection, dict) or not isinstance(projection.get("independent_shapes"), list):
        raise VerificationError("inputs.json must define approved shared-file shapes")
    for shape in projection["independent_shapes"]:
        if not isinstance(shape, dict) or set(shape) != {"projection_sha256", "dependencies"} or not HEX.fullmatch(str(shape["projection_sha256"])):
            raise VerificationError("inputs.json has a malformed approved shared-file shape")
        dependencies = shape["dependencies"]
        if not isinstance(dependencies, dict) or any(not isinstance(path, str) or Path(path).is_absolute() or ".." in Path(path).parts or not HEX.fullmatch(str(value)) for path, value in dependencies.items()):
            raise VerificationError("inputs.json has malformed shared-file dependencies")
    if coverage.get("schema_version") != 1:
        raise VerificationError("coverage.json has an unsupported schema")
    validate_coverage(coverage)
    return inputs, coverage, digest_json({"inputs": digest_bytes(inputs_raw), "coverage": digest_bytes(coverage_raw)})


def validate_coverage(coverage: dict[str, Any]) -> None:
    capabilities = coverage.get("capabilities")
    lanes = coverage.get("lanes")
    cases = coverage.get("cases")
    if not isinstance(capabilities, dict) or not isinstance(lanes, list) or not isinstance(cases, list):
        raise VerificationError("coverage.json must define capabilities, lanes, and cases")
    lane_ids = [lane.get("id") for lane in lanes if isinstance(lane, dict) and lane.get("required") is True]
    if len(lane_ids) != len(set(lane_ids)) or set(lane_ids) != EXPECTED_LANES:
        raise VerificationError("coverage.json must require each of the five supported lanes exactly once")
    case_ids = [case.get("id") for case in cases if isinstance(case, dict)]
    if len(case_ids) != len(cases) or len(case_ids) != len(set(case_ids)):
        raise VerificationError("coverage.json contains malformed or duplicate case ids")
    known_cases = set(case_ids)
    known_capabilities = set(capabilities)
    common = coverage.get("common_required_cases")
    if not isinstance(common, list) or len(common) != len(set(common)) or not set(common) <= known_cases:
        raise VerificationError("coverage.json has invalid common_required_cases")
    for lane in lanes:
        if lane.get("required") is not True:
            continue
        if not isinstance(lane.get("environment"), str) or not lane["environment"].strip():
            raise VerificationError(f"lane {lane.get('id')} has no environment description")
        lane_cases = lane.get("required_cases")
        lane_caps = lane.get("required_capabilities")
        if not isinstance(lane_cases, list) or len(lane_cases) != len(set(lane_cases)) or not set(lane_cases) <= known_cases:
            raise VerificationError(f"lane {lane['id']} has invalid required_cases")
        if not isinstance(lane_caps, list) or len(lane_caps) != len(set(lane_caps)) or not set(lane_caps) <= known_capabilities:
            raise VerificationError(f"lane {lane['id']} has invalid required_capabilities")
    for case in cases:
        if not isinstance(case.get("capability"), str) or case["capability"] not in known_capabilities:
            raise VerificationError(f"case {case.get('id')} references an unknown capability")
    conditional = coverage.get("conditional_cases", [])
    if not isinstance(conditional, list):
        raise VerificationError("coverage.json conditional_cases must be an array")
    conditional_ids: set[str] = set()
    for item in conditional:
        if not isinstance(item, dict) or not isinstance(item.get("id"), str) or item["id"] in known_cases or item["id"] in conditional_ids:
            raise VerificationError("coverage.json has an invalid or duplicate conditional case")
        conditional_ids.add(item["id"])
        if item.get("capability") not in known_capabilities or not set(item.get("lanes", [])) <= EXPECTED_LANES:
            raise VerificationError(f"conditional case {item.get('id')} has invalid capability or lanes")


def conditional_applicability(coverage: dict[str, Any], lane_id: str) -> dict[str, bool] | None:
    items = [item for item in coverage.get("conditional_cases", []) if lane_id in item["lanes"]]
    if not items:
        return {}
    floors = coverage.get("support_floors", {})
    values = floors.get("values") if floors.get("status") == "resolved" else None
    if not isinstance(values, dict):
        return None
    variants = values.get("nix_variants") or values.get("variants")
    if not isinstance(variants, dict) or not isinstance(variants.get(lane_id), list):
        return None
    supported = variants[lane_id]
    if not all(isinstance(value, str) for value in supported):
        return None
    result: dict[str, bool] = {}
    for item in items:
        variant = item["id"].rsplit(".", 1)[-1]
        result[item["id"]] = variant in supported
    return result


def required_cases(coverage: dict[str, Any], lane_id: str) -> tuple[list[str], list[str]]:
    lane = next(item for item in coverage["lanes"] if item["id"] == lane_id)
    ids = list(dict.fromkeys([*coverage["common_required_cases"], *lane["required_cases"]]))
    applicability = conditional_applicability(coverage, lane_id)
    if applicability is None:
        return ids, [item["id"] for item in coverage.get("conditional_cases", []) if lane_id in item["lanes"]]
    ids.extend(case_id for case_id, applies in applicability.items() if applies)
    return ids, []


def tokenize_nix(source: str) -> list[str]:
    tokens: list[str] = []
    index = 0
    while index < len(source):
        if source[index].isspace():
            index += 1
            continue
        if source.startswith("/*", index):
            depth = 1
            index += 2
            while index < len(source) and depth:
                if source.startswith("/*", index):
                    depth += 1
                    index += 2
                elif source.startswith("*/", index):
                    depth -= 1
                    index += 2
                else:
                    index += 1
            if depth:
                raise VerificationError("flake.nix has an unterminated block comment")
            continue
        if source[index] == "#":
            newline = source.find("\n", index)
            index = len(source) if newline < 0 else newline + 1
            continue
        if source.startswith("''", index):
            start = index
            index += 2
            while index < len(source):
                if source.startswith("'''", index):
                    index += 3
                elif source.startswith("''", index):
                    index += 2
                    break
                else:
                    index += 1
            else:
                raise VerificationError("flake.nix has an unterminated indented string")
            tokens.append(source[start:index])
            continue
        if source[index] == '"':
            start = index
            index += 1
            while index < len(source):
                if source[index] == "\\":
                    index += 2
                elif source[index] == '"':
                    index += 1
                    break
                else:
                    index += 1
            else:
                raise VerificationError("flake.nix has an unterminated string")
            tokens.append(source[start:index])
            continue
        if source.startswith("//", index) or source.startswith("->", index) or source.startswith("==", index):
            tokens.append(source[index : index + 2])
            index += 2
            continue
        match = re.match(r"[A-Za-z_][A-Za-z0-9_'-]*|[0-9]+(?:\.[0-9]+)*", source[index:])
        if match:
            tokens.append(match.group(0))
            index += len(match.group(0))
            continue
        tokens.append(source[index])
        index += 1
    return tokens


def matching(tokens: list[str], opening: int) -> int:
    pairs = {"{": "}", "[": "]", "(": ")"}
    if tokens[opening] not in pairs:
        raise VerificationError("internal flake tokenizer error")
    stack = [pairs[tokens[opening]]]
    for index in range(opening + 1, len(tokens)):
        token = tokens[index]
        if token in pairs:
            stack.append(pairs[token])
        elif token in {"}", "]", ")"}:
            if not stack or token != stack.pop():
                raise VerificationError("flake.nix has unbalanced delimiters")
            if not stack:
                return index
    raise VerificationError("flake.nix has unbalanced delimiters")


def split_bindings(tokens: list[str], start: int, end: int) -> list[tuple[int, int]]:
    result: list[tuple[int, int]] = []
    part = start
    index = start
    while index < end:
        if tokens[index] in {"{", "[", "("}:
            index = matching(tokens, index) + 1
        elif tokens[index] == ";":
            result.append((part, index + 1))
            part = index + 1
            index += 1
        else:
            index += 1
    if part < end:
        result.append((part, end))
    return result


def flake_projection(source: str, policy: dict[str, Any], dependencies: dict[str, str]) -> bytes:
    tokens = tokenize_nix(source)
    markers = [i for i in range(len(tokens) - 2) if tokens[i : i + 3] == ["in", "flake", "//"]]
    output_sets = [i + 3 for i in markers if i + 3 < len(tokens) and tokens[i + 3] == "{"]
    if len(output_sets) != 1:
        raise VerificationError("cannot identify the root flake output attribute set; refusing an ambiguous shared-file edit")
    opening = output_sets[0]
    closing = matching(tokens, opening)
    bindings = split_bindings(tokens, opening + 1, closing)
    devshell = policy["flake_projection"]["excluded_output_attribute"]
    excluded: set[int] = set()
    devshell_ranges: list[tuple[int, int]] = []
    for start, end in bindings:
        if start < end and tokens[start] == devshell and start + 1 < end and tokens[start + 1] == "=":
            excluded.update(range(start, end))
            devshell_ranges.append((start, end))
    if len(devshell_ranges) > 1:
        raise VerificationError("flake.nix has ambiguous devShells bindings")
    if devshell_ranges:
        start, end = devshell_ranges[0]
        outside_references = [
            i for i, token in enumerate(tokens)
            if i not in range(start, end) and (token == devshell or (token.startswith(('"', "''")) and devshell in token))
        ]
        if outside_references:
            excluded.difference_update(range(start, end))
            devshell_ranges = []
    tooling_name = policy["flake_projection"]["excluded_tooling_binding"]
    tooling_occurrences = [i for i, token in enumerate(tokens) if token == tooling_name]
    tooling_string_refs = [i for i, token in enumerate(tokens) if token.startswith(('"', "''")) and tooling_name in token]
    if len(tooling_occurrences) == 2 and not tooling_string_refs and devshell_ranges and any(devshell_ranges[0][0] <= i < devshell_ranges[0][1] for i in tooling_occurrences):
        candidate = [i for i in range(len(tokens) - 1) if tokens[i] == tooling_name and tokens[i + 1] == "="]
        if len(candidate) == 1:
            declaration = candidate[0]
            prior_lets = [i for i in range(declaration) if tokens[i] == "let"]
            if prior_lets:
                let_start = prior_lets[-1]
                depth = 0
                scope_end = None
                i = let_start + 1
                while i < declaration:
                    if tokens[i] in {"{", "[", "("}:
                        i = matching(tokens, i) + 1
                    elif tokens[i] == "in":
                        scope_end = i
                        break
                    else:
                        i += 1
                if scope_end is None:
                    i = declaration + 2
                    while i < len(tokens):
                        if tokens[i] in {"{", "[", "("}:
                            i = matching(tokens, i) + 1
                        elif tokens[i] == "in":
                            scope_end = i
                            break
                        else:
                            i += 1
                if scope_end is not None and let_start < declaration < scope_end:
                    binding_end = declaration + 2
                    while binding_end < scope_end:
                        if tokens[binding_end] in {"{", "[", "("}:
                            binding_end = matching(tokens, binding_end) + 1
                        elif tokens[binding_end] == ";":
                            binding_end += 1
                            break
                        else:
                            binding_end += 1
                    excluded.update(range(declaration, binding_end))
    projected = canonical_json([token for i, token in enumerate(tokens) if i not in excluded])
    for shape in policy["flake_projection"].get("independent_shapes", []):
        if shape["projection_sha256"] == digest_bytes(projected) and shape["dependencies"] == dependencies:
            return projected
    return canonical_json(tokens)


def rules_for_path(inputs: dict[str, Any], path: str) -> str | None:
    if Path(path).suffix.lower() in {".md", ".markdown", ".rst", ".adoc"}:
        return None
    matches = [rule["family"] for rule in inputs["rules"] if any(fnmatch.fnmatchcase(path, pattern) for pattern in rule["paths"])]
    if len(matches) > 1:
        raise VerificationError(f"input path {path} is assigned to multiple families: {matches}")
    return matches[0] if matches else None


def tree_inputs(repo: Path, commit: str, policy_dir: Path) -> tuple[InputIdentity, dict[str, InputEntry]]:
    inputs, coverage, policy_digest = read_policy(policy_dir)
    output = git(repo, "ls-tree", "-r", "-z", "--full-tree", commit).stdout
    dependency_paths = {path for shape in inputs["flake_projection"].get("independent_shapes", []) for path in shape["dependencies"]}
    dependencies = {}
    for record in output.split(b"\0"):
        if not record:
            continue
        header, raw_path = record.split(b"\t", 1)
        path = os.fsdecode(raw_path)
        mode, kind, oid = header.decode().split(" ")
        if path in dependency_paths and kind == "blob" and mode in {"100644", "100755"}:
            dependencies[path] = digest_bytes(git(repo, "cat-file", "blob", oid).stdout)
    entries: dict[str, InputEntry] = {}
    for record in output.split(b"\0"):
        if not record:
            continue
        header, raw_path = record.split(b"\t", 1)
        mode, kind, oid = header.decode().split(" ")
        path = os.fsdecode(raw_path)
        family = rules_for_path(inputs, path)
        if family is None:
            continue
        if kind != "blob" or mode == "120000":
            raise VerificationError(f"installer-owned input {path} is a symlink or submodule; source identity refuses indirect content")
        data = git(repo, "cat-file", "blob", oid).stdout
        projected = flake_projection(data.decode(), inputs, dependencies) if path == inputs["flake_projection"]["path"] else data
        entries[path] = InputEntry(family, path, mode, digest_bytes(projected))
    identity = InputIdentity(policy_digest, tuple(entries[path] for path in sorted(entries)))
    return identity, entries


def delta_families(left: dict[str, InputEntry], right: dict[str, InputEntry]) -> list[str]:
    changed = [path for path in sorted(set(left) | set(right)) if left.get(path) != right.get(path)]
    return sorted({(left.get(path) or right[path]).family for path in changed})


def suite_identity(repo: Path, commit: str, policy_dir: Path, coverage: dict[str, Any], policy_digest: str) -> SuiteIdentity:
    verifier = git(repo, "show", f"{commit}:installer/verification/verify.py", check=False)
    if verifier.returncode:
        raise VerificationError(f"{commit} does not contain the maintained verification entrypoint")
    coverage_blob = git(repo, "show", f"{commit}:installer/verification/coverage.json").stdout
    fixture_entries = git(repo, "ls-tree", "-r", "-z", "--full-tree", commit).stdout
    fixture_hashes: list[tuple[str, str]] = []
    for record in fixture_entries.split(b"\0"):
        if not record:
            continue
        header, raw_path = record.split(b"\t", 1)
        _, kind, oid = header.decode().split(" ")
        path = os.fsdecode(raw_path)
        if kind == "blob" and (path.startswith("installer/verification/tests/") or path.startswith("installer/verification/fixtures/") or path.startswith("installer/verification/adapters/")):
            fixture_hashes.append((path, digest_bytes(git(repo, "cat-file", "blob", oid).stdout)))
    contract = {"protocol": PROTOCOL, "python": [sys.version_info.major, sys.version_info.minor], "lanes": coverage["lanes"], "conditional_cases": coverage.get("conditional_cases", []), "adapters": ADAPTER_PATHS}
    return SuiteIdentity(
        coverage.get("suite_id", ""), policy_digest, digest_bytes(verifier.stdout), digest_bytes(coverage_blob),
        digest_json(fixture_hashes), digest_json(contract),
    )


def validate_target_policy(repo: Path, commit: str, policy_dir: Path) -> None:
    for name in ("verify.py", "inputs.json", "coverage.json"):
        committed = git(repo, "show", f"{commit}:installer/verification/{name}", check=False)
        current = (policy_dir / name).read_bytes()
        if committed.returncode or digest_bytes(committed.stdout) != digest_bytes(current):
            raise VerificationError(f"pushed target policy differs from the installed snapshot at {name}; refresh setup-hooks and run verification for {commit}")


def input_identity(repo: Path, commit: str, policy_dir: Path) -> InputIdentity:
    return tree_inputs(repo, commit, policy_dir)[0]


def is_ancestor(repo: Path, ancestor: str, descendant: str) -> bool:
    result = git(repo, "merge-base", "--is-ancestor", ancestor, descendant, check=False)
    if result.returncode not in (0, 1):
        raise VerificationError(result.stderr.decode(errors="replace").strip())
    return result.returncode == 0


def incoming_commits(repo: Path, target: str, baselines: list[str]) -> list[str]:
    args = ["rev-list", target]
    if baselines:
        args.extend(["--not", *baselines])
    result = git(repo, *args)
    return result.stdout.decode().splitlines()


def destination_baselines(repo: Path, remote: str) -> list[tuple[str, str]]:
    advertised = git(repo, "ls-remote", "--refs", remote, check=False)
    if advertised.returncode:
        raise VerificationError(f"cannot inspect refs advertised by destination {remote!r}; fetch the destination refs and retry")
    refs = []
    for line in advertised.stdout.decode(errors="replace").splitlines():
        parts = line.split("\t", 1)
        if len(parts) != 2:
            continue
        oid, ref = parts
        if not OID.fullmatch(oid):
            raise VerificationError(f"destination advertised malformed object id for {ref}")
        try:
            commit = object_commit(repo, oid)
        except VerificationError as error:
            raise VerificationError(f"destination history for {ref} is unavailable locally; run git fetch {shlex.quote(remote)} {shlex.quote(ref)} and retry") from error
        refs.append((commit, ref))
    return refs


def candidate_artifacts(items: Any, manifest_dir: Path, label: str) -> tuple[ArtifactIdentity, ...]:
    if not isinstance(items, list) or not items:
        raise VerificationError(f"candidate {label} must contain at least one artifact")
    result = []
    names = set()
    for item in items:
        if not isinstance(item, dict) or set(item) != {"name", "path", "sha256", "size"}:
            raise VerificationError(f"candidate {label} has a malformed artifact entry")
        name, relative, expected, size = item["name"], item["path"], item["sha256"], item["size"]
        if not isinstance(name, str) or not name or name in names or not isinstance(relative, str) or not HEX.fullmatch(str(expected)) or not isinstance(size, int) or size < 0:
            raise VerificationError(f"candidate {label} has an invalid artifact identity")
        names.add(name)
        path = manifest_dir / relative
        if Path(relative).is_absolute() or ".." in Path(relative).parts or path.is_symlink() or not path.is_file():
            raise VerificationError(f"candidate artifact {name} is missing or outside the manifest directory")
        data = path.read_bytes()
        if digest_bytes(data) != expected or len(data) != size:
            raise VerificationError(f"candidate artifact {name} does not match its declared digest and size")
        result.append(ArtifactIdentity(name, expected, size))
    return tuple(result)


def load_candidate(path: Path, repo: Path, source: str) -> CandidateIdentity:
    raw = path.read_bytes()
    value = strict_json(raw, str(path))
    if not isinstance(value, dict) or set(value) != {"schema_version", "version", "source_commit", "artifacts", "previous_pair"} or value.get("schema_version") != 1:
        raise VerificationError("candidate manifest must use schema_version 1 and declare version, source_commit, artifacts, and previous_pair")
    if not isinstance(value["version"], str) or not value["version"].strip():
        raise VerificationError("candidate version is empty")
    source_commit = object_commit(repo, value["source_commit"])
    if source_commit != source:
        raise VerificationError(f"candidate source {source_commit} does not match requested source {source}")
    previous = value["previous_pair"]
    if not isinstance(previous, dict) or set(previous) != {"version", "artifacts"} or not isinstance(previous["version"], str) or not previous["version"].strip():
        raise VerificationError("candidate previous_pair must identify a version and its exact artifacts")
    artifacts = candidate_artifacts(value["artifacts"], path.parent, "artifacts")
    previous_artifacts = candidate_artifacts(previous["artifacts"], path.parent, "previous_pair.artifacts")
    return CandidateIdentity(digest_bytes(raw), value["version"], source_commit, artifacts, previous["version"], previous_artifacts)


def common_dir(repo: Path) -> Path:
    value = git_text(repo, "rev-parse", "--git-common-dir")
    path = Path(value)
    return (repo / path).resolve() if not path.is_absolute() else path.resolve()


def store_root(repo: Path) -> Path:
    return common_dir(repo) / "installer-verification"


def write_atomic(path: Path, data: bytes, mode: int = 0o600) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.{os.getpid()}.{time.time_ns()}.tmp")
    descriptor = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL, mode)
    try:
        with os.fdopen(descriptor, "wb") as stream:
            stream.write(data)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        if temporary.exists():
            temporary.unlink()


def acquire_lock(path: Path, timeout: float = 5.0) -> Path:
    lock = path.with_suffix(path.suffix + ".lock")
    deadline = time.monotonic() + timeout
    while True:
        try:
            lock.mkdir(parents=True)
            return lock
        except FileExistsError:
            if time.monotonic() >= deadline:
                raise VerificationError(f"verification selection is locked at {lock}; inspect and remove a stale lock before retrying")
            time.sleep(0.05)


def selection_path(root: Path, input_digest: str) -> Path:
    return root / "selection" / f"{input_digest}.json"


def set_selection(root: Path, input_digest: str, value: dict[str, Any]) -> None:
    path = selection_path(root, input_digest)
    lock = acquire_lock(path)
    try:
        write_atomic(path, canonical_json(value))
    finally:
        lock.rmdir()


def load_evidence(root: Path, pointer: dict[str, Any]) -> dict[str, Any]:
    if pointer.get("state") != "complete":
        raise VerificationError(f"selected candidate evidence is {pointer.get('state', 'missing')}; run local verification")
    relative = pointer.get("record")
    if not isinstance(relative, str) or not re.fullmatch(r"evidence/[0-9a-f]{64}\.json", relative):
        raise VerificationError("selected evidence pointer has an invalid record path")
    path = root / relative
    raw = path.read_bytes()
    record_digest = relative.rsplit("/", 1)[1][:-5]
    if digest_bytes(raw) != record_digest or digest_bytes(raw) != pointer.get("record_digest"):
        raise VerificationError("selected evidence record digest does not match its immutable name")
    record = strict_json(raw, str(path))
    validate_record_shape(record)
    if record.get("record_type") != "production" or record.get("schema_version") != 1 or record.get("state") != "complete":
        raise VerificationError("selected record is not complete production evidence")
    return record



def validate_record_shape(record: Any) -> None:
    if not isinstance(record, dict):
        raise VerificationError("evidence record must be a JSON object")
    for key in ("inputs", "candidate", "suite"):
        if not isinstance(record.get(key), dict):
            raise VerificationError(f"evidence record has malformed {key}")
    if not isinstance(record.get("tested_source_commit"), str) or not OID.fullmatch(record["tested_source_commit"]):
        raise VerificationError("evidence record has an invalid tested source")
    lanes = record.get("lanes")
    if not isinstance(lanes, list) or any(not isinstance(lane, dict) or not isinstance(lane.get("id"), str) for lane in lanes):
        raise VerificationError("evidence record has malformed lanes")
    if len({lane["id"] for lane in lanes}) != len(lanes):
        raise VerificationError("evidence record has duplicate lanes")
    for lane in lanes:
        capabilities = lane.get("capabilities")
        if not isinstance(capabilities, list) or any(not isinstance(value, str) for value in capabilities):
            raise VerificationError("evidence lane has malformed capabilities")
        if not isinstance(lane.get("cases"), list) or any(not isinstance(case, dict) for case in lane["cases"]):
            raise VerificationError("evidence lane has malformed cases")
    candidate = record["candidate"]
    if not isinstance(candidate.get("previous_pair"), dict):
        raise VerificationError("evidence candidate has a malformed previous pair")


def verify_observations(root: Path, record: dict[str, Any]) -> None:
    for lane in record.get("lanes", []):
        for case in lane.get("cases", []):
            observation = case.get("observation")
            if not isinstance(observation, dict) or set(observation) != {"path", "sha256"} or not HEX.fullmatch(str(observation["sha256"])):
                raise VerificationError(f"case {case.get('id')} has no immutable observation reference")
            relative = observation["path"]
            if not isinstance(relative, str) or not re.fullmatch(r"observations/[0-9a-f]{64}", relative):
                raise VerificationError(f"case {case.get('id')} has an invalid observation path")
            data = (root / relative).read_bytes()
            if digest_bytes(data) != observation["sha256"]:
                raise VerificationError(f"case {case.get('id')} observation digest changed")


def validate_runner(lane_id: str, runner: Any) -> None:
    if not isinstance(runner, dict):
        raise VerificationError(f"lane {lane_id} has no runner identity")
    required = {"os", "architecture", "service_manager", "runner_id"}
    if not required <= set(runner) or any(not isinstance(runner[key], str) or not runner[key] for key in required):
        raise VerificationError(f"lane {lane_id} has incomplete runner identity")
    expected = {
        "linux-x86_64": ("linux", "x86_64", "systemd"),
        "linux-arm64": ("linux", "aarch64", "systemd"),
        "macos-arm64": ("macos", "arm64", "launchd"),
    }
    if lane_id in expected and tuple(runner[key] for key in ("os", "architecture", "service_manager")) != expected[lane_id]:
        raise VerificationError(f"lane {lane_id} runner does not match its required operating system, architecture, and service manager")
    if lane_id.startswith("wsl2-"):
        suffix = "x86_64" if lane_id.endswith("x86_64") else "aarch64"
        if runner["os"] != "windows" or runner.get("wsl_architecture") != suffix or runner.get("native_wsl_dispatch") is not True:
            raise VerificationError(f"lane {lane_id} lacks native PowerShell and actual wsl.exe dispatch identity")


def validate_lane_results(coverage: dict[str, Any], lane_id: str, result: Any) -> dict[str, Any]:
    if not isinstance(result, dict) or result.get("lane") != lane_id:
        raise VerificationError(f"adapter returned the wrong lane for {lane_id}")
    lane = next(item for item in coverage["lanes"] if item["id"] == lane_id)
    validate_runner(lane_id, result.get("runner"))
    capabilities = result.get("capabilities")
    if not isinstance(capabilities, list) or len(capabilities) != len(set(capabilities)) or not set(lane["required_capabilities"]) <= set(capabilities):
        raise VerificationError(f"lane {lane_id} does not prove every required capability")
    ids, unresolved = required_cases(coverage, lane_id)
    if unresolved:
        raise VerificationError(f"lane {lane_id} conditional cases have unresolved applicability: {', '.join(unresolved)}")
    cases = result.get("cases")
    if not isinstance(cases, list):
        raise VerificationError(f"lane {lane_id} adapter returned no case list")
    by_id: dict[str, dict[str, Any]] = {}
    for case in cases:
        if not isinstance(case, dict) or not isinstance(case.get("id"), str) or case["id"] in by_id:
            raise VerificationError(f"lane {lane_id} has malformed or duplicate case results")
        by_id[case["id"]] = case
    if set(by_id) != set(ids):
        raise VerificationError(f"lane {lane_id} case set differs from required coverage; missing={sorted(set(ids)-set(by_id))}, extra={sorted(set(by_id)-set(ids))}")
    for case_id in ids:
        case = by_id[case_id]
        if case.get("result") != "passed" or not isinstance(case.get("observation"), dict):
            raise VerificationError(f"lane {lane_id} case {case_id} did not pass with an observation")
        if set(case["observation"]) != {"path", "sha256"}:
            raise VerificationError(f"lane {lane_id} case {case_id} has a malformed observation")
    return {"id": lane_id, "environment": lane["environment"], "runner": result["runner"], "capabilities": sorted(capabilities), "cases": [by_id[case_id] for case_id in ids]}


def runner_command(value: str, source_dir: Path, lane_id: str) -> list[str]:
    expected = ADAPTER_PATHS[lane_id]
    if value != expected:
        raise VerificationError(f"adapter for {lane_id} must be the committed maintained entrypoint {expected}")
    path = source_dir / expected
    if path.is_symlink() or not path.is_file() or source_dir.resolve() not in path.resolve().parents:
        raise VerificationError(f"real platform adapter is unavailable at {expected}")
    if path.suffix == ".py":
        return [sys.executable, str(path)]
    if path.suffix == ".ps1":
        powershell = shutil.which("powershell.exe") or shutil.which("powershell") or shutil.which("pwsh")
        if not powershell:
            raise VerificationError("PowerShell is unavailable for the native WSL dispatch adapter")
        return [powershell, "-NoProfile", "-ExecutionPolicy", "Bypass", "-File", str(path)]
    if path.suffix == ".sh" and os.access(path, os.X_OK):
        return [str(path)]
    raise VerificationError(f"adapter {expected} has no supported direct runner")


def extract_source(repo: Path, source: str, destination: Path) -> None:
    archive = git(repo, "archive", "--format=tar", source).stdout
    with tarfile.open(fileobj=__import__("io").BytesIO(archive), mode="r:") as bundle:
        root = destination.resolve()
        members = bundle.getmembers()
        for member in members:
            target = (destination / member.name).resolve()
            if target != root and root not in target.parents:
                raise VerificationError("Git archive contains a path outside the disposable source directory")
        bundle.extractall(destination)


def copy_observations(root: Path, lanes: list[dict[str, Any]]) -> list[dict[str, Any]]:
    output = []
    for lane in lanes:
        cases = []
        for case in lane["cases"]:
            source = Path(case["observation"]["path"]).expanduser().resolve(strict=True)
            if not source.is_file() or source.is_symlink():
                raise VerificationError(f"case {case['id']} observation is not a regular file")
            data = source.read_bytes()
            digest = digest_bytes(data)
            if digest != case["observation"]["sha256"]:
                raise VerificationError(f"case {case['id']} observation digest mismatch")
            relative = f"observations/{digest}"
            target = root / relative
            if not target.exists():
                write_atomic(target, data, 0o400)
            cases.append({**case, "observation": {"path": relative, "sha256": digest}})
        output.append({**lane, "cases": cases})
    return output


def policy_health(coverage: dict[str, Any]) -> list[str]:
    problems = []
    if coverage.get("status") != "ready":
        problems.append(f"coverage registry status is {coverage.get('status')!r}, not 'ready'")
    floors = coverage.get("support_floors", {})
    if floors.get("status") != "resolved" or not isinstance(floors.get("values"), dict) or not floors["values"]:
        problems.append("supported OS, WSL, and Nix version floors are unresolved")
    if coverage.get("conditional_cases") and not all(conditional_applicability(coverage, lane["id"]) is not None for lane in coverage["lanes"] if lane["required"]):
        problems.append("conditional Nix-variant applicability cannot be resolved from the supported matrix")
    return problems


def plan_data(coverage: dict[str, Any], adapters: dict[str, str]) -> dict[str, Any]:
    lanes = []
    for lane in coverage["lanes"]:
        if lane["required"]:
            cases, unresolved = required_cases(coverage, lane["id"])
            lanes.append({"id": lane["id"], "environment": lane["environment"], "required_capabilities": lane["required_capabilities"], "required_cases": cases, "unresolved_conditional_cases": unresolved, "adapter": adapters.get(lane["id"], "unavailable")})
    return {"suite_id": coverage["suite_id"], "registry_status": coverage["status"], "lanes": lanes}


def run_verification(repo: Path, source_arg: str, candidate_path: Path, policy_dir: Path, adapters: dict[str, str]) -> int:
    source = object_commit(repo, source_arg)
    validate_target_policy(repo, source, policy_dir)
    inputs, coverage, policy_digest = read_policy(policy_dir)
    identity, _ = tree_inputs(repo, source, policy_dir)
    candidate = load_candidate(candidate_path.resolve(), repo, source)
    suite = suite_identity(repo, source, policy_dir, coverage, policy_digest)
    root = store_root(repo)
    pointer_path = selection_path(root, identity.as_json()["digest"])
    set_selection(root, identity.as_json()["digest"], {
        "schema_version": 1, "state": "running", "candidate_digest": candidate.as_json()["digest"], "suite_digest": suite.as_json()["digest"],
    })
    health = policy_health(coverage)
    missing = sorted(EXPECTED_LANES - set(adapters))
    if health or missing:
        set_selection(root, identity.as_json()["digest"], {
            "schema_version": 1, "state": "incomplete", "candidate_digest": candidate.as_json()["digest"], "suite_digest": suite.as_json()["digest"],
            "reasons": [*health, *(f"no real platform adapter is configured for {lane}" for lane in missing)],
        })
        for problem in health:
            print(f"INCOMPLETE: {problem}", file=sys.stderr)
        for lane in missing:
            print(f"UNAVAILABLE: no real platform adapter for {lane}", file=sys.stderr)
        print("No production success record was written.", file=sys.stderr)
        return 2
    with tempfile.TemporaryDirectory(prefix="installer-verify-source-") as temporary:
        source_dir = Path(temporary) / "source"
        source_dir.mkdir()
        extract_source(repo, source, source_dir)
        raw_lanes = []
        for lane_id in sorted(EXPECTED_LANES):
            prefix = runner_command(adapters[lane_id], source_dir, lane_id)
            case_ids, unresolved = required_cases(coverage, lane_id)
            if unresolved:
                raise VerificationError(f"conditional cases unresolved for {lane_id}: {', '.join(unresolved)}")
            command = [*prefix, "--source", str(source_dir), "--candidate", str(candidate_path.resolve()), "--lane", lane_id, "--required-cases", json.dumps(case_ids)]
            result = subprocess.run(command, cwd=source_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, timeout=60 * 60)
            if result.returncode:
                raise VerificationError(f"adapter for {lane_id} failed with exit {result.returncode}; inspect the sanitized adapter report")
            raw_lanes.append(validate_lane_results(coverage, lane_id, strict_json(result.stdout.encode(), f"{lane_id} adapter result")))
    verified_lanes = copy_observations(root, raw_lanes)
    record = {
        "schema_version": 1, "record_type": "production", "state": "complete", "tested_source_commit": source,
        "inputs": identity.as_json(), "candidate": candidate.as_json(), "suite": suite.as_json(), "lanes": verified_lanes,
    }
    raw = canonical_json(record)
    record_digest = digest_bytes(raw)
    evidence_path = root / "evidence" / f"{record_digest}.json"
    if evidence_path.exists():
        if evidence_path.read_bytes() != raw:
            raise VerificationError("immutable evidence path already contains different bytes")
    else:
        write_atomic(evidence_path, raw, 0o400)
    verify_observations(root, record)
    current = strict_json(pointer_path.read_bytes(), str(pointer_path)) if pointer_path.exists() else {}
    if current.get("state") != "running" or current.get("candidate_digest") != candidate.as_json()["digest"] or current.get("suite_digest") != suite.as_json()["digest"]:
        print("Evidence completed, but another explicit candidate selection replaced it before completion.", file=sys.stderr)
        return 2
    set_selection(root, identity.as_json()["digest"], {
        "schema_version": 1, "state": "complete", "candidate_digest": candidate.as_json()["digest"], "suite_digest": suite.as_json()["digest"],
        "record": f"evidence/{record_digest}.json", "record_digest": record_digest,
    })
    print(json.dumps({"state": "complete", "record": str(evidence_path), "record_digest": record_digest}, sort_keys=True))
    return 0


def verify_record(repo: Path, target: str, identity: InputIdentity, suite: SuiteIdentity, coverage: dict[str, Any], root: Path, policy_dir: Path) -> None:
    pointer_file = selection_path(root, identity.as_json()["digest"])
    if not pointer_file.exists():
        raise VerificationError(f"no selected evidence for relevant inputs; run python3 installer/verification/verify.py run --source {target} --candidate <exact-candidate-manifest.json>")
    pointer = strict_json(pointer_file.read_bytes(), str(pointer_file))
    if not isinstance(pointer, dict):
        raise VerificationError("selected evidence pointer must be a JSON object")
    if pointer.get("suite_digest") != suite.as_json()["digest"]:
        raise VerificationError(f"selected candidate was tested with a stale suite; run python3 installer/verification/verify.py run --source {target} --candidate <exact-candidate-manifest.json>")
    record = load_evidence(root, pointer)
    if record.get("tested_source_commit") != record.get("candidate", {}).get("source_commit"):
        raise VerificationError("evidence source and candidate source differ")
    if record.get("inputs") != identity.as_json() or record.get("candidate", {}).get("digest") != pointer.get("candidate_digest") or record.get("suite") != suite.as_json():
        raise VerificationError("selected evidence does not match the pushed input, candidate, and suite identities")
    if not is_ancestor(repo, record["tested_source_commit"], target):
        raise VerificationError("tested source is not an ancestor of the pushed target")
    tested_identity = input_identity(repo, record["tested_source_commit"], policy_dir)
    if tested_identity.as_json() != identity.as_json():
        raise VerificationError("pushed installer inputs differ from the tested source")
    problems = policy_health(coverage)
    if problems:
        raise VerificationError("acceptance policy is incomplete: " + "; ".join(problems))
    by_id = {lane.get("id"): lane for lane in record.get("lanes", []) if isinstance(lane, dict)}
    if set(by_id) != EXPECTED_LANES:
        raise VerificationError("production evidence does not contain every required lane")
    for lane_id in sorted(EXPECTED_LANES):
        lane = by_id[lane_id]
        cases, unresolved = required_cases(coverage, lane_id)
        if unresolved:
            raise VerificationError(f"conditional cases remain unresolved for {lane_id}")
        if not set(next(item for item in coverage["lanes"] if item["id"] == lane_id)["required_capabilities"]) <= set(lane.get("capabilities", [])):
            raise VerificationError(f"lane {lane_id} is missing a required capability")
        results = lane.get("cases", [])
        if not isinstance(results, list) or [case.get("id") for case in results] != cases or any(case.get("result") != "passed" for case in results):
            raise VerificationError(f"lane {lane_id} has missing, duplicate, reordered, or failed required cases")
        validate_runner(lane_id, lane.get("runner"))
    verify_observations(root, record)
    candidate = record["candidate"]
    candidate_digest = candidate.get("digest")
    body = {key: value for key, value in candidate.items() if key != "digest"}
    if digest_json(body) != candidate_digest or not candidate.get("artifacts") or not candidate.get("previous_pair", {}).get("artifacts"):
        raise VerificationError("candidate identity is incomplete or malformed")
    for group in (candidate["artifacts"], candidate["previous_pair"]["artifacts"]):
        if not isinstance(group, list) or not group or any(not isinstance(item, dict) or not HEX.fullmatch(str(item.get("digest", ""))) or not isinstance(item.get("size"), int) or item["size"] < 0 for item in group):
            raise VerificationError("candidate pair does not bind exact artifact identities")


def check_push(repo: Path, remote: str, input_data: bytes, policy_dir: Path) -> int:
    updates = []
    for number, line in enumerate(input_data.decode(errors="strict").splitlines(), 1):
        parts = line.split()
        if len(parts) != 4:
            raise VerificationError(f"pre-push update line {number} must contain exactly four fields")
        local_ref, local_oid, remote_ref, remote_oid = parts
        if not OID.fullmatch(local_oid) or not OID.fullmatch(remote_oid):
            raise VerificationError(f"pre-push update line {number} contains an invalid object id")
        updates.append((local_ref, local_oid, remote_ref, remote_oid))
    if not updates:
        print("No refs are being pushed.")
        return 0
    advertised: list[tuple[str, str]] | None = None
    for local_ref, local_oid, remote_ref, remote_oid in updates:
        if ZERO_OID.fullmatch(local_oid):
            print(f"SKIP {remote_ref}: ref deletion")
            continue
        target = object_commit(repo, local_oid)
        identity, entries = tree_inputs(repo, target, policy_dir)
        relevant = False
        if ZERO_OID.fullmatch(remote_oid):
            if advertised is None:
                advertised = destination_baselines(repo, remote)
            baselines = [commit for commit, _ in advertised]
            incoming = incoming_commits(repo, target, baselines)
            for commit in incoming:
                parents = git_text(repo, "rev-list", "--parents", "-n", "1", commit).split()[1:]
                _, child = tree_inputs(repo, commit, policy_dir)
                if not parents:
                    relevant = relevant or bool(child)
                for parent_oid in parents:
                    _, parent = tree_inputs(repo, object_commit(repo, parent_oid), policy_dir)
                    relevant = relevant or bool(delta_families(parent, child))
        else:
            try:
                base = object_commit(repo, remote_oid)
            except VerificationError as error:
                raise VerificationError(f"remote context for {remote_ref} is unavailable locally; run git fetch <remote> {shlex.quote(remote_ref)} and retry") from error
            incoming = incoming_commits(repo, target, [base])
            for commit in incoming:
                parents = git_text(repo, "rev-list", "--parents", "-n", "1", commit).split()[1:]
                _, child = tree_inputs(repo, commit, policy_dir)
                if not parents:
                    relevant = relevant or bool(child)
                for parent_oid in parents:
                    _, parent = tree_inputs(repo, object_commit(repo, parent_oid), policy_dir)
                    relevant = relevant or bool(delta_families(parent, child))
            _, base_entries = tree_inputs(repo, base, policy_dir)
            relevant = relevant or bool(delta_families(base_entries, entries))
        if not relevant:
            print(f"PASS {remote_ref}: no installer inputs changed")
            continue
        validate_target_policy(repo, target, policy_dir)
        inputs, coverage, policy_digest = read_policy(policy_dir)
        suite = suite_identity(repo, target, policy_dir, coverage, policy_digest)
        verify_record(repo, target, identity, suite, coverage, store_root(repo), policy_dir)
        print(f"PASS {remote_ref}: complete local evidence matches {identity.as_json()['digest']}")
    return 0


def worktree_paths(repo: Path) -> list[Path]:
    lines = git_text(repo, "worktree", "list", "--porcelain").splitlines()
    paths = []
    for line in lines:
        if line.startswith("worktree "):
            path = Path(line[9:]).resolve()
            if path.exists():
                paths.append(path)
    return paths


def gitdir_for(repo: Path) -> str:
    return str(Path(git_text(repo, "rev-parse", "--absolute-git-dir")).resolve())


def config_value(repo: Path, key: str, *scope: str) -> str | None:
    result = git(repo, "config", *scope, "--get", key, check=False)
    return result.stdout.decode().strip() if result.returncode == 0 else None


def policy_snapshot_digest(snapshot: Path) -> str:
    files = {}
    for name in ("verify.py", "inputs.json", "coverage.json"):
        path = snapshot / name
        if not path.is_file() or path.is_symlink():
            raise VerificationError(f"installed policy snapshot is missing {name}")
        files[name] = digest_bytes(path.read_bytes())
    return digest_json(files)


def hook_policy_state(repo: Path, hook_name: str | None = None) -> tuple[Path, dict[str, Any]]:
    root = store_root(repo)
    expected = config_value(repo, "installer-verification.snapshot-sha256", "--local")
    expected_state = config_value(repo, "installer-verification.state-sha256", "--local")
    hooks_path = config_value(repo, "installer-verification.hooks-path", "--local")
    snapshot = root / "snapshots" / str(expected)
    state_path = root / "ownership" / f"{expected_state}.json"
    if not expected or policy_snapshot_digest(snapshot) != expected:
        raise VerificationError("installed verifier snapshot failed its immutable digest check; run setup-hooks from the intended source checkout")
    state_bytes = state_path.read_bytes()
    if not expected_state or digest_bytes(state_bytes) != expected_state:
        raise VerificationError("installed hook ownership state failed its digest check; refusing to replace or bypass it")
    state = strict_json(state_bytes, str(state_path))
    if state.get("schema_version") != 1 or not isinstance(state.get("prior_hooks"), dict) or not isinstance(state.get("wrapper_digests"), dict):
        raise VerificationError("installed hook ownership state has an unsupported schema")
    if hook_name is not None:
        current = Path(git_text(repo, "rev-parse", "--path-format=absolute", "--git-path", f"hooks/{hook_name}")).resolve()
        wrapper = root / str(hooks_path) / hook_name if hooks_path else current
        expected_wrapper = state["wrapper_digests"].get(hook_name)
        if current != wrapper.resolve() or not expected_wrapper or digest_bytes(current.read_bytes()) != expected_wrapper:
            raise VerificationError(f"installed {hook_name} wrapper failed its ownership check")
    else:
        for name, expected_wrapper in state["wrapper_digests"].items():
            path = root / str(hooks_path) / name
            if not path.is_file() or digest_bytes(path.read_bytes()) != expected_wrapper:
                raise VerificationError(f"installed {name} wrapper failed its ownership check")
    return snapshot, state


STANDARD_HOOKS = {
    "applypatch-msg", "pre-applypatch", "post-applypatch", "pre-commit", "prepare-commit-msg", "commit-msg", "post-commit",
    "pre-rebase", "post-checkout", "post-merge", "pre-push", "pre-receive", "update", "post-update", "push-to-checkout",
    "post-rewrite", "sendemail-validate", "fsmonitor-watchman", "p4-changelist", "p4-prepare-changelist", "p4-post-changelist",
    "p4-pre-submit", "post-index-change", "reference-transaction", "proc-receive", "pre-auto-gc", "post-receive",
}


def all_config_values(repo: Path, key: str, scope: str) -> list[str]:
    result = git(repo, "config", scope, "--get-all", key, check=False)
    return result.stdout.decode().splitlines() if result.returncode == 0 else []


def restore_config_values(repo: Path, key: str, scope: str, values: list[str]) -> None:
    git(repo, "config", scope, "--unset-all", key, check=False)
    for value in values:
        git(repo, "config", scope, "--add", key, value)


def effective_hook_path(repo: Path, name: str) -> Path:
    directory = Path(git_text(repo, "rev-parse", "--git-path", "hooks"))
    if not directory.is_absolute():
        directory = repo / directory
    return directory.absolute() / name


def previous_hook_state(repo: Path, worktrees: list[Path]) -> dict[str, Any]:
    names = set(STANDARD_HOOKS)
    for worktree in worktrees:
        directory = effective_hook_path(worktree, "pre-push").parent
        if directory.is_dir():
            names.update(path.name for path in directory.iterdir() if path.is_file() and os.access(path, os.X_OK))
    prior = {}
    for worktree in worktrees:
        hooks = {}
        for name in sorted(names):
            path = effective_hook_path(worktree, name)
            hooks[name] = {"path": str(path), "enabled": path.is_file() and os.access(path, os.X_OK)}
        prior[gitdir_for(worktree)] = {
            "hooks": hooks,
            "configured_value": config_value(worktree, "core.hooksPath"),
            "origin": git_text(worktree, "config", "--show-origin", "--get", "core.hooksPath", check=False),
        }
    default_dir = effective_hook_path(repo, "pre-push").parent
    return {"prior_hooks": prior, "default_hooks_dir": str(default_dir), "hook_names": sorted(names)}


def install_hook_snapshot(repo: Path) -> None:
    for name in ("verify.py", "inputs.json", "coverage.json"):
        committed = git(repo, "show", f"HEAD:installer/verification/{name}", check=False)
        if committed.returncode or digest_bytes(committed.stdout) != digest_bytes((POLICY_DIR / name).read_bytes()):
            raise VerificationError(f"setup-hooks requires committed {name} bytes that match this checkout")
    read_policy(POLICY_DIR)
    common = common_dir(repo)
    lock = acquire_lock(common / ".installer-verification-setup")
    try:
        _install_hook_snapshot_locked(repo, common / "installer-verification")
    finally:
        lock.rmdir()


def _install_hook_snapshot_locked(repo: Path, root: Path) -> None:
    common = common_dir(repo)
    configured_snapshot = config_value(repo, "installer-verification.snapshot-sha256", "--local")
    worktrees = worktree_paths(repo)
    if configured_snapshot:
        _, old_state = hook_policy_state(repo)
        state_base = {key: value for key, value in old_state.items() if key != "wrapper_digests"}
        expected_hooks = str((root / "hooks").resolve())
        if any(config_value(worktree, "core.hooksPath") != expected_hooks for worktree in worktrees):
            raise VerificationError("core.hooksPath changed after installer verification setup; inspect it before retrying")
    else:
        if root.exists() and any(root.iterdir()):
            raise VerificationError(f"{root} already exists without an installer-verification ownership record")
        state_base = {"schema_version": 1, **previous_hook_state(repo, worktrees)}
    snapshot_digest = digest_json({name: digest_bytes((POLICY_DIR / name).read_bytes()) for name in ("verify.py", "inputs.json", "coverage.json")})
    snapshots = root / "snapshots"
    snapshot = snapshots / snapshot_digest
    root.mkdir(parents=True, exist_ok=True)
    if not snapshot.exists():
        snapshots.mkdir(parents=True, exist_ok=True)
        temporary = Path(tempfile.mkdtemp(prefix=".snapshot-", dir=snapshots))
        for name in ("verify.py", "inputs.json", "coverage.json"):
            path = temporary / name
            path.write_bytes((POLICY_DIR / name).read_bytes())
            os.chmod(path, 0o444)
        os.replace(temporary, snapshot)
    if policy_snapshot_digest(snapshot) != snapshot_digest:
        raise VerificationError("content-addressed policy snapshot has conflicting bytes")
    old_names = set(state_base.get("hook_names", []))
    names = old_names | STANDARD_HOOKS
    hooks_dir = root / "hooks"
    hooks_dir.mkdir(parents=True, exist_ok=True)
    wrappers = {
        name: ("#!/bin/sh\nexec python3 " + shlex.quote(str(snapshot / "verify.py")) + " hook " + shlex.quote(name) + " \"$@\"\n").encode()
        for name in sorted(names)
    }
    state = {**state_base, "hook_names": sorted(names), "wrapper_digests": {name: digest_bytes(data) for name, data in wrappers.items()}}
    state_bytes = canonical_json(state)
    state_digest = digest_bytes(state_bytes)
    ownership = root / "ownership"
    ownership.mkdir(parents=True, exist_ok=True)
    state_path = ownership / f"{state_digest}.json"
    if not state_path.exists():
        write_atomic(state_path, state_bytes, 0o400)
    if state_path.read_bytes() != state_bytes:
        raise VerificationError("content-addressed hook ownership state has conflicting bytes")
    for name, data in wrappers.items():
        write_atomic(hooks_dir / name, data, 0o555)
        os.chmod(hooks_dir / name, 0o555)
    target_path = str(hooks_dir.resolve())
    extension = config_value(repo, "extensions.worktreeConfig", "--bool") == "true"
    old_local = all_config_values(repo, "core.hooksPath", "--local")
    old_worktree = {str(worktree): all_config_values(worktree, "core.hooksPath", "--worktree") for worktree in worktrees} if extension else {}
    new_install = not configured_snapshot
    try:
        if extension:
            git(repo, "config", "--local", "--replace-all", "core.hooksPath", target_path)
            for worktree in worktrees:
                git(worktree, "config", "--worktree", "--replace-all", "core.hooksPath", target_path)
        else:
            git(repo, "config", "--local", "--replace-all", "core.hooksPath", target_path)
        for worktree in worktrees:
            if effective_hook_path(worktree, "pre-push") != hooks_dir / "pre-push":
                raise VerificationError(f"could not install hook for worktree {worktree}")
        for key, value in (("snapshot-sha256", snapshot_digest), ("state-sha256", state_digest), ("hooks-path", "hooks")):
            git(repo, "config", "--local", "--replace-all", f"installer-verification.{key}", value)
    except Exception:
        if extension:
            for worktree in worktrees:
                restore_config_values(worktree, "core.hooksPath", "--worktree", old_worktree[str(worktree)])
        restore_config_values(repo, "core.hooksPath", "--local", old_local)
        if new_install:
            for key in ("snapshot-sha256", "state-sha256", "hooks-path"):
                git(repo, "config", "--local", "--unset-all", f"installer-verification.{key}", check=False)
            shutil.rmtree(root, ignore_errors=True)
        raise
    print(json.dumps({"state": "installed", "hook": str(hooks_dir / "pre-push"), "snapshot_sha256": snapshot_digest, "worktrees": len(worktrees), "wrappers": len(wrappers)}, sort_keys=True))


def run_prior_hook(repo: Path, state: dict[str, Any], name: str, args: list[str], stdin_data: bytes) -> int:
    worktree = state["prior_hooks"].get(gitdir_for(repo), {})
    prior = worktree.get("hooks", {}).get(name)
    if prior is None:
        path = Path(state["default_hooks_dir"]) / name
        enabled = path.is_file() and os.access(path, os.X_OK)
    else:
        path = Path(prior["path"])
        enabled = path.is_file() and os.access(path, os.X_OK)
    if not enabled or not path.is_file() or not os.access(path, os.X_OK):
        return 0
    return subprocess.run([str(path), *args], cwd=repo, input=stdin_data).returncode


def parse_adapters(values: list[str]) -> dict[str, str]:
    adapters = {}
    for value in values:
        lane, separator, command = value.partition("=")
        if not separator or lane not in EXPECTED_LANES or not command.strip() or lane in adapters:
            raise VerificationError(f"invalid or duplicate --adapter {value!r}; use lane=command")
        adapters[lane] = command
    return adapters


def command_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="installer-verify")
    commands = parser.add_subparsers(dest="command", required=True, metavar="{classify,plan,run,check-push,setup-hooks,self-test}")
    classify = commands.add_parser("classify")
    classify.add_argument("--repo", type=Path, default=ROOT)
    classify.add_argument("--base", required=True)
    classify.add_argument("--target", required=True)
    plan = commands.add_parser("plan")
    plan.add_argument("--repo", type=Path, default=ROOT)
    plan.add_argument("--adapter", action="append", default=[])
    run = commands.add_parser("run")
    run.add_argument("--repo", type=Path, default=ROOT)
    run.add_argument("--source", required=True)
    run.add_argument("--candidate", type=Path, required=True)
    run.add_argument("--adapter", action="append", default=[])
    check = commands.add_parser("check-push")
    check.add_argument("--repo", type=Path, default=ROOT)
    check.add_argument("--remote", required=True)
    setup = commands.add_parser("setup-hooks")
    setup.add_argument("--repo", type=Path, default=ROOT)
    hook = commands.add_parser("hook")
    hook.add_argument("hook_name")
    hook.add_argument("hook_args", nargs=argparse.REMAINDER)
    hook.add_argument("--repo", type=Path, default=Path.cwd())
    commands.add_parser("self-test")
    return parser


def run_self_tests() -> int:
    import unittest

    suite = unittest.defaultTestLoader.discover(str(POLICY_DIR / "tests"), pattern="test_*.py")
    result = unittest.TextTestRunner(verbosity=2).run(suite)
    return 0 if result.wasSuccessful() else 1


def main(argv: list[str] | None = None) -> int:
    args = command_parser().parse_args(argv)
    try:
        if args.command == "self-test":
            return run_self_tests()
        repo = getattr(args, "repo", Path.cwd()).resolve()
        if args.command == "classify":
            base = object_commit(repo, args.base)
            target = object_commit(repo, args.target)
            before, left = tree_inputs(repo, base, POLICY_DIR)
            after, right = tree_inputs(repo, target, POLICY_DIR)
            print(json.dumps({"base": base, "target": target, "families": delta_families(left, right), "base_inputs": before.as_json(), "target_inputs": after.as_json()}, sort_keys=True, indent=2))
            return 0
        if args.command == "plan":
            _, coverage, _ = read_policy(POLICY_DIR)
            adapters = parse_adapters(args.adapter)
            plan = plan_data(coverage, adapters)
            problems = policy_health(coverage)
            missing = sorted(EXPECTED_LANES - set(adapters))
            print(json.dumps({"plan": plan, "incomplete": [*problems, *(f"no real adapter configured for {lane}" for lane in missing)]}, sort_keys=True, indent=2))
            return 2 if problems or missing else 0
        if args.command == "run":
            return run_verification(repo, args.source, args.candidate.resolve(), POLICY_DIR, parse_adapters(args.adapter))
        if args.command == "check-push":
            return check_push(repo, args.remote, sys.stdin.buffer.read(), POLICY_DIR)
        if args.command == "setup-hooks":
            install_hook_snapshot(repo)
            return 0
        if args.command == "hook":
            lock = acquire_lock(common_dir(repo) / ".installer-verification-setup")
            try:
                snapshot, state = hook_policy_state(repo, args.hook_name)
                if POLICY_DIR != snapshot:
                    raise VerificationError("hook policy changed while this invocation started; retry the Git operation")
            finally:
                lock.rmdir()
            stdin_data = sys.stdin.buffer.read()
            prior_status = run_prior_hook(repo, state, args.hook_name, args.hook_args, stdin_data)
            if prior_status or args.hook_name != "pre-push":
                return prior_status
            if len(args.hook_args) != 2:
                raise VerificationError("Git pre-push hook did not provide its remote name and location")
            lock = acquire_lock(common_dir(repo) / ".installer-verification-setup")
            try:
                current_snapshot, _ = hook_policy_state(repo, args.hook_name)
                if current_snapshot != snapshot:
                    raise VerificationError("hook policy changed during the prior hook; retry the push")
                return check_push(repo, args.hook_args[1], stdin_data, snapshot)
            finally:
                lock.rmdir()
    except (VerificationError, OSError, subprocess.TimeoutExpired) as error:
        print(f"installer verification failed: {error}", file=sys.stderr)
        return 2
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
