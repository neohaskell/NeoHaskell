"""Validate local installer candidate identity and source bindings.

Parsed and source-bound results do not prove compiler execution, cache signatures,
Nix substitution, or platform acceptance.
"""
from dataclasses import dataclass
import base64
import hashlib
import json
import os
from pathlib import Path
import re
import stat
import subprocess
from typing import Any, Dict, List, Optional, Tuple
import weakref

import engine as release
import github as remote


SKILL_ROOT = Path(__file__).resolve().parents[2]
CONTRACT_PATH = SKILL_ROOT / "references/installer-contract.json"
CONTRACT_RELATIVE_PATH = ".agents/skills/neohaskell-release/references/installer-contract.json"
CANDIDATE_RELATIVE_PATH = ".agents/skills/neohaskell-release/scripts/releases/candidate.py"
ENGINE_RELATIVE_PATH = ".agents/skills/neohaskell-release/scripts/releases/engine.py"
GITHUB_RELATIVE_PATH = ".agents/skills/neohaskell-release/scripts/releases/github.py"
CONFIG_RELATIVE_PATH = ".agents/skills/neohaskell-release/references/config.json"
SHA256 = re.compile(r"[0-9a-f]{64}\Z")
COMMIT = re.compile(r"[0-9a-f]{40}\Z")
SAFE_NAME = re.compile(r"[A-Za-z0-9][A-Za-z0-9._+-]{0,511}\Z")
NIX_STORE_PATH = re.compile(
    r"/nix/store/([0123456789abcdfghijklmnpqrsvwxyz]{32})-([A-Za-z0-9+._?=-]{1,211})\Z"
)
_SOURCE_BOUND_CANDIDATES = weakref.WeakSet()


@dataclass(frozen=True)
class ParsedCandidate:
    candidate_id: str
    core_bytes: bytes
    artifact_directory: Path
    version: str
    context_kind: str

    @property
    def core(self) -> Dict[str, Any]:
        return json.loads(self.core_bytes.decode("utf-8"))


@dataclass(frozen=True, eq=False)
class SourceBoundCandidate:
    candidate_id: str
    change_source: str
    build_revision: str
    source_tree_sha256: str
    build_tree_sha256: str
    version: str
    context_kind: str
    artifact_directory: Path


@dataclass(frozen=True)
class ParsedSelection:
    selection_id: str
    current_candidate_id: str
    previous_candidate_id: str
    current_version: str
    previous_version: str


def _decode_error(message: str) -> None:
    raise ValueError(message)


def _parse_integer(text: str) -> int:
    if len(text.lstrip("-")) > 19:
        _decode_error("JSON integer exceeds the signed 64-bit range")
    value = int(text)
    if value < -(1 << 63) or value > (1 << 63) - 1:
        _decode_error("JSON integer exceeds the signed 64-bit range")
    return value


def _reject_float(_text: str) -> None:
    _decode_error("Floating point values are not part of the candidate contract")


def _reject_constant(_text: str) -> None:
    _decode_error("Non-finite JSON values are not allowed")


def _pairs_without_duplicates(pairs: List[Tuple[str, Any]]) -> Dict[str, Any]:
    value: Dict[str, Any] = {}
    for key, item in pairs:
        if key in value:
            _decode_error("Duplicate JSON object key")
        value[key] = item
    return value


def _walk_json(value: Any, max_depth: int, max_nodes: int, reject_booleans: bool) -> None:
    stack = [(value, 0)]
    visited = 0
    while stack:
        item, depth = stack.pop()
        visited += 1
        if visited > max_nodes:
            _decode_error("JSON value count exceeds the candidate limit")
        if depth > max_depth:
            _decode_error("JSON nesting exceeds the candidate limit")
        if isinstance(item, bool):
            if reject_booleans:
                _decode_error("Boolean values are not part of CandidateCore")
        elif item is None or isinstance(item, str):
            if isinstance(item, str):
                try:
                    item.encode("utf-8", "strict")
                except UnicodeEncodeError as error:
                    raise ValueError("JSON contains an invalid Unicode scalar") from error
        elif type(item) is int:
            if item < -(1 << 63) or item > (1 << 63) - 1:
                _decode_error("JSON integer exceeds the signed 64-bit range")
        elif isinstance(item, float):
            _decode_error("Floating point values are not part of the candidate contract")
        elif isinstance(item, list):
            stack.extend((child, depth + 1) for child in item)
        elif isinstance(item, dict):
            for key, child in item.items():
                if not isinstance(key, str):
                    _decode_error("JSON object keys must be strings")
                try:
                    key.encode("utf-8", "strict")
                except UnicodeEncodeError as error:
                    raise ValueError("JSON contains an invalid object key") from error
                stack.append((child, depth + 1))
        else:
            _decode_error("Value cannot be represented in canonical JSON")


def _loads(raw: bytes, max_bytes: int, max_depth: int, max_nodes: int,
           reject_booleans: bool = False) -> Any:
    if not isinstance(raw, bytes):
        _decode_error("JSON input must be bytes")
    if len(raw) > max_bytes:
        _decode_error("JSON input exceeds the candidate byte limit")
    try:
        text = raw.decode("utf-8", "strict")
    except UnicodeDecodeError as error:
        raise ValueError("JSON input is not UTF-8") from error
    try:
        value = json.loads(
            text,
            object_pairs_hook=_pairs_without_duplicates,
            parse_int=_parse_integer,
            parse_float=_reject_float,
            parse_constant=_reject_constant,
        )
    except ValueError as error:
        if str(error).startswith(("Duplicate JSON", "JSON integer", "Floating point", "Non-finite")):
            raise
        raise ValueError("Malformed JSON input") from error
    except (json.JSONDecodeError, RecursionError) as error:
        raise ValueError("Malformed JSON input") from error
    _walk_json(value, max_depth, max_nodes, reject_booleans)
    return value


def canonical_json(value: Any) -> bytes:
    """Encode interoperable JSON with sorted keys, compact separators, and no newline."""
    _walk_json(value, max_depth=24, max_nodes=2500000, reject_booleans=False)
    try:
        return json.dumps(
            value,
            sort_keys=True,
            ensure_ascii=False,
            separators=(",", ":"),
            allow_nan=False,
        ).encode("utf-8", "strict")
    except (TypeError, ValueError, UnicodeEncodeError) as error:
        raise ValueError("Value cannot be encoded as canonical UTF-8 JSON") from error


def _sha256(raw: bytes) -> str:
    return hashlib.sha256(raw).hexdigest()


def _canonical_digest(value: Any) -> str:
    return _sha256(canonical_json(value))


def _read_worktree_file(root: Path, relative: str, max_bytes: int) -> bytes:
    if not isinstance(relative, str) or not relative or relative.startswith("/"):
        raise ValueError("Source input path is not relative")
    parts = relative.split("/")
    if any(part in {"", ".", ".."} or "\\" in part for part in parts):
        raise ValueError("Source input path is unsafe")
    path = root
    for part in parts:
        path = path / part
        try:
            info = path.lstat()
        except OSError as error:
            raise ValueError("Missing source-owned input: " + relative) from error
        if stat.S_ISLNK(info.st_mode):
            raise ValueError("Source-owned input contains a symlink: " + relative)
    if not stat.S_ISREG(info.st_mode) or info.st_size > max_bytes:
        raise ValueError("Source-owned input is not a bounded regular file: " + relative)
    try:
        resolved = path.resolve(strict=True)
        resolved.relative_to(root.resolve(strict=True))
        with path.open("rb") as source:
            data = source.read(max_bytes + 1)
    except (OSError, ValueError) as error:
        raise ValueError("Cannot read source-owned input: " + relative) from error
    if len(data) > max_bytes or len(data) != info.st_size:
        raise ValueError("Source-owned input changed or exceeds its byte limit: " + relative)
    return data


def _registry() -> Tuple[bytes, Dict[str, Any]]:
    try:
        raw = CONTRACT_PATH.read_bytes()
    except OSError as error:
        raise ValueError("Missing installer contract registry") from error
    value = _loads(raw, 2 * 1024 * 1024, 24, 100000)
    if not isinstance(value, dict):
        raise ValueError("Installer contract registry must be an object")
    expected = {
        "schema", "policy", "encoding", "hash_algorithms", "wire_fields",
        "roles", "auxiliary_assets", "source_inputs", "metadata_files",
        "trusted_cache_keys", "limits", "context_rules",
    }
    if set(value) != expected:
        raise ValueError("Installer contract registry fields differ from its owner")
    if type(value["schema"]) is not int or value["schema"] != 1:
        raise ValueError("Unknown installer contract registry schema")
    if value["policy"] != "installer-v1" or value["encoding"] != "canonical-utf8-json-v1":
        raise ValueError("Unknown installer candidate policy or encoding")
    _validate_registry(value)
    return raw, value


def _validate_registry(registry: Dict[str, Any]) -> None:
    algorithms = registry["hash_algorithms"]
    if not isinstance(algorithms, dict) or set(algorithms) != {
        "candidate_id", "selection_id", "source_tree", "build_tree", "recipe_set",
        "version_seed_recipe", "version_seed_diff", "closure_id",
        "receipt_identity", "release_seed_identity",
    }:
        raise ValueError("Installer registry hash algorithms are incomplete")
    expected_algorithms = {
        "candidate_id": "sha256-canonical-core-bytes-v1",
        "selection_id": "sha256-canonical-selection-bytes-v1",
        "source_tree": "sha256-git-ls-tree-recursive-z-v1",
        "build_tree": "sha256-git-ls-tree-recursive-z-overlay-v1",
        "recipe_set": "sha256-canonical-source-file-identities-v1",
        "version_seed_recipe": "sha256-canonical-source-file-identities-v1",
        "version_seed_diff": "sha256-canonical-version-edit-identities-v1",
        "closure_id": "sha256-canonical-reachable-path-identities-v1",
        "receipt_identity": "sha256-canonical-receipt-object-v1",
        "release_seed_identity": "sha256-canonical-release-seed-identity-v1",
    }
    if algorithms != expected_algorithms:
        raise ValueError("Unknown installer contract hash algorithm")
    fields = registry["wire_fields"]
    expected_field_sets = {
        "core", "engineering_context", "release_context", "committed_version_seed",
        "engineering_version_seed", "build_inputs", "starter_inputs", "file_identity",
        "native_neo_entry", "native_installer_entry", "bootstrap_entry", "locator_input",
        "package_output", "pair_output", "pair_members", "closure_table", "closure_path",
        "cache_location", "build_receipts_table", "build_receipt", "native_output_identity",
        "nix_output_identity", "pair_output_identity", "verification_observations",
        "verification_selection",
    }
    if not isinstance(fields, dict) or set(fields) != expected_field_sets:
        raise ValueError("Installer registry wire fields are incomplete")
    for name, names in fields.items():
        if not isinstance(names, list) or any(type(item) is not str for item in names):
            raise ValueError("Installer registry wire field list is malformed: " + name)
        if len(set(names)) != len(names):
            raise ValueError("Installer registry wire field list has duplicates: " + name)
    roles = registry["roles"]
    if not isinstance(roles, dict) or set(roles) != {
        "native_neo", "native_installers", "bootstraps", "nix_packages", "output_receipt_count"
    }:
        raise ValueError("Installer registry roles are incomplete")
    if roles["native_neo"] != {
        "target_policy": "native-v2",
        "resolved_by": ["engine.targets_for", "github.required_assets"],
        "expected_target_count": 2,
    }:
        raise ValueError("Unknown native Neo target owner or policy")
    installers = roles["native_installers"]
    if not isinstance(installers, dict) or set(installers) != {"platforms", "filename_prefix"}:
        raise ValueError("Native installer role definition is malformed")
    if installers["platforms"] != [
        "x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu", "aarch64-apple-darwin"
    ] or installers["filename_prefix"] != "neo-installer-":
        raise ValueError("Native installer platform contract changed")
    bootstraps = roles["bootstraps"]
    if not isinstance(bootstraps, dict) or set(bootstraps) != {"posix", "powershell"}:
        raise ValueError("Bootstrap role set is incomplete")
    for name, filename in (("posix", "install"), ("powershell", "install.ps1")):
        item = bootstraps[name]
        if not isinstance(item, dict) or set(item) != {"filename", "locator_input"}:
            raise ValueError("Bootstrap definition is malformed: " + name)
        if item["filename"] != filename or item["locator_input"] != {
            "kind": "runtime-argument", "name": "--candidate-locator"
        }:
            raise ValueError("Bootstrap must receive a runtime candidate locator")
    packages = roles["nix_packages"]
    if not isinstance(packages, dict) or set(packages) != {"systems", "outputs", "output_receipt_count"}:
        raise ValueError("Nix package role definition is malformed")
    if packages["systems"] != ["x86_64-linux", "aarch64-linux", "aarch64-darwin"]:
        raise ValueError("Nix package platform contract changed")
    if packages["outputs"] != ["neo", "neoctl", "pair"] or packages["output_receipt_count"] != 9:
        raise ValueError("Nix package output role contract changed")
    if roles["output_receipt_count"] != 14:
        raise ValueError("Candidate output receipt count changed")
    auxiliary = registry["auxiliary_assets"]
    if auxiliary != {
        "resolved_by": "github.required_assets",
        "exclude_roles": ["native_neo_files"],
        "must_match_exactly": True,
    }:
        raise ValueError("Auxiliary asset ownership changed")
    source_inputs = registry["source_inputs"]
    if not isinstance(source_inputs, dict) or set(source_inputs) != {
        "recipe", "version_seed_recipe", "build", "english_resource",
        "bootstrap_generator", "engineering_starter_framework_revision",
    }:
        raise ValueError("Source-owned candidate inputs are incomplete")
    required_recipe_inputs = {
        CONFIG_RELATIVE_PATH, CONTRACT_RELATIVE_PATH, CANDIDATE_RELATIVE_PATH,
        ENGINE_RELATIVE_PATH, GITHUB_RELATIVE_PATH, "flake.nix", "nix/neo-package.nix",
        "neo/lifecycle/src/english.json", "neo/lifecycle/scripts/generate_bootstraps.py",
    }
    recipe_paths = source_inputs["recipe"]
    if (not isinstance(recipe_paths, list) or any(type(path) is not str for path in recipe_paths)
            or len(set(recipe_paths)) != len(recipe_paths) or set(recipe_paths) != required_recipe_inputs):
        raise ValueError("Recipe input list differs from the exact source-owned owner set")
    seed_paths = source_inputs["version_seed_recipe"]
    if not isinstance(seed_paths, list) or set(seed_paths) != {
        CONFIG_RELATIVE_PATH, ENGINE_RELATIVE_PATH
    } or len(seed_paths) != 2:
        raise ValueError("Engineering version seed recipe inputs changed")
    for path in recipe_paths + seed_paths:
        _validate_source_path(path)
    if source_inputs["english_resource"] != "neo/lifecycle/src/english.json":
        raise ValueError("Bootstrap English resource path changed")
    if source_inputs["bootstrap_generator"] != "neo/lifecycle/scripts/generate_bootstraps.py":
        raise ValueError("Bootstrap generator path changed")
    if source_inputs["engineering_starter_framework_revision"] != "ea75873fbd0da7da39124c82f328c2e70bdca629":
        raise ValueError("Engineering starter framework pin changed")
    build_inputs = source_inputs["build"]
    if not isinstance(build_inputs, dict) or set(build_inputs) != {
        "flake_lock", "cargo_lock", "starter"
    }:
        raise ValueError("Build input paths are malformed")
    if build_inputs["flake_lock"] != "flake.lock" or build_inputs["cargo_lock"] != "neo/Cargo.lock":
        raise ValueError("Build lock input paths changed")
    if build_inputs["starter"] != {
        "flake": "neo/starter/flake.nix",
        "lock": "neo/starter/flake.lock",
        "cabal_project": "neo/starter/cabal.project",
    }:
        raise ValueError("Starter source paths changed")
    _validate_source_path(build_inputs["flake_lock"])
    _validate_source_path(build_inputs["cargo_lock"])
    for path in build_inputs["starter"].values():
        _validate_source_path(path)
    if registry["metadata_files"] != {
        "closures": "closures.json", "build_receipts": "build-receipts.json"
    }:
        raise ValueError("Candidate metadata filenames changed")
    trusted = registry["trusted_cache_keys"]
    if not isinstance(trusted, list) or not trusted:
        raise ValueError("Trusted cache key registry is empty")
    seen_caches = set()
    for item in trusted:
        if not isinstance(item, dict) or set(item) != {"cache", "key"}:
            raise ValueError("Trusted cache entry is malformed")
        if type(item["cache"]) is not str or type(item["key"]) is not str:
            raise ValueError("Trusted cache entry is malformed")
        if item["cache"] in seen_caches or not item["cache"].startswith("https://"):
            raise ValueError("Trusted cache registry has a duplicate or unsafe cache")
        seen_caches.add(item["cache"])
        _key_id, key_bytes = _split_key(item["key"])
        if len(key_bytes) != 32:
            raise ValueError("Trusted cache public key has the wrong length")
    limits = registry["limits"]
    expected_limit_fields = {
        "max_core_bytes", "max_table_bytes", "max_core_depth", "max_json_nodes",
        "max_name_bytes", "max_nix_store_path_bytes", "max_native_file_bytes",
        "max_bootstrap_file_bytes", "max_metadata_file_bytes", "max_closure_paths",
        "max_references_per_path", "max_closure_edges", "max_cache_locations_per_path",
        "max_signatures_per_location", "max_source_tree_entries", "max_source_tree_index_bytes",
    }
    if not isinstance(limits, dict) or set(limits) != expected_limit_fields:
        raise ValueError("Candidate parser resource bounds are incomplete")
    hard_limits = {
        "max_core_bytes": 2 * 1024 * 1024,
        "max_table_bytes": 32 * 1024 * 1024,
        "max_core_depth": 24,
        "max_json_nodes": 2500000,
        "max_name_bytes": 512,
        "max_nix_store_path_bytes": 255,
        "max_native_file_bytes": 256 * 1024 * 1024,
        "max_bootstrap_file_bytes": 2 * 1024 * 1024,
        "max_metadata_file_bytes": 32 * 1024 * 1024,
        "max_closure_paths": 100000,
        "max_references_per_path": 512,
        "max_closure_edges": 400000,
        "max_cache_locations_per_path": 2,
        "max_signatures_per_location": 2,
        "max_source_tree_entries": 200000,
        "max_source_tree_index_bytes": 64 * 1024 * 1024,
    }
    for name, maximum in hard_limits.items():
        if type(limits[name]) is not int or limits[name] <= 0 or limits[name] > maximum:
            raise ValueError("Candidate parser limit exceeds its code owner: " + name)
    if limits["max_references_per_path"] != 512 or limits["max_closure_paths"] != 100000:
        raise ValueError("Candidate closure bounds differ from the accepted contract")
    expected_context_rules = {
        "engineering_committed_build_revision_equals_change_source": True,
        "engineering_committed_build_tree_equals_source_tree": True,
        "engineering_version_seed_uses_release_engine": True,
        "engineering_version_seed_changes_only_configured_version_inputs": True,
        "engineering_version_seed_binds_actual_build_tree": True,
        "engineering_starter_framework_revision": source_inputs["engineering_starter_framework_revision"],
        "release_build_revision_is_generated_preparation": True,
        "release_requires_distribution_policy_bound_to_registry": True,
        "release_seed_identity_binds_actual_starter_files": True,
        "bootstrap_embedded_candidate_id_forbidden": True,
        "cache_signature_validation": "structural-only; Nix cryptographic verification is future",
    }
    if registry["context_rules"] != expected_context_rules:
        raise ValueError("Candidate context rules differ from the accepted contract")


def _validate_source_path(path: str) -> None:
    if type(path) is not str or not path or path.startswith("/") or "\\" in path:
        raise ValueError("Unsafe source-owned path")
    if any(part in {"", ".", ".."} for part in path.split("/")):
        raise ValueError("Unsafe source-owned path")


def _split_key(value: str) -> Tuple[str, bytes]:
    if type(value) is not str or value.count(":") != 1:
        raise ValueError("Malformed trusted cache key")
    name, encoded = value.split(":", 1)
    if not re.fullmatch(r"[A-Za-z0-9._-]{1,128}", name):
        raise ValueError("Malformed trusted cache key name")
    try:
        raw = base64.b64decode(encoded, validate=True)
    except (ValueError, base64.binascii.Error) as error:
        raise ValueError("Malformed trusted cache key encoding") from error
    if base64.b64encode(raw).decode("ascii") != encoded:
        raise ValueError("Trusted cache key encoding is not canonical")
    return name, raw


def _nix_string_list(flake_bytes: bytes, variable: str) -> List[str]:
    try:
        text = flake_bytes.decode("utf-8", "strict")
    except UnicodeDecodeError as error:
        raise ValueError("flake.nix is not UTF-8") from error
    matches = list(re.finditer(r"(?m)^\s*" + re.escape(variable) + r"\s*=\s*\[(.*?)\];", text, re.S))
    if len(matches) != 1:
        raise ValueError("flake.nix does not declare one " + variable + " list")
    match = matches[0]
    values = re.findall(r'"([^"\\]*)"', match.group(1))
    remainder = re.sub(r'"([^"\\]*)"', "", match.group(1))
    remainder = re.sub(r"(?m)#.*$", "", remainder)
    if re.sub(r"[\s,]", "", remainder):
        raise ValueError("flake.nix " + variable + " list has unsupported expressions")
    if len(values) != len(set(values)):
        raise ValueError("flake.nix " + variable + " list has duplicates")
    return values


def _assert_trusted_keys_align(flake_bytes: bytes, registry: Dict[str, Any]) -> None:
    substituters = _nix_string_list(flake_bytes, "extra-substituters")
    public_keys = _nix_string_list(flake_bytes, "extra-trusted-public-keys")
    trusted = registry["trusted_cache_keys"]
    expected_caches = sorted(item["cache"] for item in trusted)
    expected_keys = sorted(item["key"] for item in trusted)
    if sorted(substituters) != expected_caches or sorted(public_keys) != expected_keys:
        raise ValueError("Candidate trusted caches and keys differ from flake.nix")


def _resolved_registry(registry: Dict[str, Any]) -> Dict[str, Any]:
    policy = registry["roles"]["native_neo"]["target_policy"]
    if release.CONFIG.get("native_policy") != policy:
        raise ValueError("Installer registry native policy differs from release config")
    plan = {"group": "platform", "target_policy": policy}
    targets = sorted(release.targets_for(plan))
    if len(targets) != registry["roles"]["native_neo"]["expected_target_count"]:
        raise ValueError("Existing native Neo policy resolved to an unexpected target count")
    required_assets = sorted(remote.required_assets(plan))
    prefix = release.group_config("platform")["asset_prefix"]
    native_files = {prefix + target for target in targets}
    if not native_files.issubset(required_assets):
        raise ValueError("Existing native Neo owner omitted a required target asset")
    auxiliary = sorted(set(required_assets) - native_files)
    return {
        "native_neo": {"policy": policy, "targets": targets},
        "required_assets": required_assets,
        "auxiliary_assets": auxiliary,
    }


def contract_registry_projection() -> Dict[str, Any]:
    """Return the registry plus its resolved existing native-v2 asset obligation."""
    _raw, registry = _registry()
    _assert_trusted_keys_align(
        _read_worktree_file(release.ROOT, "flake.nix", 2 * 1024 * 1024), registry
    )
    return {"registry": registry, "resolved": _resolved_registry(registry)}


def contract_registry_bytes() -> bytes:
    """Return deterministic canonical bytes for the Rust conformance projection."""
    return canonical_json(contract_registry_projection())


def contract_registry_sha256() -> str:
    """Return the SHA-256 identity bound into candidate cores and new plans."""
    return _sha256(contract_registry_bytes())


def _exact_keys(value: Any, wire_fields: Dict[str, Any], name: str) -> Dict[str, Any]:
    if not isinstance(value, dict):
        raise ValueError(name + " must be an object")
    fields = wire_fields[name]
    if set(value) != set(fields) or len(value) != len(fields):
        raise ValueError(name + " fields differ from the closed candidate contract")
    return value


def _string(value: Any, label: str, max_bytes: int = 1024) -> str:
    if type(value) is not str:
        raise ValueError(label + " must be a string")
    try:
        raw = value.encode("utf-8", "strict")
    except UnicodeEncodeError as error:
        raise ValueError(label + " contains invalid Unicode") from error
    if not raw or len(raw) > max_bytes:
        raise ValueError(label + " has an invalid length")
    return value


def _sha(value: Any, label: str) -> str:
    if type(value) is not str or not SHA256.fullmatch(value):
        raise ValueError(label + " must be a lowercase SHA-256 digest")
    return value


def _commit(value: Any, label: str) -> str:
    if type(value) is not str or not COMMIT.fullmatch(value):
        raise ValueError(label + " must be a lowercase Git commit SHA")
    return value


def _integer(value: Any, label: str, minimum: int, maximum: int) -> int:
    if type(value) is not int or value < minimum or value > maximum:
        raise ValueError(label + " is outside its integer bounds")
    return value


def _valid_sri(value: Any, label: str) -> str:
    if type(value) is not str or not value.startswith("sha256-"):
        raise ValueError(label + " must be a SHA-256 SRI value")
    encoded = value[7:]
    try:
        decoded = base64.b64decode(encoded, validate=True)
    except (ValueError, base64.binascii.Error) as error:
        raise ValueError(label + " has invalid SRI base64") from error
    if len(decoded) != 32 or base64.b64encode(decoded).decode("ascii") != encoded:
        raise ValueError(label + " has non-canonical SRI bytes")
    return value


def _valid_store_path(value: Any, limits: Dict[str, Any]) -> str:
    if type(value) is not str or len(value.encode("utf-8", "strict")) > limits["max_nix_store_path_bytes"]:
        raise ValueError("Nix output must be a bounded store path")
    match = NIX_STORE_PATH.fullmatch(value)
    if not match:
        raise ValueError("Nix output must use the full realized store-path form")
    name = match.group(2)
    if name.endswith(".drv") or name in {".", ".."}:
        raise ValueError("Nix output cannot be a derivation or traversal path")
    return value


def _validate_context(core: Dict[str, Any], registry: Dict[str, Any], wire: Dict[str, Any]) -> Dict[str, Any]:
    context = core["context"]
    if not isinstance(context, dict):
        raise ValueError("Candidate context must be an object")
    kind = context.get("kind")
    if kind == "engineering":
        _exact_keys(context, wire, "engineering_context")
        _commit(context["change_source"], "Engineering change source")
        _commit(context["build_revision"], "Engineering build revision")
        if context["build_revision"] != context["change_source"]:
            raise ValueError("Engineering candidates build from their committed source")
        source_tree = _sha(context["source_tree_sha256"], "Committed source tree identity")
        build_tree = _sha(context["build_tree_sha256"], "Engineering build tree identity")
        seed = context["version_seed"]
        if not isinstance(seed, dict):
            raise ValueError("Engineering version seed must be an object")
        if seed.get("kind") == "committed":
            _exact_keys(seed, wire, "committed_version_seed")
            if build_tree != source_tree:
                raise ValueError("Committed engineering build tree must equal its source tree")
        elif seed.get("kind") == "engineering-version":
            _exact_keys(seed, wire, "engineering_version_seed")
            seed_version = _string(seed["version"], "Engineering seed version", 32)
            try:
                release.version(seed_version)
                release.version(core["version"])
            except (TypeError, ValueError) as error:
                raise ValueError("Engineering seed version is not a stable numeric version") from error
            if seed_version != core["version"]:
                raise ValueError("Engineering seed version differs from the built candidate version")
            _sha(seed["recipe_sha256"], "Engineering seed recipe identity")
            _sha(seed["generated_diff_sha256"], "Engineering generated diff identity")
            if _sha(seed["build_tree_sha256"], "Engineering seed build tree identity") != build_tree:
                raise ValueError("Engineering version seed build tree differs from its context")
        else:
            raise ValueError("Unknown engineering version seed")
        return context
    if kind == "release":
        _exact_keys(context, wire, "release_context")
        _commit(context["change_source"], "Release change source")
        _commit(context["build_revision"], "Generated release build revision")
        if context["build_revision"] == context["change_source"]:
            raise ValueError("Release build revision must be the generated preparation")
        _sha(context["source_tree_sha256"], "Release source tree identity")
        _sha(context["build_tree_sha256"], "Release build tree identity")
        _sha(context["plan_id"], "Release plan identity")
        _sha(context["seed_receipt_sha256"], "Release seed identity")
        return context
    raise ValueError("Unknown candidate build context")


def _validate_file_identity(value: Any, wire: Dict[str, Any], limits: Dict[str, Any], label: str) -> Dict[str, Any]:
    item = _exact_keys(value, wire, "file_identity")
    name = _string(item["name"], label + " filename", limits["max_name_bytes"])
    if not SAFE_NAME.fullmatch(name) or name in {".", ".."} or name.endswith(".drv"):
        raise ValueError(label + " filename is not a safe basename")
    _sha(item["sha256"], label + " SHA-256")
    _integer(item["size"], label + " size", 1, limits["max_native_file_bytes"])
    return item


def _collect_file_identities(core: Dict[str, Any], registry: Dict[str, Any], resolved: Dict[str, Any]) -> List[Tuple[Dict[str, Any], int, str]]:
    wire = registry["wire_fields"]
    limits = registry["limits"]
    identities: List[Tuple[Dict[str, Any], int, str]] = []
    if not isinstance(core["auxiliary_assets"], dict):
        raise ValueError("Candidate auxiliary assets must be an object")
    for target, entry in core["native_neo"].items():
        _exact_keys(entry, wire, "native_neo_entry")
        identity = _validate_file_identity(entry["file"], wire, limits, "Native Neo")
        expected = release.group_config("platform")["asset_prefix"] + target
        if identity["name"] != expected:
            raise ValueError("Native Neo filename differs from required_assets")
        _sha(entry["build_receipt_sha256"], "Native Neo build receipt identity")
        identities.append((identity, limits["max_native_file_bytes"], "native-neo"))
    for target, entry in core["native_installers"].items():
        _exact_keys(entry, wire, "native_installer_entry")
        identity = _validate_file_identity(entry["file"], wire, limits, "Native installer")
        if identity["name"] != registry["roles"]["native_installers"]["filename_prefix"] + target:
            raise ValueError("Native installer filename differs from its platform role")
        _sha(entry["build_receipt_sha256"], "Native installer build receipt identity")
        identities.append((identity, limits["max_native_file_bytes"], "native-installer"))
    for name, entry in core["bootstraps"].items():
        _exact_keys(entry, wire, "bootstrap_entry")
        identity = _validate_file_identity(entry["file"], wire, limits, "Bootstrap")
        if identity["name"] != registry["roles"]["bootstraps"][name]["filename"]:
            raise ValueError("Bootstrap filename differs from its role")
        if entry["locator_input"] != registry["roles"]["bootstraps"][name]["locator_input"]:
            raise ValueError("Bootstrap must receive the candidate locator at runtime")
        _sha(entry["english_source_sha256"], "Bootstrap English source identity")
        _sha(entry["generator_recipe_sha256"], "Bootstrap generator recipe identity")
        identities.append((identity, limits["max_bootstrap_file_bytes"], "bootstrap"))
    for filename, identity in core["auxiliary_assets"].items():
        checked = _validate_file_identity(identity, wire, limits, "Auxiliary asset")
        if checked["name"] != filename:
            raise ValueError("Auxiliary map key differs from its file identity")
        identities.append((checked, limits["max_metadata_file_bytes"], "metadata"))
    for key, filename in (("closures", "closures"), ("build_receipts", "build_receipts")):
        identity = _validate_file_identity(core[key], wire, limits, filename)
        if identity["name"] != registry["metadata_files"][key]:
            raise ValueError("Candidate metadata filename differs from its registry")
        identities.append((identity, limits["max_table_bytes"], "metadata"))
    names = [identity["name"] for identity, _maximum, _kind in identities]
    if len(names) != len(set(names)) or "candidate.json" in names:
        raise ValueError("Candidate file identities contain a duplicate or reserved filename")
    expected_auxiliary = set(resolved["auxiliary_assets"])
    if set(core["auxiliary_assets"]) != expected_auxiliary:
        raise ValueError("Candidate auxiliary assets differ from github.required_assets")
    return identities


def _read_regular_file(directory: Path, name: str, expected_size: int, expected_sha256: str,
                       max_bytes: int, collect: bool) -> Optional[bytes]:
    path = directory / name
    try:
        before = path.lstat()
    except OSError as error:
        raise ValueError("Missing candidate file: " + name) from error
    if stat.S_ISLNK(before.st_mode) or not stat.S_ISREG(before.st_mode):
        raise ValueError("Candidate file is not a regular file: " + name)
    if before.st_size != expected_size or before.st_size > max_bytes:
        raise ValueError("Candidate file size differs from its identity: " + name)
    flags = os.O_RDONLY | getattr(os, "O_BINARY", 0) | getattr(os, "O_NOFOLLOW", 0)
    try:
        descriptor = os.open(str(path), flags)
    except OSError as error:
        raise ValueError("Cannot safely open candidate file: " + name) from error
    digest = hashlib.sha256()
    body = bytearray() if collect else None
    total = 0
    try:
        info = os.fstat(descriptor)
        if not stat.S_ISREG(info.st_mode) or info.st_size != expected_size:
            raise ValueError("Candidate file changed before it was opened: " + name)
        while True:
            chunk = os.read(descriptor, 1024 * 1024)
            if not chunk:
                break
            total += len(chunk)
            if total > expected_size or total > max_bytes:
                raise ValueError("Candidate file exceeds its declared size: " + name)
            digest.update(chunk)
            if body is not None:
                body.extend(chunk)
    finally:
        os.close(descriptor)
    if total != expected_size or digest.hexdigest() != expected_sha256:
        raise ValueError("Candidate file bytes differ from their identity: " + name)
    return bytes(body) if body is not None else None


def _verify_materialized_files(core_bytes: bytes, core: Dict[str, Any], artifact_directory: Path,
                               identities: List[Tuple[Dict[str, Any], int, str]], limits: Dict[str, Any]) -> Tuple[Dict[str, bytes], Path]:
    requested = Path(artifact_directory).absolute()
    try:
        info = requested.lstat()
    except OSError as error:
        raise ValueError("Candidate artifact directory is missing") from error
    if stat.S_ISLNK(info.st_mode) or not stat.S_ISDIR(info.st_mode):
        raise ValueError("Candidate artifact directory must be a real directory")
    directory = requested.resolve(strict=True)
    expected_names = {identity["name"] for identity, _max, _kind in identities} | {"candidate.json"}
    try:
        entries = list(directory.iterdir())
    except OSError as error:
        raise ValueError("Cannot list candidate artifact directory") from error
    actual_names = {entry.name for entry in entries}
    if actual_names != expected_names or len(entries) != len(expected_names):
        raise ValueError("Candidate artifact directory has missing or unlisted files")
    manifest = _read_regular_file(
        directory, "candidate.json", len(core_bytes), _sha256(core_bytes),
        limits["max_core_bytes"], True,
    )
    if manifest != core_bytes:
        raise ValueError("Materialized candidate.json differs from the parsed core")
    metadata: Dict[str, bytes] = {}
    for identity, maximum, kind in identities:
        content = _read_regular_file(
            directory, identity["name"], identity["size"], identity["sha256"], maximum,
            kind == "metadata" or kind == "bootstrap",
        )
        if content is not None and kind == "metadata":
            metadata[identity["name"]] = content
        elif content is not None and kind == "bootstrap":
            metadata[identity["name"]] = content
    return metadata, directory


def _validate_core_shape(core: Any, registry: Dict[str, Any]) -> Dict[str, Any]:
    wire = registry["wire_fields"]
    core = _exact_keys(core, wire, "core")
    if type(core["schema_version"]) is not int or core["schema_version"] != 2:
        raise ValueError("Unknown candidate core schema version")
    if core["policy"] != registry["policy"]:
        raise ValueError("Unknown candidate distribution policy")
    _sha(core["contract_registry_sha256"], "Contract registry identity")
    if core["contract_registry_sha256"] != contract_registry_sha256():
        raise ValueError("Candidate contract registry identity differs from this owner")
    if type(core["version"]) is not str:
        raise ValueError("Candidate version must be a string")
    try:
        release.version(core["version"])
    except (TypeError, ValueError) as error:
        raise ValueError("Candidate version must be a stable numeric version") from error
    _validate_context(core, registry, wire)
    inputs = _exact_keys(core["build_inputs"], wire, "build_inputs")
    _sha(inputs["flake_lock_sha256"], "Nix flake lock identity")
    _sha(inputs["cargo_lock_sha256"], "Cargo lock identity")
    _sha(inputs["recipe_set_sha256"], "Source-owned recipe identity")
    starter = _exact_keys(inputs["starter"], wire, "starter_inputs")
    _commit(starter["framework_revision"], "Starter framework revision")
    _valid_sri(starter["framework_nar_hash"], "Starter framework NAR hash")
    for name in ("flake_sha256", "lock_sha256", "cabal_project_sha256"):
        _sha(starter[name], "Starter " + name)
    native_policy = registry["roles"]["native_neo"]["target_policy"]
    plan = {"group": "platform", "target_policy": native_policy}
    native_targets = sorted(release.targets_for(plan))
    if not isinstance(core["native_neo"], dict):
        raise ValueError("Candidate native Neo roles must be an object")
    if set(core["native_neo"]) != set(native_targets):
        raise ValueError("Candidate native Neo roles differ from the existing release policy")
    installers = registry["roles"]["native_installers"]["platforms"]
    if not isinstance(core["native_installers"], dict) or set(core["native_installers"]) != set(installers):
        raise ValueError("Candidate native installer roles differ from installer-v1")
    if not isinstance(core["bootstraps"], dict) or set(core["bootstraps"]) != set(registry["roles"]["bootstraps"]):
        raise ValueError("Candidate bootstrap roles differ from installer-v1")
    package_policy = registry["roles"]["nix_packages"]
    if not isinstance(core["packages"], dict) or set(core["packages"]) != set(package_policy["systems"]):
        raise ValueError("Candidate Nix platforms differ from installer-v1")
    output_roots: Dict[str, Dict[str, str]] = {}
    all_store_paths = []
    for system, outputs in core["packages"].items():
        if not isinstance(outputs, dict) or set(outputs) != set(package_policy["outputs"]):
            raise ValueError("Candidate Nix output roles are incomplete: " + system)
        output_roots[system] = {}
        for name, value in outputs.items():
            expected_fields = "pair_output" if name == "pair" else "package_output"
            item = _exact_keys(value, wire, expected_fields)
            path = _valid_store_path(item["store_path"], registry["limits"])
            all_store_paths.append(path)
            _valid_sri(item["nar_hash"], "Nix output NAR hash")
            _integer(item["nar_size"], "Nix output NAR size", 1, (1 << 63) - 1)
            _sha(item["closure_id"], "Nix closure identity")
            _sha(item["build_receipt_sha256"], "Nix build receipt identity")
            output_roots[system][name] = path
            if name == "pair":
                members = _exact_keys(item["members"], wire, "pair_members")
                expected_members = {
                    "neo": outputs["neo"]["store_path"],
                    "neoctl": outputs["neoctl"]["store_path"],
                }
                if members != expected_members:
                    raise ValueError("Nix pair members differ from its system's neo and neoctl")
    if len(all_store_paths) != 9 or len(set(all_store_paths)) != 9:
        raise ValueError("Nix package roots must contain nine distinct realized outputs")
    _validate_file_identity(core["closures"], wire, registry["limits"], "Closure table")
    _validate_file_identity(core["build_receipts"], wire, registry["limits"], "Build receipt table")
    resolved = _resolved_registry(registry)
    identities = _collect_file_identities(core, registry, resolved)
    return {"core": core, "output_roots": output_roots, "identities": identities}


def _validate_name_list(values: Any, limit: int, label: str) -> List[str]:
    if not isinstance(values, list) or len(values) > limit:
        raise ValueError(label + " must be a bounded array")
    checked = []
    for value in values:
        item = _valid_store_path(value, {"max_nix_store_path_bytes": 255})
        checked.append(item)
    if checked != sorted(checked) or len(checked) != len(set(checked)):
        raise ValueError(label + " must be sorted and unique")
    return checked


def _validate_signature(signature: Any, cache: str, trusted_keys: Dict[str, str]) -> None:
    if type(signature) is not str or len(signature) > 256 or signature.count(":") != 1:
        raise ValueError("Cache signature metadata is malformed")
    key_name, encoded = signature.split(":", 1)
    expected = trusted_keys.get(cache)
    if expected is None or not signature.startswith(expected.split(":", 1)[0] + ":"):
        raise ValueError("Cache signature key is not in the source-owned registry")
    try:
        raw = base64.b64decode(encoded, validate=True)
    except (ValueError, base64.binascii.Error) as error:
        raise ValueError("Cache signature metadata has invalid base64") from error
    if len(raw) != 64 or base64.b64encode(raw).decode("ascii") != encoded:
        raise ValueError("Cache signature metadata is not canonical Ed25519-sized data")
    if key_name != expected.split(":", 1)[0]:
        raise ValueError("Cache signature metadata key identifier differs")


def _closure_identity(item: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "store_path": item["store_path"],
        "nar_hash": item["nar_hash"],
        "nar_size": item["nar_size"],
        "references": item["references"],
    }


def _closure_digest(paths: List[str], path_map: Dict[str, Dict[str, Any]]) -> str:
    rows = [_closure_identity(path_map[path]) for path in sorted(paths)]
    return _canonical_digest(rows)


def _validate_closure_table(value: Any, core: Dict[str, Any], registry: Dict[str, Any],
                            expected_roots: Dict[str, Dict[str, str]]) -> None:
    wire = registry["wire_fields"]
    limits = registry["limits"]
    table = _exact_keys(value, wire, "closure_table")
    if type(table["schema_version"]) is not int or table["schema_version"] != 1:
        raise ValueError("Unknown closure table schema version")
    if table["roots"] != expected_roots:
        raise ValueError("Closure roots differ from the exact nine package outputs")
    paths = table["paths"]
    if not isinstance(paths, list) or len(paths) > limits["max_closure_paths"]:
        raise ValueError("Closure path count exceeds its bound")
    path_map: Dict[str, Dict[str, Any]] = {}
    trusted_keys = {item["cache"]: item["key"] for item in registry["trusted_cache_keys"]}
    total_edges = 0
    previous_path = None
    for value in paths:
        item = _exact_keys(value, wire, "closure_path")
        path = _valid_store_path(item["store_path"], limits)
        if previous_path is not None and path <= previous_path:
            raise ValueError("Closure paths must be sorted and unique")
        previous_path = path
        _valid_sri(item["nar_hash"], "Closure NAR hash")
        _integer(item["nar_size"], "Closure NAR size", 1, (1 << 63) - 1)
        references = _validate_name_list(
            item["references"], limits["max_references_per_path"], "Closure references"
        )
        total_edges += len(references)
        if total_edges > limits["max_closure_edges"]:
            raise ValueError("Closure edge count exceeds its bound")
        locations = item["locations"]
        if not isinstance(locations, list) or not 1 <= len(locations) <= limits["max_cache_locations_per_path"]:
            raise ValueError("Closure cache location count exceeds its bound")
        last_cache = None
        seen_locations = set()
        for location_value in locations:
            location = _exact_keys(location_value, wire, "cache_location")
            cache = _string(location["cache"], "Cache location", 256)
            if cache not in trusted_keys or cache in seen_locations:
                raise ValueError("Closure uses an untrusted or duplicate cache location")
            if last_cache is not None and cache <= last_cache:
                raise ValueError("Cache locations must be sorted and unique")
            last_cache = cache
            seen_locations.add(cache)
            signatures = location["signatures"]
            if not isinstance(signatures, list) or not 1 <= len(signatures) <= limits["max_signatures_per_location"]:
                raise ValueError("Cache signature metadata count exceeds its bound")
            if any(type(signature) is not str for signature in signatures):
                raise ValueError("Cache signature metadata must contain strings")
            if signatures != sorted(set(signatures)):
                raise ValueError("Cache signature metadata must be sorted and unique")
            for signature in signatures:
                _validate_signature(signature, cache, trusted_keys)
        path_map[path] = item
    if not path_map:
        raise ValueError("Closure table has no paths")
    for path, item in path_map.items():
        missing = set(item["references"]) - set(path_map)
        if missing:
            raise ValueError("Closure contains a missing reference from " + path)
    all_roots = [path for system in expected_roots.values() for path in system.values()]
    if len(all_roots) != 9 or len(set(all_roots)) != 9:
        raise ValueError("Closure root contract does not contain nine distinct outputs")
    reachable_by_root: Dict[str, set] = {}
    reachable_union = set()
    for system, roles in expected_roots.items():
        for role, root in roles.items():
            if root not in path_map:
                raise ValueError("Closure table omits a declared package root")
            stack = [root]
            reachable = set()
            while stack:
                current = stack.pop()
                if current in reachable:
                    continue
                reachable.add(current)
                stack.extend(path_map[current]["references"])
            reachable_by_root[root] = reachable
            reachable_union.update(reachable)
            if _closure_digest(sorted(reachable), path_map) != core["packages"][system][role]["closure_id"]:
                raise ValueError("Declared closure identity differs from reachable NAR metadata")
            if not any(location["cache"] == "https://neohaskell.cachix.org"
                       for location in path_map[root]["locations"]):
                raise ValueError("Every package root must be listed in the NeoHaskell cache")
            output = core["packages"][system][role]
            if (output["nar_hash"], output["nar_size"]) != (
                path_map[root]["nar_hash"], path_map[root]["nar_size"]
            ):
                raise ValueError("Package root NAR identity differs from closure metadata")
    if reachable_union != set(path_map):
        raise ValueError("Closure table contains an unreachable extra path")
    for system, roles in expected_roots.items():
        root_set = set(roles.values())
        for role in ("neo", "neoctl"):
            foreign = (reachable_by_root[roles[role]] & root_set) - {roles[role]}
            if foreign:
                raise ValueError("Separate Nix tool output reaches another package root")
        pair = roles["pair"]
        pair_members = {roles["neo"], roles["neoctl"]}
        if not pair_members.issubset(path_map[pair]["references"]):
            raise ValueError("Aggregate pair output does not directly reference both members")
        if (reachable_by_root[pair] & root_set) != pair_members | {pair}:
            raise ValueError("Aggregate pair closure contains an unexpected package root")
        if (set(path_map[pair]["references"]) & root_set) != pair_members:
            raise ValueError("Aggregate pair directly references the wrong package members")


def _expected_output_receipts(core: Dict[str, Any]) -> Dict[Tuple[str, str], Tuple[Dict[str, Any], Dict[str, Any], Dict[str, Any]]]:
    outputs: Dict[Tuple[str, str], Tuple[Dict[str, Any], Dict[str, Any], Dict[str, Any]]] = {}
    for platform, entry in core["native_neo"].items():
        identity = {"sha256": entry["file"]["sha256"], "size": entry["file"]["size"]}
        outputs[("native-neo", platform)] = (entry, identity, {"neo": core["version"]})
    for platform, entry in core["native_installers"].items():
        identity = {"sha256": entry["file"]["sha256"], "size": entry["file"]["size"]}
        outputs[("native-installer", platform)] = (entry, identity, {"neo-installer": core["version"]})
    for system, roles in core["packages"].items():
        for name, entry in roles.items():
            role = "nix-" + name
            identity = {
                "store_path": entry["store_path"],
                "nar_hash": entry["nar_hash"],
                "nar_size": entry["nar_size"],
            }
            if name == "pair":
                identity["members"] = entry["members"]
            versions = {"neo": core["version"], "neoctl": core["version"]} if name == "pair" else {
                name: core["version"]
            }
            outputs[(role, system)] = (entry, identity, versions)
    return outputs


def _validate_receipts_table(value: Any, core: Dict[str, Any], registry: Dict[str, Any]) -> None:
    wire = registry["wire_fields"]
    table = _exact_keys(value, wire, "build_receipts_table")
    if type(table["schema_version"]) is not int or table["schema_version"] != 1:
        raise ValueError("Unknown build receipt table schema version")
    receipts = table["receipts"]
    expected_outputs = _expected_output_receipts(core)
    if not isinstance(receipts, list) or len(receipts) != len(expected_outputs):
        raise ValueError("Build receipt table must cover every one of the fourteen outputs")
    context_digest = _canonical_digest(core["context"])
    inputs_digest = _canonical_digest(core["build_inputs"])
    source_digest = core["context"]["source_tree_sha256"]
    build_digest = core["context"]["build_tree_sha256"]
    recipe_digest = core["build_inputs"]["recipe_set_sha256"]
    previous_key = None
    seen = set()
    for receipt_value in receipts:
        receipt = _exact_keys(receipt_value, wire, "build_receipt")
        role = _string(receipt["role"], "Build receipt role", 64)
        platform = _string(receipt["platform"], "Build receipt platform", 128)
        key = (role, platform)
        if previous_key is not None and key <= previous_key:
            raise ValueError("Build receipts must be sorted and unique")
        previous_key = key
        if key not in expected_outputs or key in seen:
            raise ValueError("Build receipt names an unknown or duplicate output role")
        seen.add(key)
        _sha(receipt["context_sha256"], "Build receipt context identity")
        _sha(receipt["source_tree_sha256"], "Build receipt source identity")
        _sha(receipt["build_tree_sha256"], "Build receipt build tree identity")
        _sha(receipt["build_inputs_sha256"], "Build receipt input identity")
        _sha(receipt["recipe_set_sha256"], "Build receipt recipe identity")
        _sha(receipt["producer_sha256"], "Build receipt producer identity")
        if (receipt["context_sha256"], receipt["source_tree_sha256"],
                receipt["build_tree_sha256"], receipt["build_inputs_sha256"],
                receipt["recipe_set_sha256"], receipt["producer_sha256"], receipt["version"]) != (
            context_digest, source_digest, build_digest, inputs_digest,
            recipe_digest, recipe_digest, core["version"]
        ):
            raise ValueError("Build receipt source, recipe, context, or version differs from CandidateCore")
        _string(receipt["builder_system"], "Build receipt builder system", 128)
        output = receipt["output_identity"]
        expected_entry, expected_identity, reported_versions = expected_outputs[key]
        if key[0].startswith("native-"):
            identity_fields = "native_output_identity"
        elif key[0] == "nix-pair":
            identity_fields = "pair_output_identity"
        else:
            identity_fields = "nix_output_identity"
        _exact_keys(output, wire, identity_fields)
        if output != expected_identity:
            raise ValueError("Build receipt output identity differs from its CandidateCore role")
        observations = _exact_keys(receipt["verification_observations"], wire, "verification_observations")
        for name in ("argv_sha256", "result_sha256", "toolchain_sha256"):
            _sha(observations[name], "Build receipt " + name)
        reported = observations["reported_versions"]
        if not isinstance(reported, dict) or reported != reported_versions:
            raise ValueError("Build receipt reported versions differ from the lockstep candidate version")
        receipt_hash = _canonical_digest(receipt)
        if expected_entry["build_receipt_sha256"] != receipt_hash:
            raise ValueError("Candidate output points at a different build receipt")
    if seen != set(expected_outputs):
        raise ValueError("Build receipt table omits a required output role")


def parse_candidate(core_bytes: bytes, materialized_dir: Path) -> ParsedCandidate:
    """Parse a canonical core and recheck every declared local file byte.

    The result establishes only schema, identity, graph, and receipt consistency.
    It does not verify Nix signatures or claim that the receipt author executed a build.
    """
    _raw_registry, registry = _registry()
    limits = registry["limits"]
    data = _loads(
        core_bytes,
        limits["max_core_bytes"],
        limits["max_core_depth"],
        limits["max_json_nodes"],
        reject_booleans=True,
    )
    if canonical_json(data) != core_bytes:
        raise ValueError("CandidateCore is not canonical UTF-8 JSON")
    checked = _validate_core_shape(data, registry)
    metadata, directory = _verify_materialized_files(
        core_bytes, checked["core"], Path(materialized_dir), checked["identities"], limits
    )
    _starter_fields(checked["core"], metadata, directory, registry)
    closure_bytes = metadata[checked["core"]["closures"]["name"]]
    receipts_bytes = metadata[checked["core"]["build_receipts"]["name"]]
    closure_table = _loads(
        closure_bytes, limits["max_table_bytes"], limits["max_core_depth"],
        limits["max_json_nodes"], reject_booleans=True,
    )
    receipts_table = _loads(
        receipts_bytes, limits["max_table_bytes"], limits["max_core_depth"],
        limits["max_json_nodes"], reject_booleans=True,
    )
    if canonical_json(closure_table) != closure_bytes:
        raise ValueError("Closure table is not canonical UTF-8 JSON")
    if canonical_json(receipts_table) != receipts_bytes:
        raise ValueError("Build receipt table is not canonical UTF-8 JSON")
    _validate_closure_table(closure_table, checked["core"], registry, checked["output_roots"])
    _validate_receipts_table(receipts_table, checked["core"], registry)
    candidate_id = _sha256(core_bytes)
    for name in (entry["file"]["name"] for entry in checked["core"]["bootstraps"].values()):
        if candidate_id.encode("ascii") in metadata[name]:
            raise ValueError("Bootstrap embeds its own candidate identity")
    return ParsedCandidate(
        candidate_id=candidate_id,
        core_bytes=core_bytes,
        artifact_directory=directory,
        version=checked["core"]["version"],
        context_kind=checked["core"]["context"]["kind"],
    )


def _git(root: Path, *arguments: str, input_bytes: Optional[bytes] = None,
         max_output: Optional[int] = None) -> bytes:
    try:
        result = subprocess.run(
            ["git", *arguments], cwd=str(root), input=input_bytes,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True, timeout=120,
        )
    except (OSError, subprocess.SubprocessError) as error:
        raise ValueError("Git source binding failed for " + arguments[0]) from error
    if max_output is not None and len(result.stdout) > max_output:
        raise ValueError("Git source metadata exceeds its byte limit")
    return result.stdout


def _git_commit(root: Path, revision: str) -> str:
    _commit(revision, "Source revision")
    actual = _git(root, "rev-parse", "--verify", revision + "^{commit}").decode("ascii", "strict").strip()
    if actual != revision:
        raise ValueError("Candidate source revision is not an exact Git commit")
    return actual


def _git_tree(root: Path, revision: str, limits: Dict[str, Any]) -> Tuple[bytes, Dict[bytes, Tuple[bytes, bytes, bytes]]]:
    _git_commit(root, revision)
    raw = _git(
        root, "ls-tree", "-rz", "-r", "--full-tree", revision,
        max_output=limits["max_source_tree_index_bytes"],
    )
    records = raw.split(b"\0")
    if records and records[-1] == b"":
        records.pop()
    if len(records) > limits["max_source_tree_entries"]:
        raise ValueError("Source tree entry count exceeds its bound")
    index: Dict[bytes, Tuple[bytes, bytes, bytes]] = {}
    for record in records:
        try:
            metadata, path = record.split(b"\t", 1)
            mode, kind, object_id = metadata.split(b" ", 2)
        except ValueError as error:
            raise ValueError("Git returned malformed source tree metadata") from error
        if not path or path.startswith(b"/") or b"\0" in path:
            raise ValueError("Git source tree contains an unsafe path")
        if path in index:
            raise ValueError("Git source tree contains duplicate paths")
        index[path] = (mode, kind, object_id)
    return raw, index


def _git_blob(root: Path, index: Dict[bytes, Tuple[bytes, bytes, bytes]], path: str,
              max_bytes: int = 32 * 1024 * 1024) -> Tuple[bytes, bytes]:
    _validate_source_path(path)
    raw_path = path.encode("utf-8")
    item = index.get(raw_path)
    if item is None:
        raise ValueError("Missing committed source input: " + path)
    mode, kind, object_id = item
    if kind != b"blob" or mode not in {b"100644", b"100755"}:
        raise ValueError("Committed source input is not a regular tracked file: " + path)
    object_name = object_id.decode("ascii")
    size_text = _git(root, "cat-file", "-s", object_name, max_output=32).decode("ascii", "strict").strip()
    if not re.fullmatch(r"0|[1-9][0-9]*", size_text) or int(size_text) > max_bytes:
        raise ValueError("Committed source input exceeds its byte limit: " + path)
    raw = _git(root, "cat-file", "blob", object_name, max_output=max_bytes)
    if len(raw) != int(size_text):
        raise ValueError("Committed source input changed while being read: " + path)
    return mode, raw


def _tree_digest(raw: bytes) -> str:
    return _sha256(raw)


def _git_blob_oid(content: bytes) -> bytes:
    header = b"blob " + str(len(content)).encode("ascii") + b"\0"
    return hashlib.sha1(header + content).hexdigest().encode("ascii")


def _overlay_tree_bytes(raw: bytes, overlays: Dict[str, bytes]) -> bytes:
    if not overlays:
        return raw
    parsed = []
    for record in raw.split(b"\0"):
        if not record:
            continue
        try:
            metadata, path = record.split(b"\t", 1)
            mode, kind, object_id = metadata.split(b" ", 2)
        except ValueError as error:
            raise ValueError("Git returned malformed build tree metadata") from error
        replacement = overlays.get(path.decode("utf-8", "strict"))
        if replacement is not None:
            if mode not in {b"100644", b"100755"} or kind != b"blob":
                raise ValueError("Build overlay cannot replace a non-regular source file")
            object_id = _git_blob_oid(replacement)
        parsed.append((path, mode, kind, object_id))
    if set(overlays) - {path.decode("utf-8", "strict") for path, _mode, _kind, _oid in parsed}:
        raise ValueError("Build overlay adds or changes an untracked source path")
    return b"".join(mode + b" " + kind + b" " + object_id + b"\t" + path + b"\0"
                   for path, mode, kind, object_id in parsed)


def _identity_set_digest(rows: List[Dict[str, str]]) -> str:
    ordered = sorted(rows, key=lambda row: row["path"].encode("utf-8"))
    if len({row["path"] for row in ordered}) != len(ordered):
        raise ValueError("Source recipe inputs contain duplicate paths")
    return _canonical_digest(ordered)


def _source_inputs(root: Path, index: Dict[bytes, Tuple[bytes, bytes, bytes]], paths: List[str]) -> Tuple[str, Dict[str, bytes]]:
    rows = []
    files: Dict[str, bytes] = {}
    for path in paths:
        _mode, content = _git_blob(root, index, path)
        files[path] = content
        rows.append({"path": path, "sha256": _sha256(content)})
    return _identity_set_digest(rows), files


def _assert_worktree_matches(root: Path, paths: List[str], source_files: Dict[str, bytes]) -> None:
    for path in paths:
        current = _read_worktree_file(root, path, 32 * 1024 * 1024)
        if current != source_files[path]:
            raise ValueError("Working source differs from the committed candidate input: " + path)


def _config_profile(root: Path, revision: str,
                    index: Dict[bytes, Tuple[bytes, bytes, bytes]]) -> Dict[str, Any]:
    _mode, raw = _git_blob(root, index, CONFIG_RELATIVE_PATH)
    config = _loads(raw, 2 * 1024 * 1024, 24, 100000)
    if not isinstance(config, dict):
        raise ValueError("Committed release config must be an object")
    if config.get("native_policy") != release.TARGET_POLICY:
        raise ValueError("Committed source does not use the supported native-v2 policy")
    groups = config.get("groups")
    profile = groups.get("platform") if isinstance(groups, dict) else None
    if not isinstance(profile, dict) or not isinstance(profile.get("packages"), dict) or not isinstance(profile.get("locks"), dict):
        raise ValueError("Committed platform release inputs are malformed")
    current_profile = release.group_config("platform")
    source_semantics = {key: value for key, value in profile.items() if key not in ("packages", "locks")}
    current_semantics = {key: value for key, value in current_profile.items() if key not in ("packages", "locks")}
    if source_semantics != current_semantics:
        raise ValueError("Committed release configuration semantics are unsupported")
    required_products = _registry()[1]["wire_fields"]["pair_members"]
    registered_products = list(profile["packages"].values())
    if any(registered_products.count(name) != 1 for name in required_products):
        raise ValueError("Committed platform profile must register each paired product exactly once")
    for path in list(profile["packages"]) + list(profile["locks"]):
        _validate_source_path(path)
    engine_profile = release.source_group_config(release.Repo(root), revision, "platform")
    if engine_profile != profile:
        raise ValueError("Release engine source profile differs from committed input bytes")
    return profile


def _package_version(path: str, package_name: str, raw: bytes) -> str:
    try:
        text = raw.decode("utf-8", "strict")
    except UnicodeDecodeError as error:
        raise ValueError("Version input is not UTF-8: " + path) from error
    if path.endswith(".cabal"):
        matches = re.findall(r"(?mi)^\s*version:\s*([^\s]+)\s*$", text)
    elif path.endswith("Cargo.toml"):
        package = re.search(r"(?ms)^\[package\]\s*\n(.*?)(?=^\[|\Z)", text)
        matches = re.findall(r'(?m)^version\s*=\s*"([^"]+)"\s*$', package[1]) if package else []
    elif path.endswith("Cargo.lock"):
        package = re.escape(package_name)
        matches = re.findall(
            r'(?ms)^\[\[package\]\]\nname = "' + package
            + r'"\nversion = "([^"]+)"', text,
        )
    else:
        raise ValueError("Unsupported package version source: " + path)
    if len(matches) != 1:
        raise ValueError("Package version is not unique: " + path)
    try:
        release.version(matches[0])
    except ValueError as error:
        raise ValueError("Package version is not stable numeric data: " + path) from error
    return matches[0]


def _starter_fields(core: Dict[str, Any], metadata: Dict[str, bytes], directory: Path,
                    registry: Dict[str, Any]) -> Dict[str, bytes]:
    paths = registry["source_inputs"]["build"]["starter"]
    names = {
        paths["flake"]: "neo-starter-flake.nix",
        paths["lock"]: "neo-starter-flake.lock",
        paths["cabal_project"]: "neo-starter-cabal.project",
    }
    auxiliary = core["auxiliary_assets"]
    output: Dict[str, bytes] = {}
    for source_path, name in names.items():
        if name not in auxiliary:
            raise ValueError("Candidate omits a required starter file: " + name)
        content = metadata.get(name)
        if content is None:
            content = _read_regular_file(
                directory, name,
                auxiliary[name]["size"], auxiliary[name]["sha256"],
                registry["limits"]["max_metadata_file_bytes"], True,
            )
        if content is None:
            raise ValueError("Candidate starter file is unavailable: " + name)
        output[source_path] = content
    starter = core["build_inputs"]["starter"]
    expected_hashes = {
        "neo/starter/flake.nix": starter["flake_sha256"],
        "neo/starter/flake.lock": starter["lock_sha256"],
        "neo/starter/cabal.project": starter["cabal_project_sha256"],
    }
    for source_path, content in output.items():
        if _sha256(content) != expected_hashes[source_path]:
            raise ValueError("Starter bytes differ from build input identity: " + source_path)
    _verify_starter_pin(output, starter)
    expected_framework = (
        registry["source_inputs"]["engineering_starter_framework_revision"]
        if core["context"]["kind"] == "engineering"
        else core["context"]["build_revision"]
    )
    if starter["framework_revision"] != expected_framework:
        raise ValueError("Candidate starter pin differs from its build context")
    return output


def _verify_starter_pin(starter_files: Dict[str, bytes], starter: Dict[str, Any]) -> None:
    try:
        flake = starter_files["neo/starter/flake.nix"].decode("utf-8", "strict")
        project = starter_files["neo/starter/cabal.project"].decode("utf-8", "strict")
        lock_bytes = starter_files["neo/starter/flake.lock"]
    except UnicodeDecodeError as error:
        raise ValueError("Starter source metadata is malformed") from error
    lock = _loads(lock_bytes, 32 * 1024 * 1024, 24, 2500000)
    revisions = re.findall(r"\?rev=([0-9a-f]{40})", flake)
    declarations = re.findall(r'(?m)^\s*neohaskellCommit\s*=\s*"([0-9a-f]{40})"\s*;', flake)
    tags = re.findall(r"(?m)^\s*tag:\s*([0-9a-f]{40})\s*$", project)
    try:
        locked = lock["nodes"]["neohaskell"]["locked"]
    except (TypeError, KeyError) as error:
        raise ValueError("Starter lock has no pinned NeoHaskell input") from error
    if not isinstance(locked, dict):
        raise ValueError("Starter lock has no pinned NeoHaskell input")
    revision = starter["framework_revision"]
    if (revisions != [revision] or declarations != [revision] or not tags
            or any(tag != revision for tag in tags) or locked.get("rev") != revision
            or locked.get("narHash") != starter["framework_nar_hash"]):
        raise ValueError("Starter flake, Cabal, and Nix lock pins disagree")


def _tree_file(root: Path, index: Dict[bytes, Tuple[bytes, bytes, bytes]],
               path: str, overlays: Dict[str, bytes]) -> bytes:
    if path in overlays:
        return overlays[path]
    _mode, content = _git_blob(root, index, path)
    return content


def _engineering_version_overlay(root: Path, source: str,
                                 index: Dict[bytes, Tuple[bytes, bytes, bytes]],
                                 seed: Dict[str, Any], registry: Dict[str, Any]) -> Tuple[Dict[str, bytes], str]:
    _seed_hash, _seed_files = _source_inputs(root, index, registry["source_inputs"]["version_seed_recipe"])
    profile = _config_profile(root, source, index)
    edits: Dict[str, bytes] = {}
    for path, package in {**profile["packages"], **profile["locks"]}.items():
        _mode, original = _git_blob(root, index, path)
        try:
            text = original.decode("utf-8", "strict")
        except UnicodeDecodeError as error:
            raise ValueError("Version input is not UTF-8: " + path) from error
        changed = release.change_version(text, path, package, seed["version"])
        encoded = changed.encode("utf-8")
        if encoded == original:
            raise ValueError("Engineering version seed did not change " + path)
        edits[path] = encoded
    diff_rows = []
    for path, after in sorted(edits.items(), key=lambda item: item[0].encode("utf-8")):
        _mode, before = _git_blob(root, index, path)
        diff_rows.append({"path": path, "before_sha256": _sha256(before), "after_sha256": _sha256(after)})
    diff_digest = _canonical_digest(diff_rows)
    if seed["recipe_sha256"] != _seed_hash or seed["generated_diff_sha256"] != diff_digest:
        raise ValueError("Engineering version seed recipe or exact generated diff differs")
    return edits, diff_digest


def _validate_build_versions(root: Path, index: Dict[bytes, Tuple[bytes, bytes, bytes]],
                             overlays: Dict[str, bytes], profile: Dict[str, Any],
                             expected_version: str) -> None:
    for path, package in {**profile["packages"], **profile["locks"]}.items():
        content = _tree_file(root, index, path, overlays)
        if _package_version(path, package, content) != expected_version:
            raise ValueError("Candidate package or lock is not at its actual lockstep version: " + path)


def _validate_release_starter(root: Path, index: Dict[bytes, Tuple[bytes, bytes, bytes]],
                              revision: str, overlays: Dict[str, bytes]) -> None:
    paths = {'neo/starter/flake.nix', 'neo/starter/cabal.project', 'neo/starter/flake.lock'}
    if set(overlays) != paths:
        raise ValueError("Release starter overlay must contain exactly its three inputs")
    originals = {path: _git_blob(root, index, path)[1] for path in paths}
    try:
        flake = originals['neo/starter/flake.nix'].decode('utf-8', 'strict')
        project = originals['neo/starter/cabal.project'].decode('utf-8', 'strict')
    except UnicodeDecodeError as error:
        raise ValueError("Release starter source is not UTF-8") from error
    flake = re.sub(r'(?<=\?rev=)[0-9a-f]{40}', revision, flake)
    flake = re.sub(r'(neohaskellCommit = ")[0-9a-f]{40}(";)',
                   lambda match: match[1] + revision + match[2], flake)
    project = re.sub(r'(?m)^(\s*tag:\s*)[0-9a-f]{40}',
                     lambda match: match[1] + revision, project)
    if (overlays['neo/starter/flake.nix'] != flake.encode('utf-8')
            or overlays['neo/starter/cabal.project'] != project.encode('utf-8')):
        raise ValueError("Release starter differs from the maintained pin rewrites")
    before = _loads(originals['neo/starter/flake.lock'], 32 * 1024 * 1024, 24, 2500000)
    after = _loads(overlays['neo/starter/flake.lock'], 32 * 1024 * 1024, 24, 2500000)
    if (not isinstance(before, dict) or not isinstance(after, dict)
            or set(before) != {'nodes', 'root', 'version'} or set(after) != set(before)
            or before['root'] != after['root'] or before['version'] != after['version']
            or not isinstance(before['nodes'], dict) or not isinstance(after['nodes'], dict)
            or set(before['nodes']) != set(after['nodes'])):
        raise ValueError("Release starter lock structure changed")
    for name, node in before['nodes'].items():
        if name != 'neohaskell' and after['nodes'][name] != node:
            raise ValueError("Release starter lock changed an unrelated dependency")
    old = before['nodes'].get('neohaskell')
    new = after['nodes'].get('neohaskell')
    if (not isinstance(old, dict) or not isinstance(new, dict)
            or set(old) != {'flake', 'locked', 'original'} or set(new) != set(old)
            or old['flake'] is not False or new['flake'] is not False):
        raise ValueError("Release starter NeoHaskell input structure changed")
    locked_fields = {'lastModified', 'narHash', 'ref', 'rev', 'revCount', 'type', 'url'}
    for node in (old, new):
        locked, original = node['locked'], node['original']
        if (not isinstance(locked, dict) or not isinstance(original, dict)
                or not {'narHash', 'rev', 'type', 'url'} <= set(locked)
                or set(locked) - locked_fields or set(original) != {'rev', 'type', 'url'}
                or locked.get('type') != 'git' or original.get('type') != 'git'
                or locked.get('url') != 'https://github.com/neohaskell/neohaskell.git'
                or original.get('url') != locked.get('url')):
            raise ValueError("Release starter NeoHaskell source identity changed")
        for field in ('lastModified', 'revCount'):
            if field in locked and (type(locked[field]) is not int or locked[field] < 0):
                raise ValueError("Release starter lock metadata is malformed")
        if 'ref' in locked and locked['ref'] != 'refs/heads/main':
            raise ValueError("Release starter NeoHaskell source ref changed")
    if (new['original']['rev'] != revision or new['locked']['rev'] != revision):
        raise ValueError("Release starter lock does not pin its preparation")


def _validate_release_context(root: Path, core: Dict[str, Any], registry: Dict[str, Any],
                              starter_overlays: Dict[str, bytes]) -> Tuple[Dict[str, bytes], Dict[bytes, Tuple[bytes, bytes, bytes]]]:
    context = core["context"]
    repo = release.Repo(root)
    try:
        ledger = repo.ledger(context["build_revision"])
    except (ValueError, KeyError, TypeError) as error:
        raise ValueError("Release preparation has no readable immutable ledger") from error
    plan = next((item for item in ledger["attempts"] if item.get("id") == context["plan_id"]), None)
    if plan is None:
        raise ValueError("Release plan identity is absent from its generated preparation")
    if (plan.get("kind") != "release" or plan.get("group") != "platform"
            or plan.get("source_sha") != context["change_source"]
            or plan.get("version") != core["version"]):
        raise ValueError("Release plan differs from candidate source or version")
    if (plan.get("distribution_policy") != registry["policy"]
            or plan.get("contract_registry_sha256") != core["contract_registry_sha256"]):
        raise ValueError("Release plan does not bind installer-v1 and its contract registry")
    try:
        release.validate_source_policy(repo, plan)
        release.verify_generated(repo, context["build_revision"], plan)
    except (ValueError, KeyError, TypeError) as error:
        raise ValueError("Release preparation does not match its frozen plan") from error
    revision_tree, revision_index = _git_tree(root, context["build_revision"], registry["limits"])
    if context["build_revision"] == context["change_source"]:
        raise ValueError("Release preparation revision is not distinct from source")
    _validate_release_starter(root, revision_index, context["build_revision"], starter_overlays)
    overlays = starter_overlays
    build_tree = _tree_digest(_overlay_tree_bytes(revision_tree, overlays))
    seed_identity = {
        "schema_version": 1,
        "plan_id": context["plan_id"],
        "build_revision": context["build_revision"],
        "framework_revision": core["build_inputs"]["starter"]["framework_revision"],
        "framework_nar_hash": core["build_inputs"]["starter"]["framework_nar_hash"],
        "flake_sha256": core["build_inputs"]["starter"]["flake_sha256"],
        "lock_sha256": core["build_inputs"]["starter"]["lock_sha256"],
        "cabal_project_sha256": core["build_inputs"]["starter"]["cabal_project_sha256"],
    }
    if _canonical_digest(seed_identity) != context["seed_receipt_sha256"]:
        raise ValueError("Release seed identity differs from actual starter files")
    if core["context"]["build_tree_sha256"] != build_tree:
        raise ValueError("Release build tree identity differs from generated preparation and starter")
    return overlays, revision_index


def bind_source(parsed: ParsedCandidate, repository_root: Path) -> SourceBoundCandidate:
    """Bind a parsed candidate to committed Git inputs and its exact generated build tree.

    The result proves local source and byte consistency only. It grants no producer,
    cache-verification, publication, or platform-acceptance capability.
    """
    if type(parsed) is not ParsedCandidate:
        raise ValueError("Source binding requires a ParsedCandidate")
    parsed = parse_candidate(parsed.core_bytes, parsed.artifact_directory)
    root = Path(repository_root).resolve(strict=True)
    if not root.is_dir():
        raise ValueError("Source repository root is not a directory")
    core = parsed.core
    _, registry = _registry()
    context = core["context"]
    source = context["change_source"]
    source_tree, source_index = _git_tree(root, source, registry["limits"])
    source_digest = _tree_digest(source_tree)
    if source_digest != context["source_tree_sha256"]:
        raise ValueError("Candidate committed source tree identity differs")
    source_recipe_digest, recipe_files = _source_inputs(
        root, source_index, registry["source_inputs"]["recipe"]
    )
    _assert_worktree_matches(root, registry["source_inputs"]["recipe"], recipe_files)
    if source_recipe_digest != core["build_inputs"]["recipe_set_sha256"]:
        raise ValueError("Candidate source-owned recipe identity differs from committed inputs")
    source_registry = recipe_files[CONTRACT_RELATIVE_PATH]
    frozen_registry = _loads(source_registry, 2 * 1024 * 1024, 24, 100000)
    if not isinstance(frozen_registry, dict):
        raise ValueError("Committed candidate registry must be an object")
    _validate_registry(frozen_registry)
    if canonical_json(frozen_registry) != canonical_json(registry):
        raise ValueError("Candidate contract registry differs from its committed source")
    _assert_trusted_keys_align(_git_blob(root, source_index, "flake.nix")[1], registry)
    if core["contract_registry_sha256"] != contract_registry_sha256():
        raise ValueError("Candidate contract registry projection differs from its source owner")
    starter_overlays = _starter_fields(
        core, _read_bootstrap_metadata(parsed), parsed.artifact_directory, registry
    )
    english_path = registry["source_inputs"]["english_resource"]
    generator_path = registry["source_inputs"]["bootstrap_generator"]
    english = recipe_files.get(english_path)
    generator = recipe_files.get(generator_path)
    if english is None or generator is None:
        raise ValueError("Bootstrap source or generator recipe is missing")
    english_value = _loads(english, registry["limits"]["max_metadata_file_bytes"], 24, 100000)
    if not isinstance(english_value, dict):
        raise ValueError("Bootstrap English resource must be a JSON object")
    for entry in core["bootstraps"].values():
        if entry["english_source_sha256"] != _sha256(english):
            raise ValueError("Bootstrap English source hash differs from committed JSON")
        if entry["generator_recipe_sha256"] != _sha256(generator):
            raise ValueError("Bootstrap generator recipe hash differs from committed source")
    overlays: Dict[str, bytes] = {}
    if context["kind"] == "engineering":
        if context["build_revision"] != source:
            raise ValueError("Engineering build revision differs from committed source")
        if context["version_seed"]["kind"] == "committed":
            overlays = {}
            if context["build_tree_sha256"] != source_digest:
                raise ValueError("Committed engineering build tree differs from source")
        else:
            seed = context["version_seed"]
            overlays, _diff_digest = _engineering_version_overlay(
                root, source, source_index, seed, registry
            )
            build_tree, _build_index = _git_tree(root, source, registry["limits"])
            actual_build_tree = _tree_digest(_overlay_tree_bytes(build_tree, overlays))
            if actual_build_tree != seed["build_tree_sha256"]:
                raise ValueError("Engineering seed does not bind its generated build tree")
            if actual_build_tree != context["build_tree_sha256"]:
                raise ValueError("Engineering generated build tree identity differs")
    else:
        overlays, release_build_index = _validate_release_context(
            root, core, registry, starter_overlays
        )
    build_revision = context["build_revision"]
    if context["kind"] == "release":
        build_index = release_build_index
    else:
        build_index = source_index
    profile = _config_profile(root, build_revision, build_index)
    _validate_build_versions(root, build_index, overlays, profile, core["version"])
    build_inputs = core["build_inputs"]
    flake_lock = _tree_file(
        root, build_index,
        registry["source_inputs"]["build"]["flake_lock"], overlays,
    )
    cargo_lock = _tree_file(
        root, build_index,
        registry["source_inputs"]["build"]["cargo_lock"], overlays,
    )
    if _sha256(flake_lock) != build_inputs["flake_lock_sha256"]:
        raise ValueError("Candidate flake lock identity differs from its generated build tree")
    if _sha256(cargo_lock) != build_inputs["cargo_lock_sha256"]:
        raise ValueError("Candidate Cargo lock identity differs from its generated build tree")
    starter = build_inputs["starter"]
    expected_framework = (
        registry["source_inputs"]["engineering_starter_framework_revision"]
        if context["kind"] == "engineering"
        else context["build_revision"]
    )
    if starter["framework_revision"] != expected_framework:
        raise ValueError("Candidate starter pin differs from its build context")
    for source_path, content in starter_overlays.items():
        if context["kind"] == "engineering":
            _mode, source_content = _git_blob(root, source_index, source_path)
            if content != source_content:
                raise ValueError("Engineering candidate changed the committed starter input")
    _verify_starter_pin(starter_overlays, starter)
    result = SourceBoundCandidate(
        candidate_id=parsed.candidate_id,
        change_source=context["change_source"],
        build_revision=build_revision,
        source_tree_sha256=context["source_tree_sha256"],
        build_tree_sha256=context["build_tree_sha256"],
        version=core["version"],
        context_kind=context["kind"],
        artifact_directory=parsed.artifact_directory,
    )
    _SOURCE_BOUND_CANDIDATES.add(result)
    return result


def _read_bootstrap_metadata(parsed: ParsedCandidate) -> Dict[str, bytes]:
    core = parsed.core
    output: Dict[str, bytes] = {}
    for entry in core["bootstraps"].values():
        item = entry["file"]
        content = _read_regular_file(
            parsed.artifact_directory, item["name"], item["size"], item["sha256"],
            2 * 1024 * 1024, True,
        )
        if content is None:
            raise ValueError("Bootstrap source bytes are unavailable")
        output[item["name"]] = content
    for name, identity in core["auxiliary_assets"].items():
        if name.startswith("neo-starter-"):
            content = _read_regular_file(
                parsed.artifact_directory, name, identity["size"], identity["sha256"],
                32 * 1024 * 1024, True,
            )
            if content is None:
                raise ValueError("Starter source bytes are unavailable")
            output[name] = content
    return output


def parse_verification_selection(selection_bytes: bytes, current: SourceBoundCandidate,
                                 previous: SourceBoundCandidate) -> ParsedSelection:
    """Bind an ordered current/previous pair after both candidates are source-bound."""
    if (type(current) is not SourceBoundCandidate or type(previous) is not SourceBoundCandidate
            or current not in _SOURCE_BOUND_CANDIDATES
            or previous not in _SOURCE_BOUND_CANDIDATES):
        raise ValueError("Verification selection requires source-bound candidates")
    _raw, registry = _registry()
    limits = registry["limits"]
    selection = _loads(
        selection_bytes, limits["max_core_bytes"], limits["max_core_depth"],
        limits["max_json_nodes"], reject_booleans=True,
    )
    if canonical_json(selection) != selection_bytes:
        raise ValueError("VerificationSelection is not canonical UTF-8 JSON")
    selection = _exact_keys(selection, registry["wire_fields"], "verification_selection")
    if type(selection["schema_version"]) is not int or selection["schema_version"] != 1:
        raise ValueError("Unknown verification selection schema version")
    current_id = _sha(selection["current_candidate_id"], "Current candidate identity")
    previous_id = _sha(selection["previous_candidate_id"], "Previous candidate identity")
    if current_id == previous_id or current_id != current.candidate_id or previous_id != previous.candidate_id:
        raise ValueError("Verification selection differs from its ordered candidates")
    if current.version == previous.version:
        raise ValueError("Current and previous candidates must report distinct actual versions")
    return ParsedSelection(
        selection_id=_sha256(selection_bytes),
        current_candidate_id=current_id,
        previous_candidate_id=previous_id,
        current_version=current.version,
        previous_version=previous.version,
    )
