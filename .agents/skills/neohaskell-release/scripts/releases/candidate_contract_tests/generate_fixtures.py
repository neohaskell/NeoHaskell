#!/usr/bin/env python3

import argparse
import base64
from dataclasses import dataclass
import hashlib
import json
import os
from pathlib import Path
import shutil
import stat
import subprocess
import sys
import tempfile
from contextlib import contextmanager


RELEASES = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(RELEASES))
import candidate


ROOT = candidate.release.ROOT
NIX_BASE32 = "0123456789abcdfghijklmnpqrsvwxyz"
FRAMEWORK_REVISION = "ea75873fbd0da7da39124c82f328c2e70bdca629"
EXPECTED_PATHS = 76
EXPECTED_MAX_REFERENCES = 65
EXPECTED_NAR_SIZE = 5044109824


def _hash(data):
    return hashlib.sha256(data).hexdigest()


@dataclass(frozen=True, init=False)
class CandidateFixtureCorpus:
    root: Path
    source_root: Path
    source_paths: tuple

    def __init__(self, source_root, output_root):
        selected_source_root = Path(source_root).expanduser().resolve(strict=True)
        selected_output_root = _fresh_output_root(selected_source_root, output_root)
        selected_output_root.mkdir()
        source_paths = _generate_corpus(selected_source_root, selected_output_root)
        object.__setattr__(self, "root", selected_output_root)
        object.__setattr__(self, "source_root", selected_source_root)
        object.__setattr__(self, "source_paths", tuple(source_paths))


def temporary_corpus_directory(prefix="candidate-corpus-"):
    state = ROOT / ".agents-state/neohaskell-release"
    state.mkdir(parents=True, exist_ok=True)
    return tempfile.TemporaryDirectory(prefix=prefix, dir=state)


def _sri(data):
    return "sha256-" + base64.b64encode(hashlib.sha256(data).digest()).decode("ascii")


def _store_hash(number):
    result = []
    for _ in range(32):
        number, digit = divmod(number, len(NIX_BASE32))
        result.append(NIX_BASE32[digit])
    return "".join(reversed(result))


def _store_path(number, name):
    return "/nix/store/" + _store_hash(number) + "-" + name


def _git(root, *args, env=None):
    result = subprocess.run(
        ["git", *args], cwd=str(root), env=env,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True,
    )
    return result.stdout


def _source_paths(registry, source_root):
    paths = set(registry["source_inputs"]["recipe"])
    paths.add(registry["source_inputs"]["build"]["flake_lock"])
    paths.add(registry["source_inputs"]["build"]["cargo_lock"])
    paths.update(registry["source_inputs"]["build"]["starter"].values())
    config_bytes = candidate._read_worktree_file(
        source_root, candidate.CONFIG_RELATIVE_PATH, 2 * 1024 * 1024,
    )
    config = candidate._loads(config_bytes, 2 * 1024 * 1024, 24, 100000)
    profile = config["groups"]["platform"]
    paths.update(profile["packages"])
    paths.update(profile["locks"])
    for path in paths:
        candidate._validate_source_path(path)
    return sorted(paths), {**profile["packages"], **profile["locks"]}


def _missing_source_path(root, relative):
    path = root
    for part in relative.split("/"):
        path = path / part
        try:
            info = path.lstat()
        except FileNotFoundError:
            return True
        if stat.S_ISLNK(info.st_mode):
            raise ValueError("Source-owned input contains a symlink: " + relative)
    return False


def _write_source_snapshot(source_root, output_root):
    projection = candidate.contract_registry_projection()
    paths, versioned_files = _source_paths(projection["registry"], source_root)
    snapshot_root = output_root / "source"
    snapshot_root.mkdir()
    copied_paths = []
    bootstrap_generator = projection["registry"]["source_inputs"]["bootstrap_generator"]
    for relative in paths:
        destination = snapshot_root / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        if relative == bootstrap_generator and _missing_source_path(source_root, relative):
            content = b"synthetic-generator-recipe-v1\n"
        else:
            content = candidate._read_worktree_file(source_root, relative, 32 * 1024 * 1024)
            if relative in versioned_files:
                content = candidate.release.change_version(content.decode(), relative, versioned_files[relative], "0.17.1").encode()
            copied_paths.append(relative)
        destination.write_bytes(content)
    registry = candidate._loads((snapshot_root / candidate.CONTRACT_RELATIVE_PATH).read_bytes(), 2 * 1024 * 1024, 24, 100000)
    candidate._validate_registry(registry)
    if candidate.canonical_json(registry) != candidate.canonical_json(projection["registry"]):
        raise ValueError("Candidate contract registry differs from its selected source")
    return projection, tuple(copied_paths), snapshot_root


def _commit_source(root):
    _git(root, "init", "-q")
    _git(root, "add", "-A")
    env = os.environ.copy()
    env.update({
        "GIT_CONFIG_NOSYSTEM": "1",
        "GIT_AUTHOR_NAME": "Candidate Contract Fixture",
        "GIT_AUTHOR_EMAIL": "candidate-fixture@example.invalid",
        "GIT_AUTHOR_DATE": "2000-01-01T00:00:00Z",
        "GIT_COMMITTER_NAME": "Candidate Contract Fixture",
        "GIT_COMMITTER_EMAIL": "candidate-fixture@example.invalid",
        "GIT_COMMITTER_DATE": "2000-01-01T00:00:00Z",
    })
    _git(root, "-c", "user.name=Candidate Contract Fixture",
         "-c", "user.email=candidate-fixture@example.invalid",
         "-c", "commit.gpgSign=false", "commit", "-q", "-m", "fixture", env=env)
    return _git(root, "rev-parse", "HEAD").decode("ascii").strip()


@contextmanager
def _committed_source(snapshot_root):
    with tempfile.TemporaryDirectory(prefix="candidate-contract-source-", dir=snapshot_root.parent.parent) as temporary:
        checkout = Path(temporary) / "source"
        shutil.copytree(snapshot_root, checkout)
        revision = _commit_source(checkout)
        tree_bytes, index = candidate._git_tree(checkout, revision,
                                                candidate._registry()[1]["limits"])
        yield {
            "root": checkout,
            "revision": revision,
            "tree_bytes": tree_bytes,
            "tree_sha256": _hash(tree_bytes),
            "index": index,
        }


def _version_overlay(source):
    registry = candidate._registry()[1]
    seed_paths = registry["source_inputs"]["version_seed_recipe"]
    seed_digest, _seed_files = candidate._source_inputs(source["root"], source["index"], seed_paths)
    profile = candidate._config_profile(source["root"], source["revision"], source["index"])
    edits = {}
    rows = []
    for path, package in {**profile["packages"], **profile["locks"]}.items():
        _mode, before = candidate._git_blob(source["root"], source["index"], path)
        after = candidate.release.change_version(before.decode("utf-8"), path, package, "0.17.2").encode("utf-8")
        edits[path] = after
        rows.append({"path": path, "before_sha256": _hash(before), "after_sha256": _hash(after)})
    diff_digest = candidate._canonical_digest(sorted(rows, key=lambda row: row["path"].encode("utf-8")))
    return edits, seed_digest, diff_digest


def _closure_material():
    systems = candidate._registry()[1]["roles"]["nix_packages"]["systems"]
    outputs = candidate._registry()[1]["roles"]["nix_packages"]["outputs"]
    root_paths = {}
    number = 1
    for system in systems:
        root_paths[system] = {}
        for role in outputs:
            root_paths[system][role] = _store_path(number, "fixture-" + system + "-" + role)
            number += 1
    deps = [_store_path(1000 + i, "fixture-dependency-{:04d}".format(i)) for i in range(67)]
    edges = {path: [] for path in root_paths_by_flat(root_paths) | set(deps)}
    for system, roles in root_paths.items():
        edges[roles["neo"]] = [deps[0]]
        edges[roles["neoctl"]] = [deps[0]]
        edges[roles["pair"]] = sorted([roles["neo"], roles["neoctl"]])
    edges[deps[0]] = sorted(deps[1:66])
    for index in range(66, len(deps)):
        edges[deps[index - 65]].append(deps[index])
    for path in deps:
        edges[path].sort()
    all_paths = sorted(edges)
    quotient, remainder = divmod(EXPECTED_NAR_SIZE, len(all_paths))
    signatures = {
        "https://neohaskell.cachix.org": "neohaskell.cachix.org-1:" + base64.b64encode(b"\0" * 64).decode("ascii"),
        "https://cache.iog.io": "hydra.iohk.io:" + base64.b64encode(b"\1" * 64).decode("ascii"),
    }
    entries = []
    for index, path in enumerate(all_paths):
        nar = hashlib.sha256(("synthetic-nar:" + path).encode("utf-8")).digest()
        cache = "https://neohaskell.cachix.org" if path in root_paths_by_flat(root_paths) or index % 2 == 0 else "https://cache.iog.io"
        entries.append({
            "store_path": path,
            "nar_hash": "sha256-" + base64.b64encode(nar).decode("ascii"),
            "nar_size": quotient + (1 if index < remainder else 0),
            "references": edges[path],
            "locations": [{"cache": cache, "signatures": [signatures[cache]]}],
        })
    path_map = {entry["store_path"]: entry for entry in entries}
    closure_ids = {}
    for roles in root_paths.values():
        for role, root in roles.items():
            reachable = set()
            stack = [root]
            while stack:
                current = stack.pop()
                if current in reachable:
                    continue
                reachable.add(current)
                stack.extend(path_map[current]["references"])
            closure_ids[root] = candidate._closure_digest(sorted(reachable), path_map)
    table = {
        "schema_version": 1,
        "roots": root_paths,
        "paths": entries,
    }
    return root_paths, path_map, closure_ids, candidate.canonical_json(table)


def _closure_shape(raw):
    table = json.loads(raw)
    paths = table["paths"]
    return {
        "paths": len(paths),
        "max_references": max(len(item["references"]) for item in paths),
        "nar_size": sum(item["nar_size"] for item in paths),
    }


def root_paths_by_flat(roots):
    return {path for values in roots.values() for path in values.values()}


def _identity(name, content):
    return {"name": name, "sha256": _hash(content), "size": len(content)}


def _file_payloads(snapshot_root, projection, native_neo, native_installers, bootstraps):
    payloads = {}
    for name, content in native_neo.items():
        payloads[name] = content
    for name, content in native_installers.items():
        payloads[name] = content
    for role, content in bootstraps.items():
        name = projection["registry"]["roles"]["bootstraps"][role]["filename"]
        payloads[name] = content
    for name in projection["resolved"]["auxiliary_assets"]:
        if name in payloads:
            continue
        if name == "neo-starter-flake.nix":
            payloads[name] = (snapshot_root / "neo/starter/flake.nix").read_bytes()
        elif name == "neo-starter-flake.lock":
            payloads[name] = (snapshot_root / "neo/starter/flake.lock").read_bytes()
        elif name == "neo-starter-cabal.project":
            payloads[name] = (snapshot_root / "neo/starter/cabal.project").read_bytes()
        else:
            payloads[name] = candidate.canonical_json({"fixture": "PARSE ONLY", "name": name})
    return payloads


def _receipt_table(core):
    rows = []
    outputs = candidate._expected_output_receipts(core)
    for role, platform in sorted(outputs):
        entry, output_identity, reported_versions = outputs[(role, platform)]
        receipt = {
            "role": role,
            "platform": platform,
            "context_sha256": candidate._canonical_digest(core["context"]),
            "source_tree_sha256": core["context"]["source_tree_sha256"],
            "build_tree_sha256": core["context"]["build_tree_sha256"],
            "build_inputs_sha256": candidate._canonical_digest(core["build_inputs"]),
            "recipe_set_sha256": core["build_inputs"]["recipe_set_sha256"],
            "producer_sha256": core["build_inputs"]["recipe_set_sha256"],
            "version": core["version"],
            "builder_system": "synthetic-builder",
            "output_identity": output_identity,
            "verification_observations": {
                "argv_sha256": _hash((role + platform + "argv").encode("utf-8")),
                "reported_versions": reported_versions,
                "result_sha256": _hash((role + platform + "result").encode("utf-8")),
                "toolchain_sha256": _hash((role + platform + "toolchain").encode("utf-8")),
            },
        }
        receipt_hash = candidate._canonical_digest(receipt)
        if role == "native-neo":
            core["native_neo"][platform]["build_receipt_sha256"] = receipt_hash
        elif role == "native-installer":
            core["native_installers"][platform]["build_receipt_sha256"] = receipt_hash
        else:
            output_name = role[len("nix-"):]
            core["packages"][platform][output_name]["build_receipt_sha256"] = receipt_hash
        rows.append(receipt)
    return candidate.canonical_json({"schema_version": 1, "receipts": rows})


def _candidate_payload(version, source, snapshot_root, projection, closure):
    root_paths, path_map, closure_ids, closure_bytes = closure
    native_prefix = candidate.release.group_config("platform")["asset_prefix"]
    native_neo = {
        native_prefix + target: ("synthetic-neo:" + target + ":" + version).encode("utf-8")
        for target in projection["resolved"]["native_neo"]["targets"]
    }
    native_installers = {
        "neo-installer-" + target: ("synthetic-installer:" + target + ":" + version).encode("utf-8")
        for target in projection["registry"]["roles"]["native_installers"]["platforms"]
    }
    bootstraps = {
        "posix": b"#!/bin/sh\nexec neo-installer --candidate-locator \"$1\"\n",
        "powershell": b"param([Parameter(Mandatory=$true)][string]$CandidateLocator)\n& neo-installer --candidate-locator $CandidateLocator\n",
    }
    payloads = _file_payloads(snapshot_root, projection, native_neo, native_installers, bootstraps)
    if version == "0.17.1":
        overlays = {}
        build_tree_sha256 = source["tree_sha256"]
        version_seed = {"kind": "committed"}
    else:
        overlays, seed_recipe, diff_digest = _version_overlay(source)
        build_tree_sha256 = _hash(candidate._overlay_tree_bytes(source["tree_bytes"], overlays))
        version_seed = {
            "kind": "engineering-version",
            "version": version,
            "recipe_sha256": seed_recipe,
            "generated_diff_sha256": diff_digest,
            "build_tree_sha256": build_tree_sha256,
        }
    build_flake_lock = candidate._tree_file(
        source["root"], source["index"], projection["registry"]["source_inputs"]["build"]["flake_lock"], overlays,
    )
    build_cargo_lock = candidate._tree_file(
        source["root"], source["index"], projection["registry"]["source_inputs"]["build"]["cargo_lock"], overlays,
    )
    starter = {
        "framework_revision": FRAMEWORK_REVISION,
        "framework_nar_hash": json.loads((snapshot_root / "neo/starter/flake.lock").read_text())["nodes"]["neohaskell"]["locked"]["narHash"],
        "flake_sha256": _hash((snapshot_root / "neo/starter/flake.nix").read_bytes()),
        "lock_sha256": _hash((snapshot_root / "neo/starter/flake.lock").read_bytes()),
        "cabal_project_sha256": _hash((snapshot_root / "neo/starter/cabal.project").read_bytes()),
    }
    source_recipe, recipe_files = candidate._source_inputs(
        source["root"], source["index"], projection["registry"]["source_inputs"]["recipe"],
    )
    english = recipe_files[projection["registry"]["source_inputs"]["english_resource"]]
    generator = recipe_files[projection["registry"]["source_inputs"]["bootstrap_generator"]]
    auxiliary = {}
    bootstrap_names = {
        projection["registry"]["roles"]["bootstraps"][role]["filename"]
        for role in bootstraps
    }
    for name, content in payloads.items():
        if name not in native_neo and name not in native_installers and name not in bootstrap_names:
            auxiliary[name] = _identity(name, content)
    core = {
        "schema_version": 2,
        "policy": projection["registry"]["policy"],
        "contract_registry_sha256": candidate.contract_registry_sha256(),
        "context": {
            "kind": "engineering",
            "change_source": source["revision"],
            "build_revision": source["revision"],
            "source_tree_sha256": source["tree_sha256"],
            "build_tree_sha256": build_tree_sha256,
            "version_seed": version_seed,
        },
        "version": version,
        "build_inputs": {
            "flake_lock_sha256": _hash(build_flake_lock),
            "cargo_lock_sha256": _hash(build_cargo_lock),
            "recipe_set_sha256": source_recipe,
            "starter": starter,
        },
        "native_neo": {
            target: {"file": _identity(native_prefix + target, native_neo[native_prefix + target]),
                     "build_receipt_sha256": "0" * 64}
            for target in projection["resolved"]["native_neo"]["targets"]
        },
        "native_installers": {
            target: {"file": _identity("neo-installer-" + target, native_installers["neo-installer-" + target]),
                     "build_receipt_sha256": "0" * 64}
            for target in projection["registry"]["roles"]["native_installers"]["platforms"]
        },
        "bootstraps": {
            role: {
                "file": _identity(projection["registry"]["roles"]["bootstraps"][role]["filename"], content),
                "locator_input": projection["registry"]["roles"]["bootstraps"][role]["locator_input"],
                "english_source_sha256": _hash(english),
                "generator_recipe_sha256": _hash(generator),
            }
            for role, content in bootstraps.items()
        },
        "packages": {},
        "auxiliary_assets": auxiliary,
        "closures": _identity("closures.json", closure_bytes),
        "build_receipts": _identity("build-receipts.json", b"synthetic-placeholder"),
    }
    for system, outputs in root_paths.items():
        core["packages"][system] = {}
        for output, store_path in outputs.items():
            item = path_map[store_path]
            output_entry = {
                "store_path": store_path,
                "nar_hash": item["nar_hash"],
                "nar_size": item["nar_size"],
                "closure_id": closure_ids[store_path],
                "build_receipt_sha256": "0" * 64,
            }
            if output == "pair":
                output_entry["members"] = {
                    "neo": outputs["neo"],
                    "neoctl": outputs["neoctl"],
                }
            core["packages"][system][output] = output_entry
    receipts_bytes = _receipt_table(core)
    core["build_receipts"] = _identity("build-receipts.json", receipts_bytes)
    core_bytes = candidate.canonical_json(core)
    materialized = {
        **payloads,
        "closures.json": closure_bytes,
        "build-receipts.json": receipts_bytes,
        "candidate.json": core_bytes,
    }
    return core, core_bytes, materialized


def _canonical_path(path):
    result = Path(path).expanduser()
    if not result.is_absolute():
        result = Path.cwd() / result
    for alias in (Path("/tmp"), Path("/var"), Path("/etc")):
        if result == alias or alias in result.parents:
            resolved_alias = alias.resolve(strict=True)
            if resolved_alias != alias:
                result = resolved_alias / result.relative_to(alias)
            break
    return result


def _reject_symlink_components(path):
    current = Path(path.anchor)
    for part in path.parts[1:]:
        current = current / part
        try:
            info = current.lstat()
        except FileNotFoundError:
            continue
        if stat.S_ISLNK(info.st_mode):
            raise ValueError("Fixture output path contains a symlink: " + str(current))


def _inside(path, directory):
    try:
        path.relative_to(directory)
        return True
    except ValueError:
        return False


def _fresh_output_root(source_root, output_root):
    output_root = _canonical_path(output_root)
    _reject_symlink_components(output_root)
    output_root = output_root.resolve(strict=False)
    if output_root.exists() or output_root.is_symlink():
        raise ValueError("Fixture output root must not exist: " + str(output_root))
    if not output_root.parent.is_dir():
        raise ValueError("Fixture output parent must be an existing directory")
    protected = (
        ROOT / ".agents",
        ROOT / "neo/lifecycle/contract",
        source_root / ".agents",
        source_root / "neo/lifecycle/contract",
    )
    for directory in protected:
        if _inside(output_root, _canonical_path(directory)):
            raise ValueError("Fixture output root is inside a maintained source directory")
    return output_root


def _generate_corpus(source_root, output_root):
    projection, source_paths, snapshot_root = _write_source_snapshot(source_root, output_root)
    current = output_root / "current-0.17.2"
    previous = output_root / "previous-0.17.1"
    with _committed_source(snapshot_root) as source:
        closure = _closure_material()
        observed = _closure_shape(closure[3])
        expected = {
            "paths": EXPECTED_PATHS,
            "max_references": EXPECTED_MAX_REFERENCES,
            "nar_size": EXPECTED_NAR_SIZE,
        }
        if observed != expected:
            raise ValueError("Generated closure shape changed")
        for directory, version in ((previous, "0.17.1"), (current, "0.17.2")):
            core, core_bytes, files = _candidate_payload(
                version, source, snapshot_root, projection, closure,
            )
            directory.mkdir()
            for name, content in files.items():
                (directory / name).write_bytes(content)
        manifest = {
            "schema_version": 1,
            "parse_only": True,
            "source_revision": source["revision"],
            "source_tree_sha256": source["tree_sha256"],
            "candidate_ids": {
                "previous-0.17.1": _hash((previous / "candidate.json").read_bytes()),
                "current-0.17.2": _hash((current / "candidate.json").read_bytes()),
            },
            "closure_shape": observed,
            "synthetic_metadata_only": True,
        }
        (output_root / "fixture-set.json").write_bytes(candidate.canonical_json(manifest))
    return source_paths


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-root", type=Path, default=ROOT)
    parser.add_argument("--output-root", required=True, type=Path)
    args = parser.parse_args(argv)
    try:
        corpus = CandidateFixtureCorpus(args.source_root, args.output_root)
    except (OSError, ValueError) as error:
        parser.exit(1, str(error) + "\n")
    print(corpus.root)


if __name__ == "__main__":
    main()
