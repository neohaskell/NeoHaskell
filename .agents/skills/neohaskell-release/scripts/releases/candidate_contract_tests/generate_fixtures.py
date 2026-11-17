#!/usr/bin/env python3

import base64
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
from contextlib import contextmanager


RELEASES = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(RELEASES))
import candidate


FIXTURES = Path(__file__).parent / "fixtures/v1"
SOURCE = FIXTURES / "source"
CURRENT = FIXTURES / "current-0.17.2"
PREVIOUS = FIXTURES / "previous-0.17.1"
ROOT = candidate.release.ROOT
NIX_BASE32 = "0123456789abcdfghijklmnpqrsvwxyz"
FRAMEWORK_REVISION = "ea75873fbd0da7da39124c82f328c2e70bdca629"
OBSERVED_PATHS = 599
OBSERVED_MAX_REFERENCES = 65
OBSERVED_NAR_SIZE = 5044109824


def _hash(data):
    return hashlib.sha256(data).hexdigest()


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


def _source_paths(registry):
    paths = set(registry["source_inputs"]["recipe"])
    paths.add(registry["source_inputs"]["build"]["flake_lock"])
    paths.add(registry["source_inputs"]["build"]["cargo_lock"])
    paths.update(registry["source_inputs"]["build"]["starter"].values())
    config_path = ROOT / candidate.CONFIG_RELATIVE_PATH
    config = json.loads(config_path.read_text(encoding="utf-8"))
    profile = config["groups"]["platform"]
    paths.update(profile["packages"])
    paths.update(profile["locks"])
    return sorted(paths)


def _write_source_snapshot():
    if SOURCE.exists():
        shutil.rmtree(SOURCE)
    projection = candidate.contract_registry_projection()
    paths = _source_paths(projection["registry"])
    for relative in paths:
        destination = SOURCE / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        if relative == projection["registry"]["source_inputs"]["bootstrap_generator"]:
            live = ROOT / relative
            content = live.read_bytes() if live.is_file() else b"synthetic-generator-recipe-v1\n"
        else:
            content = (ROOT / relative).read_bytes()
        destination.write_bytes(content)
    return projection, paths


@contextmanager
def _committed_source(paths):
    with tempfile.TemporaryDirectory(prefix="candidate-contract-source-") as temporary:
        checkout = Path(temporary) / "source"
        shutil.copytree(SOURCE, checkout)
        _git(checkout, "init", "-q")
        _git(checkout, "add", "-A")
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
        _git(checkout, "-c", "user.name=Candidate Contract Fixture",
             "-c", "user.email=candidate-fixture@example.invalid",
             "-c", "commit.gpgSign=false", "commit", "-q", "-m", "fixture", env=env)
        revision = _git(checkout, "rev-parse", "HEAD").decode("ascii").strip()
        tree_bytes, index = candidate._git_tree(checkout, revision,
                                                projection_limits())
        if len(paths) != len(set(paths)):
            raise ValueError("Fixture source recipe paths contain duplicates")
        yield_root = Path(temporary) / "source"
        recipe_digest, recipe_files = candidate._source_inputs(
            yield_root, index, candidate.contract_registry_projection()["registry"]["source_inputs"]["recipe"]
        )
        yield {
            "root": yield_root,
            "revision": revision,
            "tree_bytes": tree_bytes,
            "tree_sha256": _hash(tree_bytes),
            "index": index,
            "recipe_sha256": recipe_digest,
            "recipe_files": recipe_files,
        }


def projection_limits():
    return candidate._registry()[1]["limits"]


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
    raw = json.loads((ROOT / ".agents-state/installer-implementation/local-tool-closures.json").read_text())
    path_count = len(raw)
    max_references = max(len(item["references"]) for item in raw.values())
    nar_size = sum(item["narSize"] for item in raw.values())
    if (path_count, max_references, nar_size) != (
            OBSERVED_PATHS, OBSERVED_MAX_REFERENCES, OBSERVED_NAR_SIZE):
        raise ValueError("Saved local closure no longer matches the recorded stress shape")
    systems = candidate._registry()[1]["roles"]["nix_packages"]["systems"]
    outputs = candidate._registry()[1]["roles"]["nix_packages"]["outputs"]
    root_paths = {}
    number = 1
    for system in systems:
        root_paths[system] = {}
        for role in outputs:
            root_paths[system][role] = _store_path(number, "fixture-" + system + "-" + role)
            number += 1
    deps = [_store_path(1000 + i, "fixture-dependency-{:04d}".format(i)) for i in range(590)]
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
    quotient, remainder = divmod(OBSERVED_NAR_SIZE, len(all_paths))
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


def root_paths_by_flat(roots):
    return {path for values in roots.values() for path in values.values()}


def _identity(name, content):
    return {"name": name, "sha256": _hash(content), "size": len(content)}


def _file_payloads(source, projection, native_neo, native_installers, bootstraps):
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
            payloads[name] = (SOURCE / "neo/starter/flake.nix").read_bytes()
        elif name == "neo-starter-flake.lock":
            payloads[name] = (SOURCE / "neo/starter/flake.lock").read_bytes()
        elif name == "neo-starter-cabal.project":
            payloads[name] = (SOURCE / "neo/starter/cabal.project").read_bytes()
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


def _candidate_payload(version, source, projection, closure):
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
    payloads = _file_payloads(source, projection, native_neo, native_installers, bootstraps)
    profile = candidate._config_profile(source["root"], source["revision"], source["index"])
    seed_paths = projection["registry"]["source_inputs"]["version_seed_recipe"]
    seed_recipe, _seed_files = candidate._source_inputs(source["root"], source["index"], seed_paths)
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
        "framework_nar_hash": json.loads((SOURCE / "neo/starter/flake.lock").read_text())["nodes"]["neohaskell"]["locked"]["narHash"],
        "flake_sha256": _hash((SOURCE / "neo/starter/flake.nix").read_bytes()),
        "lock_sha256": _hash((SOURCE / "neo/starter/flake.lock").read_bytes()),
        "cabal_project_sha256": _hash((SOURCE / "neo/starter/cabal.project").read_bytes()),
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


def _invalid_cases(projection, core, closure):
    cases = []
    for target in sorted(core["native_neo"]):
        cases.append({"name": "missing-native-neo-" + target, "document": "candidate", "operation": "remove", "path": ["native_neo", target]})
    for target in sorted(core["native_installers"]):
        cases.append({"name": "missing-native-installer-" + target, "document": "candidate", "operation": "remove", "path": ["native_installers", target]})
    for system in sorted(core["packages"]):
        for role in sorted(core["packages"][system]):
            cases.append({"name": "missing-nix-" + system + "-" + role, "document": "candidate", "operation": "remove", "path": ["packages", system, role]})
    cases.extend([
        {"name": "missing-posix-bootstrap", "document": "candidate", "operation": "remove", "path": ["bootstraps", "posix"]},
        {"name": "missing-powershell-bootstrap", "document": "candidate", "operation": "remove", "path": ["bootstraps", "powershell"]},
        {"name": "missing-nix-platform-x86_64-linux", "document": "candidate", "operation": "remove", "path": ["packages", "x86_64-linux"]},
        {"name": "unknown-nix-platform", "document": "candidate", "operation": "add", "path": ["packages", "unknown-system"], "value": {}},
        {"name": "unknown-native-installer-platform", "document": "candidate", "operation": "add", "path": ["native_installers", "unknown-platform"], "value": {}},
        {"name": "bad-nar-sri", "document": "candidate", "operation": "replace", "path": ["packages", "x86_64-linux", "neo", "nar_hash"], "value": "sha256-not-base64"},
        {
            "name": "wrong-pair-member",
            "document": "candidate",
            "operation": "replace",
            "path": ["packages", "x86_64-linux", "pair", "members", "neo"],
            "value": core["packages"]["x86_64-linux"]["neoctl"]["store_path"],
        },
        {
            "name": "derivation-is-not-an-output",
            "document": "candidate",
            "operation": "replace-with-suffix",
            "path": ["packages", "x86_64-linux", "neo", "store_path"],
            "suffix": ".drv",
        },
        {
            "name": "flake-expression-is-not-an-output",
            "document": "candidate",
            "operation": "replace",
            "path": ["packages", "x86_64-linux", "neo", "store_path"],
            "value": 'builtins.getFlake(".")#neo',
        },
        {
            "name": "unlisted-closure-reference",
            "document": "closures",
            "operation": "replace-reference",
            "path": next(
                ["paths", index, "references", 0]
                for index, item in enumerate(closure["paths"])
                if item["references"]
            ),
            "value": "/nix/store/" + "z" * 32 + "-missing-reference",
        },
        {
            "name": "unreachable-closure-path",
            "document": "closures",
            "operation": "append-unreachable-path",
            "store_path": "/nix/store/" + "y" * 32 + "-unreachable-extra",
        },
        {
            "name": "native-output-byte-flip",
            "document": "file",
            "operation": "flip-byte",
            "file": next(iter(core["native_installers"].values()))["file"]["name"],
            "offset": 0,
        },
        {
            "name": "native-output-wrong-size",
            "document": "candidate",
            "operation": "increment-file-size",
            "path": ["native_installers", next(iter(core["native_installers"])), "file", "size"],
            "amount": 1,
        },
        {
            "name": "unlisted-materialized-file",
            "document": "artifact-directory",
            "operation": "add-file",
            "file": "unlisted.txt",
        },
        {
            "name": "wrong-source-tree",
            "document": "candidate",
            "operation": "replace-and-refresh-receipts",
            "path": ["context", "source_tree_sha256"],
            "value": "0" * 64,
        },
        {
            "name": "wrong-engineering-generated-diff",
            "document": "candidate",
            "operation": "replace-and-refresh-receipts",
            "path": ["context", "version_seed", "generated_diff_sha256"],
            "value": "0" * 64,
        },
        {
            "name": "engineering-context-as-release",
            "document": "candidate",
            "operation": "retag-engineering-as-release",
        },
        {
            "name": "missing-bootstrap-generator-source",
            "document": "source",
            "operation": "remove-file",
            "file": projection["registry"]["source_inputs"]["bootstrap_generator"],
        },
        {
            "name": "forged-receipt-source-tree",
            "document": "build-receipts",
            "operation": "replace-and-refresh-output-link",
            "path": ["receipts", 0, "source_tree_sha256"],
            "value": "0" * 64,
        },
        {
            "name": "duplicate-json-key",
            "document": "candidate.json",
            "operation": "replace-raw",
            "value": '{"schema_version":2,"schema_version":2}',
        },
        {
            "name": "noncanonical-json-newline",
            "document": "candidate.json",
            "operation": "append-bytes",
            "value": "\\n",
        },
    ])
    return {"schema_version": 1, "fixture": "current-0.17.2", "parse_only": True, "cases": cases}


def main():
    FIXTURES.mkdir(parents=True, exist_ok=True)
    for directory in (CURRENT, PREVIOUS):
        if directory.exists():
            shutil.rmtree(directory)
    projection, paths = _write_source_snapshot()
    with _committed_source(paths) as source:
        closure_raw = json.loads((ROOT / ".agents-state/installer-implementation/local-tool-closures.json").read_text())
        observed = {
            "paths": len(closure_raw),
            "max_references": max(len(item["references"]) for item in closure_raw.values()),
            "nar_size": sum(item["narSize"] for item in closure_raw.values()),
        }
        if observed != {"paths": OBSERVED_PATHS, "max_references": OBSERVED_MAX_REFERENCES, "nar_size": OBSERVED_NAR_SIZE}:
            raise ValueError("Saved local closure statistics changed")
        closure = _closure_material()
        for directory, version in ((PREVIOUS, "0.17.1"), (CURRENT, "0.17.2")):
            core, core_bytes, files = _candidate_payload(version, source, projection, closure)
            directory.mkdir(parents=True)
            for name, content in files.items():
                (directory / name).write_bytes(content)
        current_core = json.loads((CURRENT / "candidate.json").read_bytes())
        closure_table = json.loads((CURRENT / "closures.json").read_bytes())
        (FIXTURES / "invalid-cases.json").write_bytes(
            candidate.canonical_json(_invalid_cases(projection, current_core, closure_table))
        )
        manifest = {
            "schema_version": 1,
            "parse_only": True,
            "source_revision": source["revision"],
            "source_tree_sha256": source["tree_sha256"],
            "candidate_ids": {
                "previous-0.17.1": _hash((PREVIOUS / "candidate.json").read_bytes()),
                "current-0.17.2": _hash((CURRENT / "candidate.json").read_bytes()),
            },
            "closure_shape": observed,
            "synthetic_metadata_only": True,
        }
        (FIXTURES / "fixture-set.json").write_bytes(candidate.canonical_json(manifest))
    print(FIXTURES)


if __name__ == "__main__":
    main()
