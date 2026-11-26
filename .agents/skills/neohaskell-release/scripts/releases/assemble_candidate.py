"""Assemble the installer release from immutable preparation inputs and native build reports."""
import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

import candidate
import engine


TARGETS = {
    "aarch64-darwin": "aarch64-apple-darwin",
    "aarch64-linux": "aarch64-unknown-linux-gnu",
    "x86_64-linux": "x86_64-unknown-linux-gnu",
}
STARTER = {"flake": "flake.nix", "lock": "flake.lock", "cabal_project": "cabal.project"}


def file_identity(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return {"name": path.name, "sha256": digest.hexdigest(), "size": path.stat().st_size}


def write_metadata(directory, name, value):
    path = directory / name
    path.write_bytes(candidate.canonical_json(value))
    return file_identity(path)


def release_inputs(root, plan, revision, directory, registry):
    source = plan["source_sha"]
    inputs = registry["source_inputs"]
    source_tree, source_index = candidate._git_tree(root, source, registry["limits"])
    prep_tree, prep_index = candidate._git_tree(root, revision, registry["limits"])
    recipe_digest, _ = candidate._source_inputs(root, source_index, inputs["recipe"])
    overlays = {}
    starter = {}
    for role, name in STARTER.items():
        raw = (directory / ("neo-starter-" + name)).read_bytes()
        overlays[inputs["build"]["starter"][role]] = raw
        starter[role + "_sha256"] = candidate._sha256(raw)
    lock = json.loads(overlays[inputs["build"]["starter"]["lock"]])
    framework = lock["nodes"]["neohaskell"]["locked"]
    starter.update(framework_revision=framework["rev"], framework_nar_hash=framework["narHash"])
    build = {"recipe_set_sha256": recipe_digest, "starter": starter}
    for field in ("flake_lock", "cargo_lock"):
        build[field + "_sha256"] = candidate._sha256(
            candidate._git_blob(root, prep_index, inputs["build"][field])[1])
    context = {
        "kind": "release", "change_source": source, "build_revision": revision,
        "plan_id": plan["id"], "source_tree_sha256": candidate._tree_digest(source_tree),
        "build_tree_sha256": candidate._tree_digest(candidate._overlay_tree_bytes(prep_tree, overlays)),
        "seed_receipt_sha256": candidate._canonical_digest({
            "schema_version": 1, "plan_id": plan["id"], "build_revision": revision, **starter}),
    }
    return context, build


def add_path(paths, row):
    path = row["store_path"]
    if path not in paths:
        paths[path] = row
        return
    existing = paths[path]
    if candidate._closure_identity(existing) != candidate._closure_identity(row):
        raise ValueError("Conflicting NAR metadata for " + path)
    locations = {}
    for location in existing["locations"] + row["locations"]:
        locations.setdefault(location["cache"], set()).update(location["signatures"])
    existing["locations"] = [{"cache": cache, "signatures": sorted(signatures)}
                             for cache, signatures in sorted(locations.items())]


def reachable(paths, root):
    result = set()
    pending = [root]
    while pending:
        path = pending.pop()
        if path not in result:
            result.add(path)
            if path not in paths:
                raise ValueError("Missing reference in platform closure: " + path)
            pending.extend(paths[path]["references"])
    return result


def build_manifest(root, plan, revision, directory, platforms):
    projection = candidate.contract_registry_projection()
    registry, resolved = projection["registry"], projection["resolved"]
    context, build = release_inputs(root, plan, revision, directory, registry)
    core = {
        "schema_version": 2, "version": plan["version"], "policy": registry["policy"],
        "contract_registry_sha256": candidate.contract_registry_sha256(),
        "context": context, "build_inputs": build,
        "native_installers": {}, "native_neo": {}, "bootstraps": {}, "packages": {},
        "auxiliary_assets": {name: file_identity(directory / name) for name in resolved["auxiliary_assets"]},
    }
    paths, roots, observations = {}, {}, {}
    toolchains = set()
    starter_hashes = {name: build["starter"][role + "_sha256"] for role, name in STARTER.items()}
    for system in registry["roles"]["nix_packages"]["systems"]:
        target = TARGETS[system]
        report = json.loads((platforms / ("platform-" + target + ".json")).read_bytes())
        expected = {"attempt": plan["id"], "revision": revision, "version": plan["version"],
                    "system": system, "target": target, "starter_hashes": starter_hashes}
        if any(report[key] != value for key, value in expected.items()):
            raise ValueError("Native build identity or starter inputs differ for " + system)
        roots[system] = report["roots"]
        if set(roots[system]) != set(registry["roles"]["nix_packages"]["outputs"]):
            raise ValueError("Native build is missing package outputs for " + system)
        platform_paths = {row["store_path"]: row for row in report["paths"]}
        reachable_paths = set.union(*(reachable(platform_paths, path) for path in roots[system].values()))
        if len(platform_paths) != len(report["paths"]) or reachable_paths != set(platform_paths):
            raise ValueError("Unexpected or duplicate closure path in native report for " + system)
        pair = roots[system]["pair"]
        toolchain = candidate._valid_store_path(report.get("toolchain"), registry["limits"])
        if (Path(toolchain).name.split("-", 1)[-1] != "neo-toolchain"
                or toolchain in toolchains
                or toolchain not in platform_paths[pair]["references"]
                or not any(
                    location["cache"] == "https://neohaskell.cachix.org"
                    for location in platform_paths[toolchain]["locations"]
                )):
            raise ValueError("Missing or inconsistent published starter toolchain for " + system)
        toolchains.add(toolchain)
        acquisition = report["acquisition"]
        pair_paths = reachable(platform_paths, pair)
        if (acquisition["build_recipes"] != 0 or acquisition["post_gc_download_paths"] != 0
                or acquisition["closure_paths"] != len(pair_paths)
                or acquisition["installed_nar_bytes"] != sum(platform_paths[path]["nar_size"] for path in pair_paths)):
            raise ValueError("Incomplete clean-store acquisition evidence for " + system)
        for row in platform_paths.values():
            add_path(paths, row)
        filenames = {"native-installer": "neo-installer-" + target}
        if target in resolved["native_neo"]["targets"]:
            filenames["native-neo"] = "neo-" + target
        roles = set(filenames) | {"nix-" + role for role in roots[system]}
        if set(report["observations"]) != roles or set(report["native"]) != set(filenames):
            raise ValueError("Native report has incorrect executable observations for " + system)
        for role, name in filenames.items():
            if role == "native-installer":
                shutil.copyfile(platforms / name, directory / name)
            identity = file_identity(directory / name)
            if report["native"][role] != identity:
                raise ValueError("Observed binary differs from release asset " + name)
            field = "native_installers" if role == "native-installer" else "native_neo"
            core[field][target] = {"file": identity}
        for role, observation in report["observations"].items():
            observations[(role, target if role in filenames else system)] = (system, observation)
    for system, outputs in roots.items():
        packages = {}
        for role, path in outputs.items():
            row = paths[path]
            packages[role] = {key: row[key] for key in ("store_path", "nar_hash", "nar_size")}
            packages[role]["closure_id"] = candidate._closure_digest(reachable(paths, path), paths)
        packages["pair"]["members"] = {name: outputs[name] for name in ("neo", "neoctl")}
        core["packages"][system] = packages
    core["closures"] = write_metadata(directory, "closures.json", {
        "schema_version": 1, "roots": roots, "paths": [paths[path] for path in sorted(paths)]})
    receipts = []
    for key, (entry, identity, _) in sorted(candidate._expected_output_receipts(core).items()):
        role, platform = key
        system, observation = observations[key]
        receipt = {"role": role, "platform": platform, "builder_system": system,
                   "version": plan["version"], "output_identity": identity,
                   "verification_observations": observation,
                   "context_sha256": candidate._canonical_digest(context),
                   "build_inputs_sha256": candidate._canonical_digest(build),
                   "source_tree_sha256": context["source_tree_sha256"],
                   "build_tree_sha256": context["build_tree_sha256"],
                   "producer_sha256": build["recipe_set_sha256"],
                   "recipe_set_sha256": build["recipe_set_sha256"]}
        entry["build_receipt_sha256"] = candidate._canonical_digest(receipt)
        receipts.append(receipt)
    core["build_receipts"] = write_metadata(directory, "build-receipts.json", {
        "schema_version": 1, "receipts": receipts})
    generator = root / registry["source_inputs"]["bootstrap_generator"]
    generate = [sys.executable, str(generator), "--version", plan["version"], "--output-dir", str(directory)]
    subprocess.run([*generate, "--artifacts-dir", str(directory)], check=True)
    english = file_identity(root / registry["source_inputs"]["english_resource"])["sha256"]
    recipe = file_identity(generator)["sha256"]
    for role, bootstrap in registry["roles"]["bootstraps"].items():
        core["bootstraps"][role] = {"file": file_identity(directory / bootstrap["filename"]),
                                    "generator_recipe_sha256": recipe, "english_source_sha256": english,
                                    "locator_input": bootstrap["locator_input"]}
    write_metadata(directory, "candidate.json", core)
    parsed = candidate.parse_candidate((directory / "candidate.json").read_bytes(), directory)
    candidate.bind_source(parsed, root)
    subprocess.run([*generate, "--candidate-json", str(directory / "candidate.json")], check=True)


def assemble(root, attempt, revision, artifacts, platforms):
    repo = engine.Repo(root)
    plan = next((p for p in repo.ledger(revision)["attempts"] if p["id"] == attempt), None)
    if plan is None or plan.get("distribution_policy") != "installer-v1":
        raise ValueError("Expected an installer release attempt in the preparation ledger")
    engine.verify_generated(repo, revision, plan)
    baseline = candidate.contract_registry_projection()["resolved"]["required_assets"]
    with tempfile.TemporaryDirectory(dir=artifacts.parent) as tmp:
        directory = Path(tmp)
        for name in baseline:
            shutil.copyfile(artifacts / name, directory / name)
        build_manifest(root, plan, revision, directory, platforms)
        for file in directory.iterdir():
            if file.name not in baseline:
                file.replace(artifacts / file.name)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--attempt", required=True)
    parser.add_argument("--revision", required=True)
    parser.add_argument("--artifacts", type=Path, required=True)
    parser.add_argument("--platforms", type=Path, required=True)
    args = parser.parse_args()
    assemble(args.root.resolve(), args.attempt, args.revision, args.artifacts, args.platforms)
