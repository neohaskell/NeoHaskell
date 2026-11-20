"""Build and verify one release's installer and signed Nix package closure."""
import base64
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

import candidate
import engine as release
from assemble_candidate import TARGETS, file_identity


SYSTEMS = {target: system for system, target in TARGETS.items()}
CACHE_NAME = "neohaskell"
PRIMARY_CACHE = "https://neohaskell.cachix.org"


def output(*command, **kwargs):
    return subprocess.check_output(command, text=True, **kwargs).strip()


def observe_versions(commands, version, revision, target, toolchain):
    arguments = []
    results = []
    versions = {}
    for name, executable in commands.items():
        argv = [name, "--version"]
        expected = name + " " + version
        if name == "neo-installer":
            expected += f" ({revision}, {target})"
        result = subprocess.run([executable, "--version"], capture_output=True, check=True)
        if result.stdout != (expected + "\n").encode() or result.stderr:
            raise ValueError(f"{name} did not report the exact release identity {expected}")
        arguments.append(argv)
        results.append({"stdout": result.stdout.decode(), "stderr": "", "status": result.returncode})
        versions[name] = version
    return {
        "argv_sha256": release.digest(candidate.canonical_json(arguments)),
        "result_sha256": release.digest(candidate.canonical_json(results)),
        "toolchain_sha256": release.digest(candidate.canonical_json(toolchain)),
        "reported_versions": versions,
    }


def build_installer(repo, revision, target, workspace, out):
    executable = "neo-installer"
    subprocess.run([
        "cargo", "build", "--release", "--locked", "--manifest-path", str(repo.root / "neo/Cargo.toml"),
        "--target", target, "--package", "neo-lifecycle", "--bin", executable,
    ], check=True, env={**os.environ, "NEO_SOURCE_REVISION": revision, "CARGO_TARGET_DIR": str(workspace)})
    path = out / f"{executable}-{target}"
    shutil.copy2(workspace / target / "release" / executable, path)
    if target.endswith("-darwin"):
        subprocess.run([
            "/usr/bin/codesign", "--force", "--sign", "-", "--timestamp=none",
            "--options", "runtime,library", "--identifier", "org.neohaskell.neo-installer", path,
        ], check=True)
        subprocess.run(["/usr/bin/codesign", "--verify", "--strict", path], check=True)
    subprocess.run([release.SKILL_ROOT / "scripts/neo-release", "inspect", path, target], check=True)
    return path


def path_info_args(*options):
    return ["nix", "path-info", "--json", "--json-format", "1", "--sigs", *options]


def remote_info(cache, path):
    args = path_info_args("--option", "narinfo-cache-negative-ttl", "0", "--store", cache, path)
    result = subprocess.run(args, capture_output=True, text=True)
    if result.returncode:
        return None
    return json.loads(result.stdout)[path]


def verify_metadata_signature(path, metadata, key, signatures):
    # Nix considers content-addressed paths trusted independently of their signatures.
    nar_hash = output("nix", "hash", "convert", "--hash-algo", "sha256", "--to", "nix32", metadata["narHash"])
    references = ",".join(sorted(metadata["references"]))
    fingerprint = f"1;{path};sha256:{nar_hash};{metadata['narSize']};{references}"
    with tempfile.TemporaryDirectory() as temporary:
        directory = Path(temporary)
        key_file = directory / "key.der"
        # RFC 8410 SubjectPublicKeyInfo header for a 32-byte Ed25519 key.
        key_file.write_bytes(bytes.fromhex("302a300506032b6570032100") + base64.b64decode(
            key.split(":", 1)[1], validate=True))
        message = directory / "fingerprint"
        message.write_bytes(fingerprint.encode())
        signature_file = directory / "signature"
        for signature in signatures:
            signature_file.write_bytes(base64.b64decode(signature.split(":", 1)[1], validate=True))
            subprocess.run([
                "openssl", "pkeyutl", "-verify", "-rawin", "-pubin", "-keyform", "DER",
                "-inkey", key_file, "-in", message, "-sigfile", signature_file,
            ], check=True, stdout=subprocess.DEVNULL)


def closure_rows(local, roots, caches):
    rows = []
    used = {}
    for path, metadata in sorted(local.items()):
        for entry in caches:
            cache = entry["cache"]
            if path in roots.values() and cache != PRIMARY_CACHE:
                continue
            info = remote_info(cache, path)
            if not info:
                continue
            if (info["narHash"], info["narSize"], sorted(info["references"])) != (
                metadata["narHash"], metadata["narSize"], sorted(metadata["references"])
            ):
                raise ValueError(f"Remote NAR identity differs from the built output at {cache}: {path}")
            prefix = entry["key"].split(":", 1)[0] + ":"
            signatures = sorted({sig for sig in info.get("signatures", []) if sig.startswith(prefix)})
            if not signatures:
                continue
            verify_metadata_signature(path, metadata, entry["key"], signatures)
            used.setdefault(cache, []).append(path)
            rows.append({
                "store_path": path, "nar_hash": metadata["narHash"], "nar_size": metadata["narSize"],
                "references": sorted(metadata["references"]),
                "locations": [{"cache": cache, "signatures": signatures}],
            })
            break
        else:
            raise ValueError(f"No approved signed cache serves {path}")
    for entry in caches:
        paths = used.get(entry["cache"], [])
        if paths:
            subprocess.run([
                "nix", "store", "verify", "--store", entry["cache"],
                "--option", "trusted-public-keys", entry["key"], "--sigs-needed", "1", "--stdin",
            ], input="\n".join(paths) + "\n", text=True, check=True)
    return rows


def publish_closure(roots, system, version, caches, cache_environment):
    pair = roots["pair"]
    local = json.loads(output(*path_info_args("--recursive", pair)))
    if not set(roots.values()) <= local.keys():
        raise ValueError("The combined package does not include both independently built tools")
    subprocess.run(["cachix", "push", CACHE_NAME, pair], check=True, env=cache_environment)
    rows = closure_rows(local, roots, caches)
    subprocess.run(["cachix", "pin", CACHE_NAME, f"neo-{version}-{system}", pair, "--keep-forever"], check=True, env=cache_environment)
    return rows


def build(repo, plan, revision, target, inputs, out, native_neo=None):
    cache_environment = os.environ.copy()
    token = os.environ.pop("CACHIX_AUTH_TOKEN", None)
    if not token:
        raise ValueError("CACHIX_AUTH_TOKEN is required to publish and pin the release packages")
    try:
        _build(repo, plan, revision, target, inputs, out, native_neo, cache_environment)
    finally:
        os.environ["CACHIX_AUTH_TOKEN"] = token


def _build(repo, plan, revision, target, inputs, out, native_neo, cache_environment):
    system = SYSTEMS[target]
    version = plan["version"]
    if bool(native_neo) != (target in release.targets_for(plan)):
        raise ValueError("A downloaded native neo is required exactly on the release's standalone CLI targets")
    if output("nix", "eval", "--raw", "--impure", "--expr", "builtins.currentSystem") != system:
        raise ValueError("This release requires a native runner for " + system)
    if plan.get("contract_registry_sha256") != candidate.contract_registry_sha256():
        raise ValueError("Installer policy differs from the frozen release plan")
    starter = repo.root / "neo/starter"
    starter_hashes = {}
    for name in ("flake.nix", "flake.lock", "cabal.project"):
        data = (starter / name).read_bytes()
        if data != (inputs / ("neo-starter-" + name)).read_bytes():
            raise ValueError("Build checkout differs from the frozen starter input " + name)
        starter_hashes[name] = release.digest(data)
    toolchain = {"cargo": output("cargo", "--version"), "rustc": output("rustc", "--version", "--verbose")}
    nix_toolchain = {"nix": output("nix", "--version"), "flake_lock_sha256": file_identity(repo.root / "flake.lock")["sha256"]}
    caches = candidate._registry()[1]["trusted_cache_keys"]
    out.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory() as temporary:
        workspace = Path(temporary)
        installer = build_installer(repo, revision, target, workspace / "target", out)
        observations = {
            "native-installer": observe_versions(
                {"neo-installer": installer.resolve()}, version, revision, target, toolchain
            ),
        }
        native = {"native-installer": file_identity(installer)}
        if native_neo:
            archive = Path(native_neo)
            if archive.name != f"neo-{target}":
                raise ValueError("Unexpected native neo release asset name")
            binary = workspace / archive.name
            shutil.copyfile(archive, binary)
            binary.chmod(0o755)
            native["native-neo"] = file_identity(binary)
            observations["native-neo"] = observe_versions({"neo": binary}, version, revision, target, toolchain)
        roots = {}
        for role, attribute in (("pair", "neo-pair"), ("neo", "neo"), ("neoctl", "neoctl")):
            link = workspace / role
            subprocess.run([
                "nix", "build", "--accept-flake-config", "--no-update-lock-file",
                "--out-link", link, f".#packages.{system}.{attribute}",
            ], cwd=repo.root, check=True)
            root = link.resolve(strict=True)
            roots[role] = str(root)
            programs = ("neo", "neoctl") if role == "pair" else (role,)
            observations["nix-" + role] = observe_versions(
                {name: root / "bin" / name for name in programs}, version, revision, target, nix_toolchain
            )
        for name in ("neo", "neoctl"):
            if not (Path(roots["pair"]) / "bin" / name).samefile(Path(roots[name]) / "bin" / name):
                raise ValueError("Combined package selected a different binary for " + name)
        paths = publish_closure(roots, system, version, caches, cache_environment)
    report = {
        "attempt": plan["id"], "revision": revision, "version": version, "target": target,
        "system": system, "roots": roots, "paths": paths, "observations": observations,
        "native": native, "starter_hashes": starter_hashes,
    }
    (out / f"platform-{target}.json").write_bytes(candidate.canonical_json(report))
