"""Build and verify one release's installer and signed Nix package closure."""
import base64
from contextlib import contextmanager
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time

import candidate
import engine as release
from assemble_candidate import TARGETS, file_identity


SYSTEMS = {target: system for system, target in TARGETS.items()}
CACHE_NAME = "neohaskell"
PRIMARY_CACHE = "https://neohaskell.cachix.org"
TOOLCHAIN_LINK = Path("share/neohaskell/toolchain")


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


def verify_metadata_signature(openssl, path, metadata, key, signatures):
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
                openssl, "pkeyutl", "-verify", "-rawin", "-pubin", "-keyform", "DER",
                "-inkey", key_file, "-in", message, "-sigfile", signature_file,
            ], check=True, stdout=subprocess.DEVNULL)


def closure_rows(local, roots, caches, openssl):
    rows = []
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
            verify_metadata_signature(openssl, path, metadata, entry["key"], signatures)
            rows.append({
                "store_path": path, "nar_hash": metadata["narHash"], "nar_size": metadata["narSize"],
                "references": sorted(metadata["references"]),
                "locations": [{"cache": cache, "signatures": signatures}],
            })
            break
        else:
            raise ValueError(f"No approved signed cache serves {path}")
    return rows


def nar_identities(metadata):
    return {
        path: (item["narHash"], item["narSize"], sorted(item["references"]))
        for path, item in metadata.items()
    }


@contextmanager
def isolated_store_directory(workspace):
    with tempfile.TemporaryDirectory(dir=workspace, prefix="acquire-") as temporary:
        directory = Path(temporary).resolve(strict=True)
        try:
            yield directory
        finally:
            for current, _, _ in os.walk(directory, followlinks=False):
                Path(current).chmod(0o700)


def verify_clean_store(root, local, system, caches, workspace):
    with isolated_store_directory(workspace) as isolated:
        store = f"local?root={isolated / 'store'}"
        retained = isolated / "pair"
        if json.loads(output(*path_info_args("--store", store, "--all"))):
            raise ValueError("Expected an empty Nix store before acquiring " + root)
        trusted = " ".join(cache["key"] for cache in caches)
        substituters = " ".join(cache["cache"] for cache in caches)
        command = [
            "nix", "build", "--store", store, "--option", "max-jobs", "0",
            "--option", "builders", "", "--option", "require-sigs", "true",
            "--option", "trusted-public-keys", trusted, "--out-link", retained,
        ]
        started = time.monotonic()
        result = subprocess.run([
            *command, "--option", "substituters", substituters,
            "--option", "narinfo-cache-negative-ttl", "0", root,
        ])
        elapsed_ms = round((time.monotonic() - started) * 1000)
        if result.returncode:
            raise ValueError(f"Could not acquire {root} for {system} from signed caches with builders disabled")
        acquired = json.loads(output(*path_info_args("--store", store, "--recursive", root)))
        expected = nar_identities(local)
        if nar_identities(acquired) != expected or str(retained.readlink()) != root:
            raise ValueError(f"The acquired closure or registered root differs for {system}: {root}")
        subprocess.run(["nix", "store", "gc", "--store", store], check=True)
        started = time.monotonic()
        subprocess.run([*command, "--offline", "--option", "substituters", "", root], check=True)
        post_gc_ms = round((time.monotonic() - started) * 1000)
        after_gc = json.loads(output(*path_info_args("--store", store, "--recursive", root)))
        if nar_identities(after_gc) != expected:
            raise ValueError(f"Garbage collection changed the retained closure for {system}: {root}")
        return {
            "closure_paths": len(acquired),
            "installed_nar_bytes": sum(item["narSize"] for item in acquired.values()),
            "acquisition_ms": elapsed_ms,
            "post_gc_ms": post_gc_ms,
            "post_gc_download_paths": 0,
            "build_recipes": 0,
        }


def pair_closure(pair, toolchain, local, system):
    direct = local[pair]["references"]
    if [path for path in direct if Path(path).name.split("-", 1)[-1] == "neo-toolchain"] != [toolchain]:
        raise ValueError(f"The {system} pair does not directly reference the exact starter toolchain {toolchain}")
    limit = candidate._registry()[1]["limits"]["max_references_per_path"]
    for path, metadata in local.items():
        count = len(metadata["references"])
        if count > limit:
            raise ValueError(f"The {system} output {path} has {count} direct references. The installer supports at most {limit}.")
    return local


def publish_closure(roots, toolchain, system, version, caches, cache_environment, workspace, openssl):
    pair = roots["pair"]
    local = pair_closure(pair, toolchain, json.loads(output(*path_info_args("--recursive", pair))), system)
    if not set(roots.values()) <= local.keys():
        raise ValueError("The combined package does not include both independently built tools")
    subprocess.run(["cachix", "push", CACHE_NAME, pair], check=True, env=cache_environment)
    rows = closure_rows(local, {**roots, "toolchain": toolchain}, caches, openssl)
    acquisition = verify_clean_store(pair, local, system, caches, workspace)
    subprocess.run(["cachix", "pin", CACHE_NAME, f"neo-{version}-{system}", pair, "--keep-forever"], check=True, env=cache_environment)
    return rows, acquisition


def build_output(repo, reference, link):
    subprocess.run([
        "nix", "build", "--accept-flake-config", "--no-update-lock-file", "--out-link", link, reference,
    ], cwd=repo.root, check=True)
    return str(link.resolve(strict=True))


def toolchain_output(repo, system, workspace):
    return build_output(repo, f"./neo/starter#packages.{system}.neo-toolchain", workspace / "neo-toolchain")


def verify_toolchain_link(pair, toolchain, system):
    if (Path(pair) / TOOLCHAIN_LINK).resolve(strict=True) != Path(toolchain):
        raise ValueError(f"The {system} pair exposes a different toolchain than the starter output {toolchain}")


def smoke_toolchain(repo, pair, toolchain):
    smoke = release.SKILL_ROOT.parent / "neohaskell-consumer-verification/scripts/neo-toolchain-smoke"
    subprocess.run([smoke, "--toolchain", toolchain, "--neo", Path(pair) / "bin" / "neo"], cwd=repo.root, check=True)


def rehearse_toolchain(repo, system, out):
    if output("nix", "eval", "--raw", "--impure", "--expr", "builtins.currentSystem") != system:
        raise ValueError("Toolchain rehearsal requires a native runner for " + system)
    out.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory() as temporary:
        workspace = Path(temporary)
        toolchain = toolchain_output(repo, system, workspace)
        pair = build_output(repo, f".#packages.{system}.neo-pair", workspace / "neo-pair")
        verify_toolchain_link(pair, toolchain, system)
        local = pair_closure(pair, toolchain, json.loads(output(*path_info_args("--recursive", pair))), system)
        secret = workspace / "cache-key"
        secret.touch(mode=0o600)
        with secret.open("w") as stream:
            subprocess.run(["nix", "key", "generate-secret", "--key-name", "neo-test"], stdout=stream, check=True)
        with secret.open() as stream:
            public = output("nix", "key", "convert-secret-to-public", stdin=stream)
        cache = (workspace / "cache").as_uri()
        subprocess.run(["nix", "copy", "--to", f"{cache}?secret-key={secret}&compression=zstd", pair], check=True)
        acquisition = verify_clean_store(pair, local, system, [{"cache": cache, "key": public}], workspace)
        archives = json.loads(output(*path_info_args("--store", cache, "--recursive", pair)))
        acquisition["compressed_nar_bytes"] = sum(entry["downloadSize"] for entry in archives.values())
        smoke_toolchain(repo, pair, toolchain)
    report = {"system": system, "pair": pair, "toolchain": toolchain, "acquisition": acquisition}
    (out / f"toolchain-{system}.json").write_bytes(candidate.canonical_json(report))


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
        openssl_output = output(
            "nix", "build", "--accept-flake-config", "--no-update-lock-file",
            "--out-link", workspace / "openssl", "--print-out-paths",
            f".#legacyPackages.{system}.openssl.bin", cwd=repo.root,
        )
        openssl = (Path(openssl_output) / "bin" / "openssl").resolve(strict=True)
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
        toolchain = toolchain_output(repo, system, workspace)
        for role, attribute in (("pair", "neo-pair"), ("neo", "neo"), ("neoctl", "neoctl")):
            roots[role] = build_output(repo, f".#packages.{system}.{attribute}", workspace / role)
            root = Path(roots[role])
            programs = ("neo", "neoctl") if role == "pair" else (role,)
            observations["nix-" + role] = observe_versions(
                {name: root / "bin" / name for name in programs}, version, revision, target, nix_toolchain
            )
        for name in ("neo", "neoctl"):
            if not (Path(roots["pair"]) / "bin" / name).samefile(Path(roots[name]) / "bin" / name):
                raise ValueError("Combined package selected a different binary for " + name)
        verify_toolchain_link(roots["pair"], toolchain, system)
        paths, acquisition = publish_closure(
            roots, toolchain, system, version, caches, cache_environment, workspace, openssl
        )
        smoke_toolchain(repo, roots["pair"], toolchain)
    report = {
        "attempt": plan["id"], "revision": revision, "version": version, "target": target,
        "system": system, "roots": roots, "paths": paths, "observations": observations,
        "native": native, "starter_hashes": starter_hashes,
        "toolchain": toolchain, "acquisition": acquisition,
    }
    (out / f"platform-{target}.json").write_bytes(candidate.canonical_json(report))
