"""Export the PARSE-only runtime contract. No source binding or readiness is granted.

Run this file with --write to refresh neo/lifecycle/contract, or --check to
byte-compare the complete generated file set without modifying it.
"""
import argparse
import hashlib
from pathlib import Path

import candidate


HERE = Path(__file__).resolve().parent
CASES = HERE / "candidate_contract_tests/runtime_cases.json"
FROZEN = HERE / "candidate_contract_tests/fixtures/v1"
DESTINATION = candidate.release.ROOT / "neo/lifecycle/contract"


def digest(raw):
    return hashlib.sha256(raw).hexdigest()


def expected_files():
    registry = candidate.contract_registry_bytes()
    outputs = {
        "registry.json": registry,
        "runtime_cases.json": CASES.read_bytes(),
        "fixtures/v1/fixture-set.json": (FROZEN / "fixture-set.json").read_bytes(),
    }
    fixture_set = candidate._loads(outputs["fixtures/v1/fixture-set.json"], 65536, 24, 1000)
    for name in sorted(fixture_set["candidate_ids"]):
        for path in sorted((FROZEN / name).iterdir()):
            if path.is_symlink() or not path.is_file():
                raise ValueError("Frozen parse input is not a regular file")
            outputs["fixtures/v1/" + name + "/" + path.name] = path.read_bytes()
    sources = [
        Path(candidate.__file__), Path(__file__), CASES, candidate.CONTRACT_PATH,
        Path(candidate.release.__file__), Path(candidate.remote.__file__),
        candidate.SKILL_ROOT / "references/config.json",
        candidate.release.ROOT / "flake.nix",
    ]
    sources.extend(sorted(path for path in FROZEN.rglob("*") if path.is_file()))
    root = candidate.release.ROOT
    source_hashes = {str(path.relative_to(root)): digest(path.read_bytes()) for path in sources}
    outputs["manifest.json"] = candidate.canonical_json({
        "schema_version": 1,
        "contract_registry_sha256": candidate.contract_registry_sha256(),
        "sources": source_hashes,
        "outputs": {name: {"sha256": digest(raw), "size": len(raw)} for name, raw in sorted(outputs.items())},
    })
    if len(outputs) > 128 or sum(map(len, outputs.values())) > 128 * 1024 * 1024:
        raise ValueError("Runtime export exceeds its bounds")
    return outputs


def safe_destination(destination):
    for path in (destination, *destination.parents):
        if path.is_symlink():
            raise ValueError("Runtime export destination contains a symlink")
    if destination.exists():
        for path in destination.rglob("*"):
            if path.is_symlink() or not (path.is_file() or path.is_dir()):
                raise ValueError("Runtime export contains a non-regular entry")


def check(destination=DESTINATION):
    safe_destination(destination)
    expected = expected_files()
    actual = {str(path.relative_to(destination)) for path in destination.rglob("*") if path.is_file() or path.is_symlink()}
    if actual != set(expected):
        raise ValueError("Runtime export file set drift")
    for name, raw in expected.items():
        path = destination / name
        if path.is_symlink() or path.read_bytes() != raw:
            raise ValueError("Runtime export byte drift: " + name)


def write(destination=DESTINATION):
    safe_destination(destination)
    expected = expected_files()
    if destination.exists():
        actual = {str(path.relative_to(destination)) for path in destination.rglob("*") if path.is_file() or path.is_symlink()}
        if actual - set(expected):
            raise ValueError("Unexpected runtime export files; inspect before removal")
    for name, raw in expected.items():
        path = destination / name
        path.parent.mkdir(parents=True, exist_ok=True)
        if path.is_symlink():
            raise ValueError("Runtime export destination is a symlink")
        path.write_bytes(raw)
    check(destination)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--write", action="store_true")
    mode.add_argument("--check", action="store_true")
    args = parser.parse_args()
    try:
        write() if args.write else check()
    except (ValueError, OSError) as error:
        parser.exit(1, str(error) + "\n")
    print("runtime contract export matches " + candidate.contract_registry_sha256())


if __name__ == "__main__":
    main()
