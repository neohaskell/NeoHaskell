#!/usr/bin/env python3
import argparse
from dataclasses import dataclass
import hashlib
import json
from pathlib import Path
import re
import shlex


TARGETS = {
    "Linux:x86_64": "x86_64-unknown-linux-gnu",
    "Linux:aarch64": "aarch64-unknown-linux-gnu",
    "Darwin:arm64": "aarch64-apple-darwin",
}
VERSION = re.compile(r"(?:0|[1-9][0-9]*)\.(?:0|[1-9][0-9]*)\.(?:0|[1-9][0-9]*)\Z")


@dataclass(frozen=True)
class Artifact:
    name: str
    size: int
    sha256: str

    @classmethod
    def read(cls, path):
        digest = hashlib.sha256()
        size = 0
        with path.open("rb") as source:
            while chunk := source.read(1024 * 1024):
                size += len(chunk)
                if size > 256 * 1024 * 1024:
                    raise ValueError("Native installer exceeds 256 MiB")
                digest.update(chunk)
        if not size:
            raise ValueError("Native installer is empty")
        return cls(path.name, size, digest.hexdigest())


POSIX = r'''#!/bin/sh
set -u
umask 077
LC_ALL=C
export LC_ALL
version=@VERSION@
base="https://github.com/neohaskell/NeoHaskell/releases/download/neo-v$version"
@MESSAGES@
fail() {
    printf '%s\n' "$1" >&2
    exit 1
}
for tool in curl uname mktemp chmod rm rmdir wc tr cmp cat; do
    command -v "$tool" >/dev/null 2>&1 || fail "$bootstrap_tools"
done
if command -v sha256sum >/dev/null 2>&1; then
    hash_file() { sha256sum < "$1"; }
elif command -v shasum >/dev/null 2>&1; then
    hash_file() { shasum -a 256 < "$1"; }
else
    fail "$bootstrap_tools"
fi
case "$(uname -s):$(uname -m)" in
@TARGETS@
    *) fail "$bootstrap_platform" ;;
esac
stage=$(mktemp -d "${TMPDIR:-/tmp}/neohaskell-bootstrap.XXXXXXXXXX") || fail "$bootstrap_staging"
cleanup() {
    result=$?
    trap - 0 HUP INT TERM
    rm -f "$stage/candidate.locator" "$stage/locator.expected" "$stage/neo-installer"
    rmdir "$stage"
    exit "$result"
}
trap cleanup 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM
fetch() (
    limit=$3
    ulimit -f $(( (limit + 511) / 512 )) || exit 1
    curl -q --fail --silent --show-error --location \
        --proto '=https' --proto-redir '=https' --max-redirs 5 \
        --connect-timeout 15 --max-time 120 --retry 2 --retry-delay 1 \
        --retry-max-time 250 --max-filesize "$limit" \
        --output "$2" "$1"
)
printf '%s\n' "$bootstrap_preparing" >&2
if [ "${1-}" = --candidate-locator ]; then
    [ "$#" -ge 2 ] || fail "$bootstrap_locator"
    locator=$2
    shift 2
else
    fetch "$base/candidate.locator" "$stage/candidate.locator" 128 || fail "$bootstrap_download"
    [ "$(wc -c < "$stage/candidate.locator" | tr -d ' ')" -le 128 ] || fail "$bootstrap_locator"
    locator=$(cat "$stage/candidate.locator") || fail "$bootstrap_locator"
    printf '%s\n' "$locator" > "$stage/locator.expected" || fail "$bootstrap_staging"
    cmp -s "$stage/candidate.locator" "$stage/locator.expected" || fail "$bootstrap_locator"
fi
case "$locator" in
    "$version"@*) candidate_sha=${locator#*@} ;;
    *) fail "$bootstrap_locator" ;;
esac
[ "${#candidate_sha}" -eq 64 ] || fail "$bootstrap_locator"
case "$candidate_sha" in
    *[!0-9a-f]*) fail "$bootstrap_locator" ;;
esac
fetch "$base/$asset" "$stage/neo-installer" "$size" || fail "$bootstrap_download"
[ "$(wc -c < "$stage/neo-installer" | tr -d ' ')" = "$size" ] || fail "$bootstrap_integrity"
actual=$(hash_file "$stage/neo-installer") || fail "$bootstrap_integrity"
actual=${actual%% *}
[ "$actual" = "$sha256" ] || fail "$bootstrap_integrity"
chmod 700 "$stage/neo-installer" || fail "$bootstrap_staging"
if [ -t 1 ] && ( : < /dev/tty ) 2>/dev/null; then
    "$stage/neo-installer" install --candidate-locator "$locator" "$@" < /dev/tty
else
    "$stage/neo-installer" install --candidate-locator "$locator" "$@"
fi
exit "$?"
'''


def generate_posix(version, artifacts, messages):
    rows = []
    for platform, target in TARGETS.items():
        artifact = artifacts[target]
        rows.append(f"    {platform}) asset={artifact.name}; size={artifact.size}; sha256={artifact.sha256} ;;")
    assignments = "\n".join(
        f"{key}={shlex.quote(value)}" for key, value in sorted(messages.items())
        if key.startswith("bootstrap_") and isinstance(value, str)
    )
    return (POSIX.replace("@VERSION@", shlex.quote(version))
            .replace("@MESSAGES@", assignments)
            .replace("@TARGETS@", "\n".join(rows))).encode("utf-8")


def candidate_locator(path, version):
    with path.open("rb") as source:
        raw = source.read(2 * 1024 * 1024 + 1)
    if len(raw) > 2 * 1024 * 1024:
        raise ValueError("Candidate exceeds 2 MiB")
    value = json.loads(raw)
    canonical = json.dumps(value, sort_keys=True, ensure_ascii=False,
                           separators=(",", ":"), allow_nan=False).encode("utf-8")
    if not isinstance(value, dict) or raw != canonical or value.get("version") != version:
        raise ValueError("Candidate must be canonical JSON with the requested version")
    return f"{version}@{hashlib.sha256(raw).hexdigest()}\n".encode("ascii")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--version", required=True)
    parser.add_argument("--artifacts-dir", type=Path)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--candidate-json", type=Path)
    args = parser.parse_args()
    if not VERSION.fullmatch(args.version) or len(args.version) > 32:
        parser.error("version must be a stable X.Y.Z version of at most 32 characters")
    if args.candidate_json:
        payload = candidate_locator(args.candidate_json, args.version)
        args.output_dir.mkdir(parents=True, exist_ok=True)
        (args.output_dir / "candidate.locator").write_bytes(payload)
        return
    if args.artifacts_dir is None:
        parser.error("--artifacts-dir is required when generating bootstraps")
    artifacts = {target: Artifact.read(args.artifacts_dir / f"neo-installer-{target}")
                 for target in TARGETS.values()}
    messages = json.loads((Path(__file__).resolve().parents[1] / "src/english.json").read_text())
    payload = generate_posix(args.version, artifacts, messages)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "install").write_bytes(payload)
    (args.output_dir / "install").chmod(0o755)


if __name__ == "__main__":
    main()
