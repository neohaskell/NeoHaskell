#!/usr/bin/env python3
"""Compare installed neo and neoctl version and startup behavior."""

from __future__ import annotations

import argparse
import base64
import hashlib
import json
import math
import os
import signal
import subprocess
import sys
import tempfile
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any


MAX_CAPTURED_STREAM_BYTES = 65_536
MAX_SNAPSHOT_ENTRIES = 256
MAX_HASHED_FILE_BYTES = 1_048_576
TERMINATION_GRACE_SECONDS = 1.0


@dataclass(frozen=True)
class Case:
    name: str
    program: Path
    arguments: tuple[str, ...]
    expected_exit_code: int
    expected_stdout: bytes
    expected_stderr: bytes
    forbidden_output: tuple[bytes, ...] = ()


@dataclass
class StreamCapture:
    data: bytearray
    byte_count: int = 0

    def read_from(self, stream: Any, limit_exceeded: threading.Event) -> None:
        while True:
            block = stream.read(65_536)
            if not block:
                return
            self.byte_count += len(block)
            available = MAX_CAPTURED_STREAM_BYTES - len(self.data)
            if available > 0:
                self.data.extend(block[:available])
            if len(block) > available:
                limit_exceeded.set()


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Compare neo and neoctl from explicit built executable paths."
    )
    parser.add_argument("--neo", required=True, help="path to the neo executable")
    parser.add_argument("--neoctl", required=True, help="path to the neoctl executable")
    parser.add_argument(
        "--expected-version", required=True, help="version both executables must report"
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=10.0,
        help="maximum seconds for each process, default 10",
    )
    arguments = parser.parse_args()
    if not arguments.expected_version.strip() or any(
        character in arguments.expected_version for character in "\r\n"
    ):
        parser.error("--expected-version must be a nonempty single-line value")
    if not math.isfinite(arguments.timeout) or arguments.timeout <= 0:
        parser.error("--timeout must be a positive finite number")
    arguments.neo = resolve_executable(parser, arguments.neo, "--neo")
    arguments.neoctl = resolve_executable(parser, arguments.neoctl, "--neoctl")
    return arguments


def resolve_executable(parser: argparse.ArgumentParser, value: str, option: str) -> Path:
    try:
        executable = Path(value).expanduser().resolve(strict=True)
    except OSError as error:
        parser.error(f"{option} does not name an existing executable: {error}")
    if not executable.is_file() or (os.name != "nt" and not os.access(executable, os.X_OK)):
        parser.error(f"{option} must name an executable file")
    return executable


def invocation_cases(neo: Path, neoctl: Path, expected_version: str) -> list[Case]:
    usage = b"Usage: neoctl [--help|-h|--version]\nDisplay usage and packaged version information for neoctl.\n"
    invalid = b"Invalid arguments. Run neoctl --help for usage.\n"
    return [
        Case(
            "neo-version",
            neo,
            ("--version",),
            0,
            f"neo {expected_version}\n".encode(),
            b"",
        ),
        Case(
            "neoctl-version",
            neoctl,
            ("--version",),
            0,
            f"neoctl {expected_version}\n".encode(),
            b"",
        ),
        Case("neoctl-no-arguments", neoctl, (), 0, usage, b""),
        Case("neoctl-help", neoctl, ("--help",), 0, usage, b""),
        Case("neoctl-short-help", neoctl, ("-h",), 0, usage, b""),
        Case("neoctl-unknown-flag", neoctl, ("--bogus",), 2, b"", invalid, (b"--bogus",)),
        Case("neoctl-doctor", neoctl, ("doctor",), 2, b"", invalid, (b"doctor",)),
        Case(
            "neoctl-mixed-help-arguments",
            neoctl,
            ("--help", "unexpected"),
            2,
            b"",
            invalid,
            (b"unexpected",),
        ),
    ]


def process_environment() -> dict[str, str]:
    environment = os.environ.copy()
    environment.pop("LOG_LEVEL", None)
    environment.pop("CI", None)
    environment["NO_COLOR"] = "1"
    return environment


def stop_process(process: subprocess.Popen[bytes]) -> None:
    if os.name == "nt":
        if process.poll() is None:
            process.terminate()
    else:
        try:
            os.killpg(process.pid, signal.SIGTERM)
        except ProcessLookupError:
            if process.poll() is None:
                return
    try:
        process.wait(timeout=TERMINATION_GRACE_SECONDS)
        return
    except subprocess.TimeoutExpired:
        pass
    if os.name == "nt":
        if process.poll() is None:
            process.kill()
    else:
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            if process.poll() is None:
                return
    if process.poll() is None:
        process.wait(timeout=TERMINATION_GRACE_SECONDS)

def run_process(case: Case, working_directory: Path, timeout: float) -> dict[str, Any]:
    command = [str(case.program), *case.arguments]
    started_at = time.monotonic()
    try:
        process = subprocess.Popen(
            command,
            cwd=working_directory,
            env=process_environment(),
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            close_fds=True,
            start_new_session=os.name != "nt",
            creationflags=getattr(subprocess, "CREATE_NEW_PROCESS_GROUP", 0)
            if os.name == "nt"
            else 0,
        )
    except OSError as error:
        return {
            "command": command,
            "exit_code": None,
            "timed_out": False,
            "duration_ms": round((time.monotonic() - started_at) * 1000, 1),
            "stdout": b"",
            "stdout_byte_count": 0,
            "stderr": b"",
            "stderr_byte_count": 0,
            "stream_limit_exceeded": False,
            "stream_readers_stuck": False,
            "launch_error": str(error),
        }

    stdout = StreamCapture(bytearray())
    stderr = StreamCapture(bytearray())
    stream_limit_exceeded = threading.Event()
    readers = [
        threading.Thread(
            target=stdout.read_from,
            args=(process.stdout, stream_limit_exceeded),
            daemon=True,
        ),
        threading.Thread(
            target=stderr.read_from,
            args=(process.stderr, stream_limit_exceeded),
            daemon=True,
        ),
    ]
    for reader in readers:
        reader.start()

    timed_out = False
    deadline = started_at + timeout
    while process.poll() is None:
        if stream_limit_exceeded.is_set():
            stop_process(process)
            break
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            timed_out = True
            stop_process(process)
            break
        try:
            process.wait(timeout=min(remaining, 0.1))
        except subprocess.TimeoutExpired:
            pass

    for reader in readers:
        reader.join(timeout=TERMINATION_GRACE_SECONDS)
    readers_stuck = any(reader.is_alive() for reader in readers)
    if readers_stuck:
        stop_process(process)
        for reader in readers:
            reader.join(timeout=TERMINATION_GRACE_SECONDS)

    return {
        "command": command,
        "exit_code": process.returncode,
        "timed_out": timed_out,
        "duration_ms": round((time.monotonic() - started_at) * 1000, 1),
        "stdout": bytes(stdout.data),
        "stdout_byte_count": stdout.byte_count,
        "stderr": bytes(stderr.data),
        "stderr_byte_count": stderr.byte_count,
        "stream_limit_exceeded": stream_limit_exceeded.is_set(),
        "stream_readers_stuck": readers_stuck,
        "launch_error": None,
    }


def snapshot_files(root: Path) -> tuple[dict[str, dict[str, Any]], bool]:
    snapshot: dict[str, dict[str, Any]] = {}
    overflow = False
    for path in sorted(root.rglob("*")):
        if len(snapshot) >= MAX_SNAPSHOT_ENTRIES:
            overflow = True
            break
        relative_path = path.relative_to(root).as_posix()
        if path.is_symlink():
            snapshot[relative_path] = {
                "kind": "symlink",
                "target": os.readlink(path),
            }
        elif path.is_dir():
            snapshot[relative_path] = {"kind": "directory"}
        elif path.is_file():
            size = path.stat().st_size
            digest = None
            if size <= MAX_HASHED_FILE_BYTES:
                hasher = hashlib.sha256()
                with path.open("rb") as file_handle:
                    for block in iter(lambda: file_handle.read(65_536), b""):
                        hasher.update(block)
                digest = hasher.hexdigest()
            snapshot[relative_path] = {
                "kind": "file",
                "size": size,
                "sha256": digest,
            }
        else:
            snapshot[relative_path] = {"kind": "other"}
    return snapshot, overflow


def changed_files(
    before: dict[str, dict[str, Any]], after: dict[str, dict[str, Any]]
) -> list[dict[str, Any]]:
    changes = []
    for relative_path in sorted(before.keys() | after.keys()):
        previous = before.get(relative_path)
        current = after.get(relative_path)
        if previous != current:
            changes.append(
                {"path": relative_path, "before": previous, "after": current}
            )
    return changes


def encode_stream(stream: bytes, byte_count: int) -> dict[str, Any]:
    captured = stream[:MAX_CAPTURED_STREAM_BYTES]
    return {
        "byte_count": byte_count,
        "base64": base64.b64encode(captured).decode("ascii"),
        "text": captured.decode("utf-8", errors="replace"),
        "truncated": byte_count > MAX_CAPTURED_STREAM_BYTES,
    }


def verify_case(
    case: Case, working_directory: Path, timeout: float
) -> tuple[dict[str, Any], list[str]]:
    before, before_overflow = snapshot_files(working_directory)
    process = run_process(case, working_directory, timeout)
    after, after_overflow = snapshot_files(working_directory)
    stdout = process.pop("stdout")
    stderr = process.pop("stderr")
    stdout_byte_count = process.pop("stdout_byte_count")
    stderr_byte_count = process.pop("stderr_byte_count")
    file_changes = changed_files(before, after)
    failures = []

    if process["launch_error"] is not None:
        failures.append(f"could not start {case.name}: {process['launch_error']}")
    if process["timed_out"]:
        failures.append(f"{case.name} exceeded the {timeout:g} second timeout")
    if process["stream_limit_exceeded"]:
        failures.append(f"{case.name} exceeded the raw stream capture limit")
    if process["stream_readers_stuck"]:
        failures.append(f"{case.name} left an output stream open after exit")
    if process["exit_code"] != case.expected_exit_code:
        failures.append(
            f"{case.name} exited {process['exit_code']}, expected {case.expected_exit_code}"
        )
    if stdout != case.expected_stdout:
        failures.append(f"{case.name} stdout did not match the expected bytes")
    if stderr != case.expected_stderr:
        failures.append(f"{case.name} stderr did not match the expected bytes")
    if before_overflow or after_overflow:
        failures.append(f"{case.name} exceeded the file snapshot limit")
    if file_changes:
        failures.append(f"{case.name} changed files in its working directory")
    for forbidden in case.forbidden_output:
        if forbidden in stdout or forbidden in stderr:
            failures.append(f"{case.name} echoed a raw argument")

    observation = {
        "name": case.name,
        **process,
        "stdout": encode_stream(stdout, stdout_byte_count),
        "stderr": encode_stream(stderr, stderr_byte_count),
        "files_before": before,
        "files_after": after,
        "file_changes": file_changes,
        "passed": not failures,
        "failures": failures,
    }
    return observation, failures


def run_pair(arguments: argparse.Namespace) -> int:
    cases = invocation_cases(arguments.neo, arguments.neoctl, arguments.expected_version)
    observations = []
    failures = []
    with tempfile.TemporaryDirectory(prefix="neoctl-tool-pair-") as temporary:
        root = Path(temporary)
        for case in cases:
            working_directory = root / case.name
            working_directory.mkdir()
            observation, case_failures = verify_case(
                case, working_directory, arguments.timeout
            )
            observations.append(observation)
            failures.extend(case_failures)

    report = {
        "status": "FAIL" if failures else "PASS",
        "expected_version": arguments.expected_version,
        "observations": observations,
        "failures": failures,
    }
    print(json.dumps(report, indent=2, sort_keys=True))
    return 1 if failures else 0


def main() -> int:
    arguments = parse_arguments()
    return run_pair(arguments)


if __name__ == "__main__":
    sys.exit(main())
