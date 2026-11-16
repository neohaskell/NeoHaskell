#!/usr/bin/env python3
"""Run disposable Docker lanes that prove systemd can manage a service as jess."""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import os
import shlex
import shutil
import subprocess
import sys
import time
import uuid
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Sequence


@dataclass(frozen=True)
class ArchitectureLane:
    name: str
    platform: str
    kernel_arch: str
    package_arch: str


LANES = {
    "x86_64": ArchitectureLane("x86_64", "linux/amd64", "x86_64", "amd64"),
    "aarch64": ArchitectureLane("aarch64", "linux/arm64", "aarch64", "arm64"),
}
BASE_IMAGE = "ubuntu:24.04"
BUILD_TIMEOUT_SECONDS = 1200
STARTUP_TIMEOUT_SECONDS = 180
ROOT = Path(__file__).resolve().parents[4]
RUNNER = Path(__file__).resolve().parent
DOCKERFILE = RUNNER / "Dockerfile"
EVIDENCE = ROOT / ".agents-state/installer-implementation/linux-runner"


def now() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat()


def after(seconds: float) -> str:
    return (dt.datetime.now(dt.timezone.utc) + dt.timedelta(seconds=seconds)).isoformat()


def save(path: Path, data: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    os.replace(temporary, path)


def call(args: Sequence[str], allow_failure: bool = False) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(list(args), capture_output=True, text=True, check=False)
    if result.returncode and not allow_failure:
        detail = (result.stderr or result.stdout).strip()
        raise RuntimeError(
            f"{shlex.join(args)} exited with {result.returncode}"
            + (f": {detail[-2000:]}" if detail else "")
        )
    return result


def output(args: Sequence[str]) -> str:
    return call(args).stdout.strip()


def operation(path: Path, state: dict[str, Any], **fields: Any) -> None:
    state.update(fields)
    state["updated_at"] = now()
    save(path, state)


def parse_fields(text: str) -> dict[str, str]:
    fields: dict[str, str] = {}
    for line in text.splitlines():
        if "=" in line:
            key, value = line.split("=", maxsplit=1)
            fields[key] = value
    return fields


def build_image(lane: ArchitectureLane, image_tag: str, state_path: Path, state: dict[str, Any]) -> None:
    command = ["docker", "build", "--pull", "--platform", lane.platform, "--progress=plain",
               "--tag", image_tag, "--file", str(DOCKERFILE), str(RUNNER)]
    deadline = time.monotonic() + BUILD_TIMEOUT_SECONDS
    operation(
        state_path,
        state,
        phase="image-build",
        lane=lane.name,
        image_tag=image_tag,
        process_id=None,
        command=shlex.join(command),
        deadline_at=after(BUILD_TIMEOUT_SECONDS),
        next_check_at=None,
        current_backoff_seconds=None,
    )
    print(f"Building {image_tag} for {lane.platform}", flush=True)
    process = subprocess.Popen(command)
    operation(state_path, state, process_id=process.pid)
    try:
        code = process.wait(timeout=max(0.0, deadline - time.monotonic()))
    except subprocess.TimeoutExpired:
        process.terminate()
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
        raise RuntimeError(f"Docker build exceeded {BUILD_TIMEOUT_SECONDS} seconds")
    except KeyboardInterrupt:
        process.terminate()
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
        raise
    if code:
        raise RuntimeError(f"Docker build exited with {code} for {lane.platform}")


def image_identity(image_tag: str, lane: ArchitectureLane) -> dict[str, Any]:
    raw = output(["docker", "image", "inspect", "--format",
                  "{{.Id}}|{{.Os}}|{{.Architecture}}|{{json .RepoDigests}}", image_tag])
    image_id, os_name, architecture, raw_digests = raw.split("|", maxsplit=3)
    if os_name != "linux" or architecture != lane.package_arch:
        raise RuntimeError(f"Built image is linux/{architecture}; expected {lane.platform}")
    return {
        "base_image_tag": BASE_IMAGE,
        "image_id": image_id,
        "image_os": os_name,
        "image_architecture": architecture,
        "repo_digests": json.loads(raw_digests),
    }


def container_logs(container_id: str) -> str:
    result = call(["docker", "logs", "--tail", "60", container_id], allow_failure=True)
    return (result.stdout + result.stderr).strip()[-6000:]


def wait_for_systemd(
    lane: ArchitectureLane,
    container_id: str,
    timeout: int,
    state_path: Path,
    state: dict[str, Any],
) -> dict[str, str]:
    deadline = time.monotonic() + timeout
    delay = 1
    while True:
        operation(
            state_path,
            state,
            phase="systemd-readiness",
            lane=lane.name,
            container_id=container_id,
            deadline_at=after(max(0, deadline - time.monotonic())),
            next_check_at=now(),
            current_backoff_seconds=delay,
        )
        status, exit_code = output(["docker", "inspect", "--format",
                                    "{{.State.Status}}|{{.State.ExitCode}}", container_id]).split("|", maxsplit=1)
        if status != "running":
            raise RuntimeError(
                f"Container stopped in state {status!r} with exit code {exit_code}. {container_logs(container_id)}"
            )
        ready = call(["docker", "exec", container_id, "systemctl", "is-active", "--quiet", "default.target"],
                     allow_failure=True)
        if ready.returncode == 0:
            break
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise RuntimeError(
                f"default.target stayed inactive for {timeout} seconds. {container_logs(container_id)}"
            )
        sleep_for = min(delay, remaining)
        operation(
            state_path,
            state,
            phase="systemd-readiness",
            lane=lane.name,
            container_id=container_id,
            deadline_at=after(remaining),
            next_check_at=after(sleep_for),
            current_backoff_seconds=delay,
        )
        time.sleep(sleep_for)
        delay *= 2

    probe = "\n".join(
        [
            'pid1="$(cat /proc/1/comm)"',
            'kernel_arch="$(uname -m)"',
            'package_arch="$(dpkg --print-architecture)"',
            'system_state="$(systemctl is-system-running 2>/dev/null || true)"',
            'default_target="$(systemctl is-active default.target)"',
            'systemctl show-environment >/dev/null',
            'systemd_private_socket=missing',
            'system_bus_socket=missing',
            '[ ! -S /run/systemd/private ] || systemd_private_socket=present',
            '[ ! -S /run/dbus/system_bus_socket ] || system_bus_socket=present',
            '[ "$pid1" = systemd ]',
            '[ "$kernel_arch" = "$EXPECTED_KERNEL_ARCH" ]',
            '[ "$package_arch" = "$EXPECTED_PACKAGE_ARCH" ]',
            '[ "$system_state" = running ] || [ "$system_state" = degraded ]',
            '[ "$default_target" = active ]',
            'printf "pid1=%s\\nkernel_arch=%s\\npackage_arch=%s\\nsystem_state=%s\\ndefault_target=%s\\nsystemctl_show_environment=passed\\nsystemd_private_socket=%s\\nsystem_bus_socket=%s\\n" "$pid1" "$kernel_arch" "$package_arch" "$system_state" "$default_target" "$systemd_private_socket" "$system_bus_socket"',
        ]
    )
    operation(state_path, state, phase="systemd-status-probe", current_backoff_seconds=None, next_check_at=None)
    result = call(["docker", "exec", "--env", f"EXPECTED_KERNEL_ARCH={lane.kernel_arch}",
                   "--env", f"EXPECTED_PACKAGE_ARCH={lane.package_arch}", container_id,
                   "/bin/sh", "-ec", probe], allow_failure=True)
    probes = parse_fields(result.stdout)
    if result.returncode:
        detail = result.stderr.strip() or result.stdout.strip()
        raise RuntimeError(f"systemd status probe exited with {result.returncode}: {detail[-3000:]}")
    return probes


def jess_service(container_id: str, state_path: Path, state: dict[str, Any]) -> str:
    unit = f"neo-installer-check-{uuid.uuid4().hex[:10]}"
    operation(state_path, state, phase="jess-service-probe", service_unit=unit)
    result = call(["docker", "exec", container_id, "systemd-run", "--quiet", "--collect", "--wait",
                   "--pipe", "--unit", unit, "--property=User=jess", "/usr/bin/id", "-un"],
                  allow_failure=True)
    if result.returncode:
        detail = result.stderr.strip() or result.stdout.strip()
        raise RuntimeError(f"systemd-run exited with {result.returncode}: {detail[-3000:]}")
    if result.stdout.strip() != "jess":
        raise RuntimeError(f"systemd-run returned {result.stdout.strip()!r}, expected 'jess'")
    return "jess"


def run_lane(
    lane: ArchitectureLane,
    run_id: str,
    operation_path: Path,
    state: dict[str, Any],
) -> dict[str, Any]:
    image_tag = f"neo-installer-check-{lane.name}:{run_id}"
    container_name = f"neo-installer-check-{lane.name}-{run_id}"
    result: dict[str, Any] = {
        "lane": lane.name,
        "docker_platform": lane.platform,
        "image_tag": image_tag,
        "container_name": container_name,
        "status": "running",
        "container_removed": False,
    }
    container_id = None
    try:
        build_image(lane, image_tag, operation_path, state)
        result.update(image_identity(image_tag, lane))
        created = call(["docker", "create", "--name", container_name, "--platform", lane.platform,
                        "--privileged", "--cgroupns=private", "--network=none", "--tmpfs", "/run",
                        "--tmpfs", "/run/lock", "--tmpfs", "/tmp", "--env", "container=docker", image_tag])
        container_id = created.stdout.strip()
        result["container_id"] = container_id
        operation(
            operation_path,
            state,
            phase="container-start",
            lane=lane.name,
            image_tag=image_tag,
            container_name=container_name,
            container_id=container_id,
            deadline_at=None,
            next_check_at=None,
        )
        call(["docker", "start", container_id])
        result["probes"] = wait_for_systemd(
            lane, container_id, STARTUP_TIMEOUT_SECONDS, operation_path, state
        )
        result["probes"]["service_user"] = jess_service(container_id, operation_path, state)
        result["status"] = "passed"
    except KeyboardInterrupt:
        result["status"] = "interrupted"
        result["error"] = "Interrupted by the operator"
        raise
    except Exception as error:
        result["status"] = "failed"
        result["error"] = str(error)
        if container_id:
            result["container_logs_on_failure"] = container_logs(container_id)
    finally:
        if container_id:
            removed = call(["docker", "rm", "--force", container_id], allow_failure=True)
            if removed.returncode == 0:
                result["container_removed"] = True
            else:
                result["cleanup_error"] = (removed.stderr or removed.stdout).strip()[-2000:]
                result["status"] = "failed"
                result["error"] = "Could not remove the disposable container created by this run"
    if result["status"] == "passed":
        print(f"{lane.name}: systemd PID 1 and service user jess passed", flush=True)
    elif result["status"] != "interrupted":
        print(f"{lane.name}: failed: {result.get('error', 'unknown error')}", flush=True)
    return result


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--architecture",
        choices=sorted(LANES),
        action="append",
        help="Run one lane. Repeat this option to run both. The default runs both.",
    )
    args = parser.parse_args()
    if shutil.which("docker") is None:
        parser.error("Docker CLI is not available in PATH")

    run_id = uuid.uuid4().hex[:12]
    revision = output(["git", "-C", str(ROOT), "rev-parse", "HEAD"])
    files = (Path(__file__).resolve(), DOCKERFILE)
    hashes = {path.name: hashlib.sha256(path.read_bytes()).hexdigest() for path in files}
    result_path = EVIDENCE / f"result-{run_id}.json"
    operation_path = EVIDENCE / f"operation-{run_id}.json"
    state: dict[str, Any] = {
        "operation_id": run_id,
        "source_revision": revision,
        "runner_files_sha256": hashes,
        "host_process_id": os.getpid(),
        "started_at": now(),
        "status": "running",
    }
    report: dict[str, Any] = {
        "operation_id": run_id,
        "source_revision": revision,
        "runner_files_sha256": hashes,
        "started_at": now(),
        "base_image_tag": BASE_IMAGE,
        "claim": "Disposable Docker systemd capability only. No installer compatibility or support-floor claim.",
        "docker": None,
        "lanes": [],
    }
    save(result_path, report)
    operation(operation_path, state, phase="docker-preflight")
    try:
        info = output(["docker", "info", "--format", "{{.OSType}}|{{.Architecture}}|{{.ServerVersion}}"])
        server_os, server_arch, server_version = info.split("|", maxsplit=2)
        if server_os != "linux":
            raise RuntimeError(f"Docker must target Linux, but reports {server_os!r}")
        report["docker"] = {
            "context": output(["docker", "context", "show"]),
            "server_os": server_os,
            "server_architecture": server_arch,
            "server_version": server_version,
        }
        operation(operation_path, state, phase="docker-preflight-complete", docker=report["docker"])
    except Exception as error:
        report["status"] = "failed"
        report["error"] = str(error)
        report["finished_at"] = now()
        save(result_path, report)
        operation(operation_path, state, phase="docker-preflight-failed", status="failed", error=str(error))
        print(f"Docker preflight failed: {error}", file=sys.stderr)
        return 1

    try:
        for name in args.architecture or list(LANES):
            lane_result = run_lane(LANES[name], run_id, operation_path, state)
            report["lanes"].append(lane_result)
            save(result_path, report)
    except KeyboardInterrupt:
        report["status"] = "interrupted"
        report["finished_at"] = now()
        save(result_path, report)
        operation(operation_path, state, phase="interrupted", status="interrupted")
        return 130

    report["status"] = "passed" if all(lane["status"] == "passed" for lane in report["lanes"]) else "failed"
    report["finished_at"] = now()
    save(result_path, report)
    operation(
        operation_path,
        state,
        phase="complete",
        status=report["status"],
        finished_at=report["finished_at"],
        result_file=str(result_path),
    )
    print(f"Evidence: {result_path}", flush=True)
    print("This capability check establishes no installer compatibility or support floor.", flush=True)
    return 0 if report["status"] == "passed" else 1


if __name__ == "__main__":
    raise SystemExit(main())
