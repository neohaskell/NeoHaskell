from __future__ import annotations

import math
import os
import selectors
import signal
import subprocess
import sys
import tempfile
import threading
import time
from dataclasses import dataclass
from enum import Enum
from pathlib import Path


DEFAULT_MAX_OUTPUT_BYTES = 8 * 1024 * 1024
DEFAULT_TERMINATE_GRACE_SECONDS = 0.5
_PARENT_CLEANUP_GRACE_ENV = "NEOHASKELL_BOUNDED_PROCESS_PARENT_GRACE"
_CANCELLATION_SIGNALS = tuple(
    value
    for value in (getattr(signal, "SIGTERM", None), getattr(signal, "SIGINT", None), getattr(signal, "SIGHUP", None))
    if value is not None
)


class ProcessOutcome(str, Enum):
    EXITED = "exited"
    TIMED_OUT = "timed-out"
    OUTPUT_LIMITED = "output-limited"
    CANCELLED = "cancelled"


@dataclass(frozen=True)
class ProcessResult:
    outcome: ProcessOutcome
    returncode: int | None
    output: bytes
    elapsed_seconds: float

    @property
    def timed_out(self) -> bool:
        return self.outcome is ProcessOutcome.TIMED_OUT

    @property
    def output_limited(self) -> bool:
        return self.outcome is ProcessOutcome.OUTPUT_LIMITED


class ProcessCancelled(RuntimeError):
    outcome = ProcessOutcome.CANCELLED

    def __init__(self, signum: int):
        self.signum = signum
        super().__init__(f"bounded process cancelled by signal {signum}")


class _Cancellation(BaseException):
    def __init__(self, signum: int):
        self.signum = signum


def _validate(command: list[str], timeout: float, max_output_bytes: int, terminate_grace: float) -> None:
    if not command or any(not isinstance(value, str) or "\0" in value for value in command):
        raise ValueError("command must be a nonempty argument array without NUL bytes")
    if isinstance(timeout, bool) or not isinstance(timeout, (int, float)) or not math.isfinite(timeout) or timeout <= 0:
        raise ValueError("timeout must be finite and positive")
    if isinstance(max_output_bytes, bool) or not isinstance(max_output_bytes, int) or max_output_bytes <= 0:
        raise ValueError("maximum output bytes must be a positive integer")
    if isinstance(terminate_grace, bool) or not isinstance(terminate_grace, (int, float)) or not math.isfinite(terminate_grace) or terminate_grace < 0:
        raise ValueError("termination grace must be finite and nonnegative")


def _owned_cleanup_grace(terminate_grace: float) -> float:
    raw = os.environ.get(_PARENT_CLEANUP_GRACE_ENV)
    if raw is None:
        return terminate_grace
    try:
        parent_grace = float(raw)
    except ValueError:
        return terminate_grace
    if not math.isfinite(parent_grace) or parent_grace < 0:
        return terminate_grace
    return min(terminate_grace, parent_grace / 2)


def _signal_group(process: subprocess.Popen[bytes], signal_number: int) -> None:
    try:
        os.killpg(process.pid, signal_number)
    except ProcessLookupError:
        pass


def _group_exists(process: subprocess.Popen[bytes]) -> bool:
    try:
        os.killpg(process.pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def _terminate_group(process: subprocess.Popen[bytes], grace: float) -> None:
    _signal_group(process, signal.SIGTERM)
    deadline = time.monotonic() + grace
    while time.monotonic() < deadline and _group_exists(process):
        if process.poll() is None:
            try:
                process.wait(timeout=min(0.05, max(0.001, deadline - time.monotonic())))
            except subprocess.TimeoutExpired:
                pass
        else:
            time.sleep(min(0.01, max(0.001, deadline - time.monotonic())))
    _signal_group(process, signal.SIGKILL)
    if process.poll() is None:
        process.wait()
    deadline = time.monotonic() + max(0.1, grace)
    while time.monotonic() < deadline and _group_exists(process):
        time.sleep(0.01)


def _restore_handlers(handlers: dict[int, object]) -> None:
    for signum, handler in handlers.items():
        signal.signal(signum, handler)


def _propagate_cancellation(signum: int) -> None:
    signal.raise_signal(signum)
    raise ProcessCancelled(signum)


def run_bounded(
    command: list[str],
    *,
    cwd: Path,
    env: dict[str, str] | None = None,
    timeout: float,
    stdin: bytes = b"",
    max_output_bytes: int = DEFAULT_MAX_OUTPUT_BYTES,
    terminate_grace: float = DEFAULT_TERMINATE_GRACE_SECONDS,
) -> ProcessResult:
    _validate(command, timeout, max_output_bytes, terminate_grace)
    if threading.current_thread() is not threading.main_thread():
        raise RuntimeError("bounded process execution must run on the main thread for signal cleanup")

    cleanup_grace = _owned_cleanup_grace(terminate_grace)
    child_env = dict(os.environ if env is None else env)
    child_env[_PARENT_CLEANUP_GRACE_ENV] = str(cleanup_grace)
    cancellation = {"signum": None, "launching": False, "cleaning": False}
    handlers: dict[int, object] = {}
    process: subprocess.Popen[bytes] | None = None
    selector: selectors.BaseSelector | None = None
    stdout = None
    output = bytearray()
    outcome = ProcessOutcome.EXITED
    started = time.monotonic()

    def handle_cancellation(signum: int, frame: object) -> None:
        if cancellation["signum"] is None:
            cancellation["signum"] = signum
        if cancellation["launching"] or cancellation["cleaning"]:
            return
        raise _Cancellation(int(cancellation["signum"]))

    try:
        for signum in _CANCELLATION_SIGNALS:
            handlers[signum] = signal.getsignal(signum)
            signal.signal(signum, handle_cancellation)

        with tempfile.TemporaryFile() as child_input:
            child_input.write(stdin)
            child_input.seek(0)
            cancellation["launching"] = True
            try:
                process = subprocess.Popen(
                    command,
                    cwd=cwd,
                    env=child_env,
                    stdin=child_input,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.STDOUT,
                    start_new_session=True,
                )
            finally:
                cancellation["launching"] = False
            if cancellation["signum"] is not None:
                raise _Cancellation(int(cancellation["signum"]))

            stdout = process.stdout
            if stdout is None:
                raise RuntimeError("bounded process did not expose captured output")
            selector = selectors.DefaultSelector()
            selector.register(stdout, selectors.EVENT_READ)
            deadline = time.monotonic() + timeout
            stop_deadline: float | None = None
            final_deadline: float | None = None
            forced_kill = False

            while True:
                now = time.monotonic()
                if outcome is ProcessOutcome.EXITED and now >= deadline:
                    outcome = ProcessOutcome.TIMED_OUT
                    _signal_group(process, signal.SIGTERM)
                    stop_deadline = now + cleanup_grace
                if stop_deadline is not None and now >= stop_deadline and not forced_kill:
                    _signal_group(process, signal.SIGKILL)
                    forced_kill = True
                    final_deadline = now + 2
                if final_deadline is not None and now >= final_deadline and selector.get_map():
                    _signal_group(process, signal.SIGKILL)
                    break
                if process.poll() is not None and not selector.get_map():
                    break

                wake_at = deadline if outcome is ProcessOutcome.EXITED else None
                if stop_deadline is not None and not forced_kill:
                    wake_at = stop_deadline if wake_at is None else min(wake_at, stop_deadline)
                if final_deadline is not None:
                    wake_at = final_deadline if wake_at is None else min(wake_at, final_deadline)
                wait = 0.1 if wake_at is None else min(0.1, max(0.0, wake_at - now))
                if selector.get_map():
                    events = selector.select(wait)
                else:
                    time.sleep(wait)
                    events = []
                for key, _ in events:
                    try:
                        chunk = os.read(key.fd, 65536)
                    except OSError:
                        chunk = b""
                    if not chunk:
                        selector.unregister(key.fileobj)
                        continue
                    remaining = max_output_bytes - len(output)
                    if len(chunk) > remaining:
                        if remaining:
                            output.extend(chunk[:remaining])
                        if outcome is ProcessOutcome.EXITED:
                            outcome = ProcessOutcome.OUTPUT_LIMITED
                            _signal_group(process, signal.SIGTERM)
                            stop_deadline = time.monotonic() + cleanup_grace
                    elif remaining > 0:
                        output.extend(chunk)

            if process.poll() is None:
                _signal_group(process, signal.SIGKILL)
            returncode = process.wait()
            result = ProcessResult(outcome, returncode, bytes(output), time.monotonic() - started)
    finally:
        cancellation["cleaning"] = True
        if process is not None and (process.poll() is None or _group_exists(process)):
            _terminate_group(process, cleanup_grace)
        if selector is not None:
            selector.close()
        if stdout is not None:
            stdout.close()
        _restore_handlers(handlers)
        if cancellation["signum"] is not None:
            _propagate_cancellation(int(cancellation["signum"]))

    return result
