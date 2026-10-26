---
name: control-cli
description: Build or adapt a local harness to drive, inspect, and profile an interactive CLI or TUI without external services. Use for CLI UX checks, startup regressions, memory leaks, hangs, prompt flows, or terminal demos.
---

# Control CLI

Use a repeatable local harness to exercise an interactive CLI instead of poking at it manually. First reuse the repo's own test/demo harness if it exists; otherwise assemble a temporary harness from standard local tools.

For the Neo CLI, first inspect compatible local tests in `neo/tests/` and the binary they exercise. Reuse local or offline fixtures; inspect prerequisites before selecting a suite, since the end-to-end suite can require real network access. Prompt, keyboard, resize, and interrupt claims require a real PTY. Set or remove `CI` and `--ci` deliberately: noninteractive output tests do not establish interactive behavior.

Identify the measured process before profiling: the Rust `neo` driver, a spawned Haskell compiler, or the running Haskell program. Use profiling facilities appropriate to that runtime and enabled in the tested build; JavaScript inspectors apply only to JavaScript processes.

## What It Is Used For

- Reproducing CLI/TUI bugs with deterministic input.
- Verifying keyboard flows, prompts, interrupts, resize behavior, and terminal layout.
- Capturing before/after transcripts for bug fixes.
- Profiling startup time, slow operations, hangs, or memory growth.
- Recording a short terminal demo when output is easier to show than explain.

## Harness Loop

1. Identify the command under test and the smallest reproducible workspace.
2. Discover existing local harnesses: package scripts, e2e tests, demo recorders, expect scripts, or PTY helpers.
3. If no harness exists, launch the CLI in an isolated terminal session with deterministic env vars.
4. Capture the current screen before interacting.
5. Send one action at a time: text, Enter, arrows, Escape, Ctrl-C, resize.
6. Wait for a concrete screen pattern or prompt before the next action.
7. Save the transcript and any profile artifacts.
8. Kill the session cleanly.

## Harness Options

- Repo-native harness: prefer checked-in scripts because they know the app's startup, env, and prompts.
- `tmux`: managed sessions, `capture-pane`, `send-keys`, attach/detach.
- PTY probe: use a short Python, Node, or Expect script when tmux is unavailable.
- Runtime inspector: for Node or Bun processes, use their inspector for CPU profiles, heap snapshots, and live evaluation. For native processes, use available native CPU, allocation, or stack profiling facilities.
- Terminal recorder: use repo-local demo tools or asciinema-compatible tools when the user asks for a demo.

## Minimal tmux Harness

```bash
SESSION="cli-harness-$(date +%s)"
tmux new-session -d -s "$SESSION" -- <command-under-test>
tmux capture-pane -pt "$SESSION"
tmux send-keys -t "$SESSION" "help" Enter
tmux capture-pane -pt "$SESSION"
tmux kill-session -t "$SESSION"
```

For Node CLIs:

```bash
NODE_OPTIONS="--inspect=127.0.0.1:0" tmux new-session -d -s "$SESSION" -- <node-cli-command>
```

Read the terminal output to find the inspector URL, then use Chrome DevTools-compatible tooling if profiling is needed.

## Minimal PTY Harness

Use a PTY script when you need deterministic waits in a repo that does not have tmux or a demo harness. Keep it temporary under `.agents-state/control-cli/` unless the user asks to add a reusable test. Capture its output there as evidence. Treat readiness timeout or early exit as a failed probe. Always close descriptors, stop the owned process group, and reap the direct child with bounded waits; use the command's own teardown for subprocesses that detach from that group.

```python
import errno
import os
import pty
import select
import signal
import subprocess
import time

master_fd, slave_fd = pty.openpty()
proc = None
buffer = bytearray()
try:
    proc = subprocess.Popen(
        ["<command>", "<arg>"],
        stdin=slave_fd,
        stdout=slave_fd,
        stderr=slave_fd,
        close_fds=True,
        start_new_session=True,
    )
    os.close(slave_fd)
    slave_fd = None
    deadline = time.monotonic() + 30
    while True:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise TimeoutError("CLI did not produce the expected ready text")
        ready, _, _ = select.select([master_fd], [], [], min(0.25, remaining))
        if not ready:
            continue
        try:
            chunk = os.read(master_fd, 4096)
        except OSError as error:
            if error.errno != errno.EIO:
                raise
            chunk = b""
        if not chunk:
            raise RuntimeError("CLI output closed before the expected ready text")
        buffer.extend(chunk)
        if b"<ready text>" in buffer:
            os.write(master_fd, b"help\n")
            break
finally:
    try:
        if slave_fd is not None:
            os.close(slave_fd)
        if proc is not None:
            try:
                os.killpg(proc.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            try:
                proc.wait(timeout=2)
            except subprocess.TimeoutExpired:
                pass
            finally:
                # Also stop children that outlive the direct process.
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                proc.wait(timeout=2)
    finally:
        os.close(master_fd)
        print(buffer.decode(errors="replace"))
```

If the CLI needs richer terminal control, use `pty.fork()` or an existing PTY library.

## Profiling Recipes

- Startup regression: capture baseline and treatment startup timings under the same machine, env, and command.
- Slow operation: start a CPU profile, perform the operation, stop the profile, and compare top self-time functions.
- Memory leak: capture runtime-appropriate allocation, residency, or heap evidence before and after repeated operations. Force GC only when supported and relevant to the tested runtime.
- Hang: capture the screen, active handles/resources, and a stack/CPU sample before interrupting.

## Guardrails

- Prefer deterministic waits over sleeps. If you must sleep, explain why.
- Do not send credentials or destructive commands into a controlled session.
- Keep temporary probes, transcripts, profiles, and recordings under a task-specific directory in repository-root `.agents-state/control-cli/`. Create that directory before capture and configure output paths accordingly; reuse existing harnesses in place.
- Do not hard-code paths from another repository. Adapt commands to the current repo's scripts and runtime.
- Clean up owned tmux sessions, processes, inspectors, and disposable runtime state, including after failures. Preserve captured transcripts, profiles, and recordings as evidence under `.agents-state/control-cli/`.
