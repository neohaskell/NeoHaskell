import base64
import json
import os
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path


TOOL = Path(__file__).resolve().parents[1] / "tool_pair.py"
EXPECTED_VERSION = "0.17.1"
HELP_OUTPUT = "Usage: neoctl [--help|-h|--version]\nRun the NeoHaskell control service at http://127.0.0.1:8080.\n"
INVALID_OUTPUT = "Invalid arguments. Run neoctl --help for usage.\n"


class ToolPairTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="neoctl-tool-pair-test-")
        self.root = Path(self.temporary.name)
        self.neo = self.write_executable(
            "neo",
            f'''import os
import sys

if os.environ.get("LOG_LEVEL") is not None:
    sys.stdout.write('{{"level":"Info"}}\\n')
sys.stdout.write("neo {EXPECTED_VERSION}\\n")
''',
        )
        self.neoctl = self.write_neoctl()

    def tearDown(self):
        self.temporary.cleanup()

    def write_executable(self, name, source):
        executable = self.root / name
        executable.write_text("#!/usr/bin/env python3\n" + source, encoding="utf-8")
        executable.chmod(executable.stat().st_mode | 0o111)
        return executable

    def write_neoctl(self, version=EXPECTED_VERSION, mode="clean"):
        return self.write_executable(
            "neoctl",
            f'''import os
import subprocess
import sys
import time
from pathlib import Path

arguments = sys.argv[1:]
if os.environ.get("LOG_LEVEL") is not None:
    sys.stdout.write('{{"level":"Info"}}\\n')
if {mode!r} == "large-output" and arguments == ["--help"]:
    sys.stdout.write("x" * 1_000_000)
    sys.stdout.flush()
    time.sleep(2)
elif {mode!r} == "resistant-descendant" and arguments == ["--help"]:
    descendant_code = (
        "import os, signal, sys, time; "
        "signal.signal(signal.SIGTERM, signal.SIG_IGN); "
        "signal.signal(signal.SIGUSR1, lambda *_: open(sys.argv[2], 'w').write('alive')); "
        "open(sys.argv[1], 'w').write('ready'); time.sleep(30)"
    )
    descendant = subprocess.Popen([
        sys.executable,
        "-c",
        descendant_code,
        os.environ["DESCENDANT_READY_FILE"],
        os.environ["DESCENDANT_ALIVE_FILE"],
    ])
    Path(os.environ["DESCENDANT_PID_FILE"]).write_text(
        str(descendant.pid), encoding="utf-8"
    )
    ready_file = Path(os.environ["DESCENDANT_READY_FILE"])
    deadline = time.monotonic() + 5
    while not ready_file.exists() and time.monotonic() < deadline:
        time.sleep(0.01)
    if not ready_file.exists():
        sys.exit(17)
    time.sleep(30)
elif arguments == ["--version"]:
    sys.stdout.write("neoctl {version}\\n")
elif arguments in (["--help"], ["-h"]):
    if {mode!r} == "timeout" and arguments == ["--help"]:
        time.sleep(2)
    sys.stdout.write({HELP_OUTPUT!r})
elif arguments in (["--bogus"], ["doctor"], ["--help", "unexpected"]):
    if {mode!r} == "side-effect" and arguments == ["doctor"]:
        Path("created.txt").write_text("changed", encoding="utf-8")
    if {mode!r} in ("echo", "side-effect"):
        sys.stderr.write(" ".join(arguments) + "\\n")
    sys.stderr.write({INVALID_OUTPUT!r})
    sys.exit(2)
else:
    sys.stderr.write({INVALID_OUTPUT!r})
    sys.exit(2)
''',
        )

    def run_tool(self, timeout=None, extra_environment=None):
        command = [
            sys.executable,
            "-B",
            str(TOOL),
            "--neo",
            str(self.neo),
            "--neoctl",
            str(self.neoctl),
            "--expected-version",
            EXPECTED_VERSION,
        ]
        if timeout is not None:
            command.extend(("--timeout", str(timeout)))
        environment = os.environ.copy()
        environment.update(extra_environment or {})
        return subprocess.run(
            command,
            cwd=self.root,
            env=environment,
            capture_output=True,
            text=True,
            timeout=15,
            check=False,
        )

    def process_survived(self, process_id, alive_file):
        try:
            os.kill(process_id, signal.SIGUSR1)
        except ProcessLookupError:
            return False
        deadline = time.monotonic() + 0.25
        while time.monotonic() < deadline:
            if alive_file.exists():
                return True
            time.sleep(0.01)
        return False

    def stop_test_process(self, process_id):
        try:
            os.kill(process_id, signal.SIGKILL)
        except ProcessLookupError:
            return
        deadline = time.monotonic() + 2
        while time.monotonic() < deadline:
            try:
                os.kill(process_id, 0)
            except ProcessLookupError:
                return
            time.sleep(0.05)

    @unittest.skipIf(os.name == "nt", "fixture commands use POSIX executable scripts")
    def test_matching_pair_passes_the_full_process_matrix(self):
        result = self.run_tool(extra_environment={"LOG_LEVEL": "Info", "CI": "1"})

        self.assertEqual(result.returncode, 0, result.stderr)
        report = json.loads(result.stdout)
        self.assertEqual(report["status"], "PASS")
        self.assertEqual(report["expected_version"], EXPECTED_VERSION)
        self.assertEqual(len(report["observations"]), 7)
        self.assertTrue(all(item["passed"] for item in report["observations"]))
        self.assertTrue(all(item["file_changes"] == [] for item in report["observations"]))
        self.assertTrue(
            all(item["stdout"]["byte_count"] < 65_536 for item in report["observations"])
        )

    @unittest.skipIf(os.name == "nt", "fixture commands use POSIX executable scripts")
    def test_version_mismatch_fails(self):
        self.neoctl = self.write_neoctl(version="0.16.9")

        result = self.run_tool()

        self.assertEqual(result.returncode, 1)
        report = json.loads(result.stdout)
        self.assertIn("neoctl-version stdout did not match the expected bytes", report["failures"])

    @unittest.skipIf(os.name == "nt", "fixture commands use POSIX executable scripts")
    def test_file_changes_and_raw_argument_echo_fail(self):
        self.neoctl = self.write_neoctl(mode="side-effect")

        result = self.run_tool()

        self.assertEqual(result.returncode, 1)
        report = json.loads(result.stdout)
        self.assertIn("neoctl-doctor changed files in its working directory", report["failures"])
        self.assertIn("neoctl-doctor echoed a raw argument", report["failures"])
        doctor = next(
            item for item in report["observations"] if item["name"] == "neoctl-doctor"
        )
        self.assertEqual(doctor["file_changes"][0]["path"], "created.txt")

    @unittest.skipIf(os.name == "nt", "fixture commands use POSIX executable scripts")
    def test_timed_out_process_fails_and_is_stopped(self):
        self.neoctl = self.write_neoctl(mode="timeout")

        result = self.run_tool(timeout=0.1)

        self.assertEqual(result.returncode, 1)
        report = json.loads(result.stdout)
        help_case = next(
            item
            for item in report["observations"]
            if item["name"] == "neoctl-help"
        )
        self.assertTrue(help_case["timed_out"])

    @unittest.skipIf(os.name == "nt", "POSIX process group behavior is required")
    def test_timeout_kills_sigterm_resistant_descendant_after_leader_exits(self):
        self.neoctl = self.write_neoctl(mode="resistant-descendant")
        descendant_pid_file = self.root / "descendant.pid"
        descendant_ready_file = self.root / "descendant.ready"
        descendant_alive_file = self.root / "descendant.alive"

        try:
            result = self.run_tool(
                timeout=1,
                extra_environment={
                    "DESCENDANT_PID_FILE": str(descendant_pid_file),
                    "DESCENDANT_READY_FILE": str(descendant_ready_file),
                    "DESCENDANT_ALIVE_FILE": str(descendant_alive_file),
                },
            )

            descendant_pid = int(descendant_pid_file.read_text(encoding="utf-8"))
            report = json.loads(result.stdout)
            help_case = next(
                item
                for item in report["observations"]
                if item["name"] == "neoctl-help"
            )
            self.assertTrue(help_case["timed_out"])
            self.assertEqual(help_case["exit_code"], -signal.SIGTERM)
            self.assertFalse(
                self.process_survived(descendant_pid, descendant_alive_file),
                "the SIGTERM-resistant descendant survived verifier cleanup",
            )
        finally:
            if descendant_pid_file.exists():
                descendant_pid = int(descendant_pid_file.read_text(encoding="utf-8"))
                self.stop_test_process(descendant_pid)

    @unittest.skipIf(os.name == "nt", "fixture commands use POSIX executable scripts")
    def test_oversized_output_is_captured_with_a_fixed_limit(self):
        self.neoctl = self.write_neoctl(mode="large-output")

        result = self.run_tool()

        self.assertEqual(result.returncode, 1)
        report = json.loads(result.stdout)
        self.assertIn(
            "neoctl-help exceeded the raw stream capture limit",
            report["failures"],
        )
        help_case = next(
            item
            for item in report["observations"]
            if item["name"] == "neoctl-help"
        )
        self.assertGreater(help_case["stdout"]["byte_count"], 65_536)
        self.assertTrue(help_case["stdout"]["truncated"])
        self.assertEqual(len(base64.b64decode(help_case["stdout"]["base64"])), 65_536)


if __name__ == "__main__":
    unittest.main()
