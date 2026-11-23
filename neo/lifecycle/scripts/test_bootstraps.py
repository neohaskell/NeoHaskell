import hashlib
import json
import os
import errno
import pty
import select
import signal
import time
from pathlib import Path
import subprocess
import tempfile
import unittest

import generate_bootstraps as generator


ROOT = Path(__file__).resolve().parents[3]
MESSAGES = json.loads((ROOT / "neo/lifecycle/src/english.json").read_text())
VERSION = "9.8.7"
LOCATOR = VERSION + "@" + "a" * 64


class PosixBootstrap(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="neo-bootstrap-test-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name).resolve()
        self.home = self.root / "home"
        self.state = self.root / "state"
        self.stage = self.root / "temporary"
        self.commands = self.root / "commands"
        for path in (self.home, self.state, self.stage, self.commands):
            path.mkdir(mode=0o700)
        self.payload = self.root / "payload"
        self.arguments = self.root / "arguments"
        self.locator = self.root / "locator"
        self.locator.write_text(LOCATOR + "\n")
        self.env = {
            "HOME": str(self.home),
            "XDG_STATE_HOME": str(self.state),
            "TMPDIR": str(self.stage),
            "PATH": str(self.commands) + os.pathsep + "/usr/bin:/bin:/usr/sbin:/sbin",
            "FIXTURE_PAYLOAD": str(self.payload),
            "FIXTURE_LOCATOR": str(self.locator),
            "FIXTURE_ARGUMENTS": str(self.arguments),
        }
        self.command("curl", """#!/bin/sh
output=
while [ "$#" -gt 0 ]; do
    if [ "$1" = --output ]; then output=$2; shift 2
    else url=$1; shift; fi
done
if [ "${FIXTURE_DOWNLOAD_FAILURE-}" = yes ]; then
    printf 404
    exit 22
fi
case "$url" in
    */candidate.locator) source=$FIXTURE_LOCATOR ;;
    *) source=$FIXTURE_PAYLOAD ;;
esac
cp "$source" "$output" || exit 23
printf 200
""")

    def command(self, name, body):
        path = self.commands / name
        path.write_text(body)
        path.chmod(0o700)

    def installer(self, status=1, message="The existing command blocks installation."):
        body = "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$FIXTURE_ARGUMENTS\"\n"
        body += "if read -r input; then exit 99; fi\n"
        if message:
            body += "printf '%s\\n' " + generator.shlex.quote(message) + " >&2\n"
        body += f"exit {status}\n"
        return self.script(body)

    def script(self, body):
        self.payload.write_text(body)
        artifact = generator.Artifact.read(self.payload)
        artifacts = {
            target: generator.Artifact("neo-installer-" + target, artifact.size, artifact.sha256)
            for target in generator.TARGETS.values()
        }
        return generator.generate_posix(VERSION, artifacts, MESSAGES)

    def run_bootstrap(self, payload, arguments=(), expect_cleanup=True, **environment):
        result = subprocess.run(
            ["/bin/sh", "-s", "--", *arguments],
            input=payload, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            env={**self.env, **environment}, timeout=30,
        )
        if expect_cleanup:
            self.assertEqual(list(self.stage.iterdir()), [], result.stderr.decode())
        return result

    def report(self):
        records = list((self.state / "neohaskell-diagnostics").glob("*.json"))
        self.assertEqual(len(records), 1)
        self.assertEqual(records[0].stat().st_mode & 0o777, 0o600)
        return json.loads(records[0].read_bytes())["report"]

    def test_native_failure_preserves_result_and_distinguishes_download_report(self):
        preserved = self.home / "existing-neo"
        preserved.write_bytes(b"standalone command")
        script = self.installer(message="Existing command blocks installation.\nDiagnostic report saved at native.json")
        result = self.run_bootstrap(script)
        self.assertEqual(result.returncode, 1)
        self.assertTrue(self.arguments.exists())
        self.assertEqual(preserved.read_bytes(), b"standalone command")
        report = self.report()
        self.assertEqual(report["error"], "native_failed")
        self.assertEqual(report["recovery"], "inspect_installer_result")
        self.assertEqual(result.stderr.count(b"Diagnostic report saved at"), 1)
        self.assertEqual(result.stderr.count(MESSAGES["bootstrap_report_saved"].encode()), 1)
        self.assertNotEqual(MESSAGES["bootstrap_report_saved"], "Diagnostic report saved at ")

    def test_silent_native_failure_retains_download_evidence(self):
        result = self.run_bootstrap(self.installer(message=""))
        self.assertEqual(result.returncode, 1)
        self.assertEqual(self.report()["error"], "native_failed")
        self.assertIn(MESSAGES["bootstrap_report_saved"].encode(), result.stderr)

    def test_success_forwards_consent_and_requested_version_through_piped_shell(self):
        result = self.run_bootstrap(self.installer(status=0, message=""), ("--yes", "--plain", "--version", VERSION))
        self.assertEqual(result.returncode, 0, result.stderr.decode())
        self.assertEqual(self.arguments.read_text().splitlines(), [
            "install", "--candidate-locator", LOCATOR, "--yes", "--plain", "--version", VERSION,
        ])
        report = self.report()
        self.assertEqual(report["recovery"], "none")
        self.assertEqual(report["error"], "none")
        self.assertNotIn(MESSAGES["bootstrap_report_saved"].encode(), result.stderr)

    def test_cancelled_native_result_is_not_replaced_with_retry_advice(self):
        result = self.run_bootstrap(self.installer(status=130, message="Installation cancelled."))
        self.assertEqual(result.returncode, 130)
        self.assertEqual(self.report()["recovery"], "inspect_installer_result")
        self.assertIn(b"Installation cancelled.", result.stderr)

    def test_download_failure_never_executes_native_payload(self):
        result = self.run_bootstrap(self.installer(), FIXTURE_DOWNLOAD_FAILURE="yes")
        self.assertEqual(result.returncode, 1)
        self.assertFalse(self.arguments.exists())
        report = self.report()
        self.assertEqual(report["error"], "download_failed")
        self.assertEqual(report["attempts"]["locator"], 1)
        self.assertNotIn("native_start_attempted", report["completed_steps"])
        self.assertIn(MESSAGES["bootstrap_download"].encode(), result.stderr)

    def test_checksum_mismatch_never_executes_native_payload(self):
        script = self.installer()
        payload = self.payload.read_bytes()
        self.payload.write_bytes(payload.replace(b"blocks", b"stops!"))
        result = self.run_bootstrap(script)
        self.assertEqual(result.returncode, 1)
        self.assertFalse(self.arguments.exists())
        self.assertEqual(self.report()["error"], "integrity_failed")
        self.assertIn(MESSAGES["bootstrap_integrity"].encode(), result.stderr)

    def test_native_launch_failure_has_bootstrap_evidence(self):
        script = self.installer()
        self.payload.write_text("#!/nonexistent/neo-test-interpreter\n")
        artifact = generator.Artifact.read(self.payload)
        artifacts = {target: artifact for target in generator.TARGETS.values()}
        script = generator.generate_posix(VERSION, artifacts, MESSAGES)
        result = self.run_bootstrap(script)
        self.assertIn(result.returncode, (126, 127))
        self.assertFalse(self.arguments.exists())
        report = self.report()
        self.assertEqual(report["error"], "native_launch_failed")
        self.assertEqual(report["change"], "native_start_attempted")
        self.assertNotIn("native_started", report["completed_steps"])
        self.assertIn(MESSAGES["bootstrap_launch_failed"].encode(), result.stderr)

    def check_download_cleanup_failure(self, native_status):
        self.command("rm", "#!/bin/sh\nexit 1\n")
        result = self.run_bootstrap(self.installer(status=native_status), expect_cleanup=False)
        self.assertEqual(result.returncode, native_status)
        self.assertTrue(self.arguments.exists())
        self.assertEqual(len(list(self.stage.glob("*/neo-installer"))), 1)
        report = self.report()
        self.assertEqual(report["download_cleanup"], "failed")
        self.assertEqual(report["exit"], native_status)
        self.assertEqual(report["recovery"], "inspect_download_cleanup" if native_status == 0 else "inspect_installer_result")
        self.assertEqual(result.stderr.count(MESSAGES["bootstrap_cleanup_failed"].encode()), 1)
        self.assertIn(MESSAGES["bootstrap_report_saved"].encode(), result.stderr)

    def test_cleanup_failure_preserves_successful_installer_result(self):
        self.check_download_cleanup_failure(0)

    def test_cleanup_failure_preserves_failed_installer_result(self):
        self.check_download_cleanup_failure(1)

    def test_piped_bootstrap_reconnects_attended_installer_to_controlling_terminal(self):
        script = self.script("""#!/bin/sh
[ -t 0 ] && [ -t 1 ] || exit 98
printf 'Review installation?\\n'
read -r answer || exit 97
[ "$answer" = continue ] || exit 96
printf 'Consent received.\\n'
""")
        script_read, script_write = os.pipe()
        child, terminal = pty.fork()
        if child == 0:
            os.close(script_write)
            os.dup2(script_read, 0)
            os.close(script_read)
            os.execve("/bin/sh", ["/bin/sh", "-s"], self.env)
        os.close(script_read)
        transcript = bytearray()
        status = None
        try:
            with os.fdopen(script_write, "wb") as source:
                source.write(script)
            deadline = time.monotonic() + 30
            consent_sent = False
            while time.monotonic() < deadline:
                ready, _, _ = select.select([terminal], [], [], deadline - time.monotonic())
                if not ready:
                    break
                try:
                    chunk = os.read(terminal, 65536)
                except OSError as error:
                    if error.errno != errno.EIO:
                        raise
                    chunk = b""
                if not chunk:
                    break
                transcript.extend(chunk)
                if not consent_sent and b"Review installation?" in transcript:
                    os.write(terminal, b"continue\n")
                    consent_sent = True
            delay = 0.001
            while time.monotonic() < deadline:
                waited, child_status = os.waitpid(child, os.WNOHANG)
                if waited:
                    status = child_status
                    break
                time.sleep(min(delay, max(0, deadline - time.monotonic())))
                delay *= 2
            self.assertIsNotNone(status, transcript.decode())
            self.assertEqual(os.waitstatus_to_exitcode(status), 0, transcript.decode())
            self.assertIn(b"Consent received.", transcript)
            self.assertEqual(self.report()["exit"], 0)
            self.assertEqual(list(self.stage.iterdir()), [])
        finally:
            if status is None:
                try:
                    os.killpg(child, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                os.waitpid(child, 0)
            os.close(terminal)

    def test_unsupported_platform_stops_before_native_download(self):
        self.command("uname", "#!/bin/sh\nprintf 'unsupported-test-system\\n'\n")
        result = self.run_bootstrap(self.installer())
        self.assertEqual(result.returncode, 1)
        self.assertFalse(self.arguments.exists())
        self.assertIn(MESSAGES["bootstrap_platform"].encode(), result.stderr)

    def test_unwritable_temporary_location_stops_before_native_download(self):
        missing = self.root / "missing" / "temporary"
        result = self.run_bootstrap(self.installer(), TMPDIR=str(missing))
        self.assertEqual(result.returncode, 1)
        self.assertFalse(self.arguments.exists())
        self.assertFalse(missing.exists())
        self.assertEqual(self.report()["error"], "staging_failed")
        self.assertIn(MESSAGES["bootstrap_staging"].encode(), result.stderr)

    def test_invalid_release_information_stops_before_native_download(self):
        self.locator.write_text("not-a-release\n")
        result = self.run_bootstrap(self.installer())
        self.assertEqual(result.returncode, 1)
        self.assertFalse(self.arguments.exists())
        report = self.report()
        self.assertEqual(report["error"], "locator_invalid")
        self.assertEqual(report["attempts"]["installer"], 0)

    def test_unavailable_diagnostic_store_preserves_original_download_failure(self):
        result = self.run_bootstrap(
            self.installer(), XDG_STATE_HOME="relative-state-is-unsafe",
            FIXTURE_DOWNLOAD_FAILURE="yes",
        )
        self.assertEqual(result.returncode, 1)
        self.assertFalse(self.arguments.exists())
        self.assertFalse((self.state / "neohaskell-diagnostics").exists())
        self.assertIn(MESSAGES["bootstrap_download"].encode(), result.stderr)
        self.assertIn(MESSAGES["bootstrap_report_unavailable"].encode(), result.stderr)

    def test_generated_bootstrap_stays_within_windows_transport_bound(self):
        script = self.installer()
        powershell = generator.generate_powershell(VERSION, script, MESSAGES)
        self.assertIn(hashlib.sha256(script).hexdigest().encode(), powershell)
        self.assertIn(str(len(script)).encode(), powershell)


if __name__ == "__main__":
    unittest.main()
