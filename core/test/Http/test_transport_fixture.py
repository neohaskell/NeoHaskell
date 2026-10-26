import gzip
import http.client
import os
from pathlib import Path
import re
import signal
import socket
import ssl
import subprocess
import sys
import tempfile
import time
import unittest
from unittest.mock import patch

import transport_fixture as transport


class FixtureTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="nh-http-fixture-test-")
        self.directory = Path(self.temp.name)
        self.fixture = transport.Fixture(self.directory)

    def tearDown(self):
        self.fixture.close()
        self.temp.cleanup()

    def test_private_trust_and_proxy_environment(self):
        inherited = {name: "inherited" for name in (
            "http_proxy", "HTTP_PROXY", "https_proxy", "HTTPS_PROXY", "all_proxy",
            "ALL_PROXY", "no_proxy", "NO_PROXY", "SSL_CERT_FILE", "SSL_CERT_DIR",
            "NIX_SSL_CERT_FILE", "SYSTEM_CERTIFICATE_PATH")}
        with patch.dict(os.environ, inherited), patch.object(sys, "platform", "linux"):
            env = transport.trusted_env(self.directory / "a-cert.pem")
            self.assertEqual(env["SYSTEM_CERTIFICATE_PATH"], str(self.directory / "a-cert.pem"))
            self.assertEqual(env["no_proxy"], "")
            self.assertEqual(env["NO_PROXY"], "")
            for name in inherited.keys() - {"no_proxy", "NO_PROXY", "SYSTEM_CERTIFICATE_PATH"}:
                self.assertNotIn(name, env)
            self.assertEqual(os.environ["SYSTEM_CERTIFICATE_PATH"], "inherited")

    def test_finite_body_controls(self):
        port = self.fixture.start("origin")
        for method, path, expected in (
            ("GET", "/exact", b"1234"), ("GET", "/over", b"12345"),
            ("GET", "/empty", b""), ("HEAD", "/head", b""),
            ("GET", "/compressed", b"x" * 128),
        ):
            with self.subTest(path=path):
                connection = http.client.HTTPConnection("127.0.0.1", port, timeout=2)
                try:
                    connection.request(method, path)
                    response = connection.getresponse()
                    body = response.read()
                    self.assertEqual(response.status, 200)
                    self.assertEqual(response.getheader("X-Fixture"), "origin")
                    if path == "/head":
                        self.assertEqual(response.getheader("Content-Length"), "10000")
                    if path == "/compressed":
                        self.assertLess(len(body), 64)
                        self.assertEqual(response.getheader("Content-Encoding"), "gzip")
                        body = gzip.decompress(body)
                    self.assertEqual(body, expected)
                finally:
                    connection.close()

    def test_withheld_bodies_and_peer_closure_without_source_eof(self):
        port = self.fixture.start("origin")
        with subprocess.Popen([sys.executable, "-c", "import time; time.sleep(30)"]) as client:
            self.fixture.client = client
            try:
                for path, wire_body in (("/advertised", b""), ("/paced-close", b"12345"),
                                        ("/paced-chunked", b"4\r\n1234\r\n1\r\n5\r\n")):
                    with self.subTest(path=path):
                        with socket.create_connection(("127.0.0.1", port), timeout=2) as connection:
                            connection.sendall(f"GET {path} HTTP/1.1\r\nHost: localhost\r\n\r\n".encode())
                            received = b""
                            while b"\r\n\r\n" not in received:
                                chunk = connection.recv(4096)
                                self.assertTrue(chunk, "source ended before headers")
                                received += chunk
                            headers, body = received.split(b"\r\n\r\n", 1)
                            while len(body) < len(wire_body):
                                chunk = connection.recv(4096)
                                self.assertTrue(chunk, "source ended before five bytes")
                                body += chunk
                            self.assertEqual(body, wire_body)
                            if path == "/advertised":
                                self.assertIn(b"Content-Length: 10000", headers)
                            connection.settimeout(0.2)
                            with self.assertRaises(socket.timeout):
                                connection.recv(1)
                            connection.shutdown(socket.SHUT_RDWR)
                        deadline = time.monotonic() + 2
                        while not any(e["method"] == "CLOSED" and e["path"] == path
                                      for e in self.fixture.events) and time.monotonic() < deadline:
                            time.sleep(0.01)
                        closed = [e for e in self.fixture.events if e["method"] == "CLOSED" and e["path"] == path]
                        self.assertEqual(len(closed), 1)
                        self.assertTrue(closed[0]["client_alive"])
                        self.assertFalse(any(e["method"] == "SOURCE_EOF" for e in self.fixture.events))
            finally:
                client.terminate()
                client.wait(timeout=2)
                self.fixture.client = None

    def test_tls_fixture_trust_and_hostname_controls(self):
        server_a, cert_a = transport.certificate(self.directory)
        server_b, cert_b = transport.certificate(self.directory, "b")
        port_a = self.fixture.start("tls", server_a)
        port_b = self.fixture.start("tls-b", server_b)
        for port, cert, kind in ((port_a, cert_a, "tls"), (port_b, cert_b, "tls-b")):
            connection = http.client.HTTPSConnection("localhost", port, timeout=2,
                                                     context=ssl.create_default_context(cafile=str(cert)))
            try:
                connection.connect()
                self.assertEqual(connection.sock.version(), "TLSv1.2")
                connection.request("GET", "/exact")
                response = connection.getresponse()
                self.assertEqual((response.status, response.getheader("X-Fixture"), response.read()),
                                 (200, kind, b"1234"))
            finally:
                connection.close()
        for host, port in (("localhost", port_b), ("127.0.0.1", port_a)):
            connection = http.client.HTTPSConnection(host, port, timeout=2,
                                                     context=ssl.create_default_context(cafile=str(cert_a)))
            try:
                with self.assertRaises(ssl.SSLCertVerificationError):
                    connection.connect()
            finally:
                connection.close()

    def test_legacy_server_is_usable_only_with_legacy_client(self):
        server, cert = transport.certificate(self.directory, version=ssl.TLSVersion.TLSv1_1)
        port = self.fixture.start("legacy", server)
        transport.legacy_control(port, cert)
        modern = http.client.HTTPSConnection("localhost", port, timeout=2,
                                            context=ssl.create_default_context(cafile=str(cert)))
        try:
            with self.assertRaises(ssl.SSLError):
                modern.connect()
        finally:
            modern.close()


class WorkflowTests(unittest.TestCase):
    def test_each_mode_requires_its_own_execution_receipt(self):
        workflow = (transport.ROOT / ".github/workflows/test.yml").read_text()
        step = workflow.split("      - name: Run byte transport fixtures\n", 1)[1]
        step = step.split("\n      - uses:", 1)[0]
        script = "\n".join(line[10:] for line in step.split("        run: |\n", 1)[1].splitlines())
        script = script.replace("python3 -B -m unittest discover -s core/test/Http -p 'test_transport_fixture.py' -v", "true")
        script = script.replace("nix develop --command core/test/Http/transport_fixture.py", "probe")
        probe = """
probe() {
  local mode="$1" control=success
  if [ "$mode" = public-body ]; then control="$CONTROL"; fi
  if [ "$control" != missing-marker ]; then echo 'fixture: Http.Client.syntheticTransport'; fi
  case "$control" in
    missing-receipt) return 0 ;;
    wrong-receipt) echo 'PASS script:core/test/Http/transport_fixture.py#tls'; return 0 ;;
    duplicate-receipt) echo "PASS script:core/test/Http/transport_fixture.py#$mode" ;;
  esac
  echo "PASS script:core/test/Http/transport_fixture.py#$mode"
  if [ "$control" = unavailable ]; then return 77; fi
}
"""
        for control in ("success", "missing-marker", "missing-receipt", "wrong-receipt", "duplicate-receipt", "unavailable"):
            with self.subTest(control=control), tempfile.TemporaryDirectory() as directory:
                result = subprocess.run(["bash", "-c", probe + script], cwd=directory,
                                        env=dict(os.environ, CONTROL=control), capture_output=True, text=True)
                self.assertEqual(result.returncode == 0, control == "success", result.stdout + result.stderr)

    def test_aggregate_requires_transport_success(self):
        workflow = (transport.ROOT / ".github/workflows/test.yml").read_text()
        gate = workflow.split("  ci-gate:\n", 1)[1]
        self.assertRegex(gate, r"needs: \[[^\n]*test-transport[^\n]*\]")
        script = gate.split("        run: |\n", 1)[1]
        script = "\n".join(line[10:] for line in script.splitlines())
        env = dict(os.environ, RUN_EXPENSIVE="true", COMPILED="true", CHANGES_RESULT="success")
        for status in ("success", "failure", "cancelled", "skipped", ""):
            with self.subTest(status=status):
                rendered = re.sub(r"\$\{\{ needs\.([\w-]+)\.result \}\}",
                                  lambda match: status if match[1] == "test-transport" else "success", script)
                result = subprocess.run(["bash", "-c", rendered], env=env, capture_output=True, text=True)
                self.assertEqual(result.returncode == 0, status == "success", result.stdout + result.stderr)


class CancellationTests(unittest.TestCase):
    def test_cancellation_reaps_child_and_retains_output(self):
        for cancellation in (signal.SIGINT, signal.SIGTERM):
            with self.subTest(signal=cancellation), tempfile.TemporaryDirectory() as temp:
                directory = Path(temp)
                wrapper = directory / ".agents/skills/neohaskell-best-practices/scripts/with-toolchain"
                wrapper.parent.mkdir(parents=True)
                wrapper.write_text(f"#!{sys.executable}\nimport os, pathlib, sys, time\n"
                                   "print('child stdout', flush=True)\nprint('child stderr', file=sys.stderr, flush=True)\n"
                                   "pathlib.Path('child.pid').write_text(str(os.getpid()))\ntime.sleep(60)\n")
                wrapper.chmod(0o755)
                runner = "import sys; from pathlib import Path; import transport_fixture as t; t.ROOT=Path(sys.argv[1]); sys.argv=['fixture','body','--evidence-dir',sys.argv[1]]; t.main()"
                process = subprocess.Popen([sys.executable, "-c", runner, temp],
                                           cwd=Path(transport.__file__).parent,
                                           stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
                child = None
                try:
                    deadline = time.monotonic() + 10
                    while not (directory / "child.pid").exists() and time.monotonic() < deadline:
                        time.sleep(0.02)
                    self.assertTrue((directory / "child.pid").exists(), "child never started")
                    child = int((directory / "child.pid").read_text())
                    process.send_signal(cancellation)
                    process.communicate(timeout=10)
                    self.assertNotEqual(process.returncode, 0)
                    with self.assertRaises(ProcessLookupError):
                        os.kill(child, 0)
                    self.assertTrue(any("child stdout" in path.read_text() for path in directory.glob("*.stdout")))
                    self.assertTrue(any("child stderr" in path.read_text() for path in directory.glob("*.stderr")))
                finally:
                    if child is not None:
                        try:
                            os.killpg(child, signal.SIGKILL)
                        except ProcessLookupError:
                            pass
                    if process.poll() is None:
                        process.kill()
                    process.communicate(timeout=10)


if __name__ == "__main__":
    unittest.main()
