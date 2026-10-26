#!/usr/bin/env python3
"""Synthetic loopback transport regressions. Invoke from the repository root.

Uses the repository toolchain and process-private Linux trust bundles.
No external provider or globally installed fixture CA is used.
"""
import argparse
import contextlib
import gzip
import http.client
import http.server
import json
import os
from pathlib import Path
import signal
import ssl
import socket
import sys
import subprocess
import tempfile
import threading
import time

ROOT = Path(__file__).resolve().parents[3]


class Fixture:
    def __init__(self, directory):
        self.directory = directory
        self.events = []
        self.lock = threading.Lock()
        self.release = threading.Event()
        self.servers = []
        self.threads = []
        self.client = None

    def event(self, **entry):
        with self.lock:
            self.events.append(entry)

    def start(self, kind, context=None):
        owner = self

        class Handler(http.server.BaseHTTPRequestHandler):
            protocol_version = "HTTP/1.1"

            def log_message(self, *args):
                pass

            def do_CONNECT(self):
                owner.event(kind=kind, method="CONNECT", path=self.path)
                self.send_error(502)

            def do_HEAD(self):
                self.respond()

            def do_GET(self):
                self.respond()

            def do_PUT(self):
                self.respond()

            def respond(self):
                self.rfile.read(int(self.headers.get("Content-Length", "0")))
                owner.event(kind=kind, method=self.command, path=self.path,
                            authorization=self.headers.get("Authorization"))
                if self.path.startswith("/paced") or self.path == "/advertised":
                    self.send_response(200)
                    if self.path == "/advertised":
                        self.send_header("Content-Length", "10000")
                        chunks = []
                    elif self.path.endswith("chunked"):
                        self.send_header("Transfer-Encoding", "chunked")
                        chunks = [b"4\r\n1234\r\n", b"1\r\n5\r\n"]
                    else:
                        self.send_header("Connection", "close")
                        self.close_connection = True
                        chunks = [b"1234", b"5"]
                    self.end_headers()
                    for chunk in chunks:
                        self.wfile.write(chunk)
                        self.wfile.flush()
                        time.sleep(0.05)
                    owner.event(kind=kind, method="FLUSHED", path=self.path,
                                body_bytes=5 if chunks else 0)
                    started = time.monotonic()
                    self.connection.settimeout(0.1)
                    while not owner.release.is_set() and time.monotonic() - started < 15:
                        try:
                            data = self.connection.recv(1)
                        except socket.timeout:
                            continue
                        except ConnectionResetError:
                            data = b""
                        if not data:
                            owner.event(kind=kind, method="CLOSED", path=self.path,
                                        elapsed=time.monotonic() - started,
                                        client_alive=owner.client is not None and owner.client.poll() is None)
                            return
                    owner.event(kind=kind, method="SOURCE_EOF", path=self.path)
                    return
                body = b"proxy" if kind == "proxy" else b"1234"
                if self.path == "/empty":
                    body = b""
                if self.path == "/over":
                    body = b"12345"
                if self.path == "/compressed":
                    body = gzip.compress(b"x" * 128)
                self.send_response(200)
                length = 10000 if self.path == "/head" else len(body)
                if self.path == "/compressed":
                    self.send_header("Content-Encoding", "gzip")
                self.send_header("Content-Length", str(length))
                self.send_header("X-Fixture", kind)
                self.end_headers()
                if self.command != "HEAD":
                    self.wfile.write(body)

        server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        server.daemon_threads = True
        if context:
            server.socket = context.wrap_socket(server.socket, server_side=True)
        self.servers.append(server)
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        self.threads.append(thread)
        return server.server_port

    def close(self):
        self.release.set()
        for server in self.servers:
            server.shutdown()
            server.server_close()
        for thread in self.threads:
            thread.join()


def clean_env():
    env = os.environ.copy()
    for name in ("http_proxy", "HTTP_PROXY", "https_proxy", "HTTPS_PROXY",
                 "all_proxy", "ALL_PROXY", "no_proxy", "NO_PROXY"):
        env.pop(name, None)
    env["no_proxy"] = ""
    env["NO_PROXY"] = ""
    return env


def repl(fixture, directory, name, expressions, env):
    source = directory / (name + ".ghci")
    source.write_text(COMMON + expressions + "\n:quit\n")
    command = [str(ROOT / ".agents/skills/neohaskell-best-practices/scripts/with-toolchain"),
               "cabal", "repl", "lib:nhcore",
               "--project-file=cabal.project.dev", "--offline", "-j1", "-v0",
               "--repl-options=-package crypton-x509-store"]
    with source.open() as stream:
        with subprocess.Popen(command, cwd=ROOT, env=env, stdin=stream,
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                              text=True, start_new_session=True) as process:
            fixture.client = process
            stdout = stderr = ""
            try:
                stdout, stderr = process.communicate(timeout=600)
            except BaseException:
                with contextlib.suppress(ProcessLookupError):
                    os.killpg(process.pid, signal.SIGKILL)
                stdout, stderr = process.communicate()
                raise
            finally:
                fixture.client = None
                (directory / (name + ".stdout")).write_text(stdout)
                (directory / (name + ".stderr")).write_text(stderr)
    print(stdout, end="")
    print(stderr, end="")
    if process.returncode != 0 or "error:" in stderr or "*** Exception" in stdout + stderr:
        raise AssertionError("native REPL compilation/execution failed, not a transport regression")
    print("fixture: Http.Client.syntheticTransport")
    return stdout


COMMON = """:set -XOverloadedStrings -XTypeApplications -XOverloadedRecordDot
import Prelude qualified as P
import Control.Concurrent qualified as Conc
import Http.Client qualified as H
import Http.Client.Internal qualified as R
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS qualified as HT
import Network.HTTP.Types.Status qualified as Status
import Network.Connection qualified as C
import Network.TLS qualified as TLS
import Data.X509.CertificateStore qualified as CA
import Control.Exception qualified as E
import System.Timeout qualified as Timeout
import Task qualified
import Bytes qualified as B
import Text qualified as T
import Array qualified as A
import Result qualified as Result
let responseIs :: T.Text -> T.Text -> H.Response B.Bytes -> P.Bool; responseIs kind bytes response = response.statusCode P.== 200 P.&& P.elem ("x-fixture", kind) (A.toLinkedList (A.map (\\(name, value) -> (T.toLower name, value)) response.headers)) P.&& response.body P.== T.toBytes bytes
"""


def certificate(directory, name="a", version=ssl.TLSVersion.TLSv1_2):
    cert, key = directory / (name + "-cert.pem"), directory / (name + "-key.pem")
    command = ["openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes",
               "-keyout", str(key), "-out", str(cert), "-days", "2", "-subj",
               "/CN=localhost", "-addext", "subjectAltName=DNS:localhost"]
    subprocess.run(command, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.minimum_version = version
    context.maximum_version = version
    if version == ssl.TLSVersion.TLSv1_1:
        context.set_ciphers("DEFAULT:@SECLEVEL=0")
    context.load_cert_chain(cert, key)
    return context, cert


def tls_case(fixture, directory):
    context, cert = certificate(directory)
    port = fixture.start("tls", context)
    text = f"""
let params = (TLS.defaultParamsClient "" "") {{TLS.clientSupported=TLS.defaultSupported {{TLS.supportedVersions=H.secureTlsSupportedVersions}}}}
P.Just trust <- CA.readCertificateStore {json.dumps(str(cert))}
let trustedParams = params {{TLS.clientShared=(TLS.clientShared params) {{TLS.sharedCAStore=trust}}}}
trusted <- HC.newManager (HC.managerSetProxy HC.noProxy (HT.mkManagerSettings (C.TLSSettings trustedParams) P.Nothing))
untrusted <- HC.newManager (HC.managerSetProxy HC.noProxy (HT.mkManagerSettings (C.TLSSettings params) P.Nothing))
rightName <- HC.parseRequest "https://localhost:{port}/tls"
wrongName <- HC.parseRequest "https://127.0.0.1:{port}/tls"
positive <- E.try @HC.HttpException (HC.httpLbs rightName trusted)
negativeCa <- E.try @HC.HttpException (HC.httpLbs rightName untrusted)
negativeHost <- E.try @HC.HttpException (HC.httpLbs wrongName trusted)
P.print ("TLS_TRUSTED" :: P.String, P.either (P.const P.False) (\\response -> HC.responseStatus response P.== Status.status200 P.&& P.lookup "X-Fixture" (HC.responseHeaders response) P.== P.Just "tls" P.&& HC.responseBody response P.== "1234") positive)
P.print ("TLS_UNTRUSTED" :: P.String, P.either (P.const P.True) (P.const P.False) negativeCa)
P.print ("TLS_WRONG_HOST" :: P.String, P.either (P.const P.True) (P.const P.False) negativeHost)
public <- Task.runResult (H.sendSecure H.Get (H.withUrl "https://localhost:{port}/public-untrusted" H.request) B.empty)
P.print ("PUBLIC_UNTRUSTED" :: P.String, Result.isErr public)
"""
    output = repl(fixture, directory, "tls", text, clean_env())
    for marker in ("TLS_TRUSTED", "TLS_UNTRUSTED", "TLS_WRONG_HOST", "PUBLIC_UNTRUSTED"):
        assert f'("{marker}",True)' in output, marker
    print("PASS: injected-manager trust and hostname controls; public untrusted rejection")
    print("NOT PROVED: trusted public cached-manager success or system-store integration")


def proxy_case(fixture, directory):
    origin = fixture.start("origin")
    context, cert = certificate(directory)
    secure = fixture.start("tls", context)
    proxy = fixture.start("proxy")
    for case in ("lower", "upper"):
        env = trusted_env(cert)
        for scheme in ("http", "https"):
            name = scheme + "_proxy"
            env[name if case == "lower" else name.upper()] = f"http://127.0.0.1:{proxy}"
        text = f"""
controlManager <- HC.newManager HT.tlsManagerSettings
control <- HC.parseRequest "http://127.0.0.1:{origin}/control-{case}"
controlHttp <- E.try @HC.HttpException (HC.httpLbs control controlManager)
P.print ("PROXY_HTTP_CONTROL" :: P.String, P.either (P.const P.False) (\\response -> HC.responseStatus response P.== Status.status200 P.&& HC.responseBody response P.== "proxy" P.&& P.lookup "X-Fixture" (HC.responseHeaders response) P.== P.Just "proxy") controlHttp)
controlTls <- HC.parseRequest "https://localhost:{secure}/control-{case}"
controlConnect <- E.try @HC.HttpException (HC.httpLbs controlTls controlManager)
P.print ("PROXY_CONNECT_CONTROL" :: P.String, case controlConnect of P.Left (HC.HttpExceptionRequest _ (HC.ProxyConnectException _ _ status)) -> status P.== Status.status502; _ -> P.False)
raw <- Task.runResult (R.sendRaw H.Put (H.addHeader "Authorization" "synthetic-test" (H.withUrl "http://127.0.0.1:{origin}/actual-{case}" H.request)) (T.toBytes "synthetic"))
P.print ("RAW_DIRECT" :: P.String, case raw of Result.Ok response -> responseIs "origin" "1234" response; _ -> P.False)
secure <- Task.runResult (H.sendSecure H.Get (H.addHeader "Authorization" "synthetic-test" (H.withUrl "https://localhost:{secure}/actual-{case}" H.request)) B.empty)
P.print ("SECURE_DIRECT" :: P.String, case secure of Result.Ok response -> responseIs "tls" "1234" response; _ -> P.False)
"""
        before = len(fixture.events)
        output = repl(fixture, directory, "proxy-" + case, text, env)
        for marker in ("PROXY_HTTP_CONTROL", "PROXY_CONNECT_CONTROL", "RAW_DIRECT", "SECURE_DIRECT"):
            assert f'("{marker}",True)' in output, marker
        events = fixture.events[before:]
        traps = [(e["method"], e["path"]) for e in events if e["kind"] == "proxy"]
        assert traps == [("GET", f"http://127.0.0.1:{origin}/control-{case}"),
                         ("CONNECT", f"localhost:{secure}")], "REGRESSION_ASSERT:environment-proxy-traversal"
        for kind in ("origin", "tls"):
            reached = [e for e in events if e["kind"] == kind]
            assert len(reached) == 1 and reached[0]["path"] == "/actual-" + case, reached
            assert reached[0]["authorization"] == "synthetic-test", reached
    print("PASS: raw and trusted public secure origins reached without additional trap events")


def body_case(fixture, directory, mode="body"):
    env = clean_env()
    context = None
    scheme, host, kind = "http", "127.0.0.1", "origin"
    if mode == "public-body":
        context, cert = certificate(directory)
        env = trusted_env(cert)
        scheme, host, kind = "https", "localhost", "tls"
    port = fixture.start(kind, context)
    sender = "H.sendSecure" if mode == "public-body" else "R.sendRaw"
    text = f"""
let raw method path cap = Task.runResult ({sender} method (H.withTimeout 10 (H.withMaxResponseSize cap (H.withUrl (T.fromLinkedList ("{scheme}://{host}:{port}" P.++ path)) H.request))) B.empty)
exact <- raw H.Get "/exact" 4
empty <- raw H.Get "/empty" 1
headOnly <- raw H.Head "/head" 1
oversized <- raw H.Get "/over" 4
advertised <- Timeout.timeout 2000000 (raw H.Get "/advertised" 4)
decoded <- raw H.Get "/compressed" 64
P.print ("OVER" :: P.String, case oversized of Result.Err (H.ResponseTooLarge 4) -> P.True; _ -> P.False)
P.print ("ADVERTISED" :: P.String, case advertised of P.Just (Result.Err (H.ResponseTooLarge 4)) -> P.True; _ -> P.False)
P.print ("DECOMPRESSED" :: P.String, case decoded of Result.Err (H.ResponseTooLarge 64) -> P.True; _ -> P.False)
zeroBuilder <- E.try @E.ErrorCall (E.evaluate (H.withMaxResponseSize 0 H.request))
P.print ("ZERO_REJECTED" :: P.String, P.either (P.const P.True) (P.const P.False) zeroBuilder)
P.print ("EXACT" :: P.String, case exact of Result.Ok response -> responseIs "{kind}" "1234" response; Result.Err _ -> P.False)
P.print ("ZERO" :: P.String, case empty of Result.Ok response -> responseIs "{kind}" "" response; Result.Err _ -> P.False)
P.print ("HEAD" :: P.String, case headOnly of Result.Ok response -> responseIs "{kind}" "" response; Result.Err _ -> P.False)
P.putStrLn "BODY_CONTROLS_COMPLETE"
chunked <- Timeout.timeout 2000000 (raw H.Get "/paced-chunked" 4)
P.print ("CHUNKED" :: P.String, case chunked of P.Just (Result.Err (H.ResponseTooLarge 4)) -> P.True; _ -> P.False)
unknown <- Timeout.timeout 2000000 (raw H.Get "/paced-close" 4)
P.print ("UNKNOWN" :: P.String, case unknown of P.Just (Result.Err (H.ResponseTooLarge 4)) -> P.True; _ -> P.False)
Conc.threadDelay 3000000
"""
    if mode == "secure-internal-body":
        text = ":module + *Http.Client\n" + text.replace(
            "Task.runResult (R.sendRaw method", "sendSecureRawIO method").replace(
            ")) B.empty)", ")) B.empty")
    output = repl(fixture, directory, mode, text, env)
    for name in ("EXACT", "ZERO", "HEAD", "ZERO_REJECTED", "OVER", "ADVERTISED", "DECOMPRESSED", "CHUNKED", "UNKNOWN"):
        assert f'("{name}",True)' in output, "REGRESSION_ASSERT:body-limit:" + name
    for path in ("/advertised", "/paced-chunked", "/paced-close"):
        assert any(e["method"] == "FLUSHED" and e["path"] == path
                   and e["body_bytes"] == (0 if path == "/advertised" else 5)
                   for e in fixture.events), "source delivery did not match fixture"
        assert any(e["method"] == "CLOSED" and e["path"] == path and e["elapsed"] < 2.5 and e["client_alive"]
                   for e in fixture.events), "response did not close on bounded rejection"
        assert not any(e["method"] == "SOURCE_EOF" and e["path"] == path
                       for e in fixture.events), "source completed before client rejection"
    print("PASS: exact/empty/HEAD, zero-builder rejection, early cutoff and socket close")
    if mode == "secure-internal-body":
        print("DIAGNOSTIC ONLY: real secure reader over plaintext; public HTTPS trust not exercised")


def trusted_env(cert):
    if sys.platform != "linux":
        print("UNSUPPORTED: pinned native trust loader has no isolated environment bundle override")
        raise SystemExit(77)
    env = clean_env()
    for name in ("SSL_CERT_FILE", "SSL_CERT_DIR", "NIX_SSL_CERT_FILE"):
        env.pop(name, None)
    env["SYSTEM_CERTIFICATE_PATH"] = str(cert)
    return env


def legacy_control(port, cert):
    context = ssl.create_default_context(cafile=str(cert))
    context.minimum_version = ssl.TLSVersion.TLSv1_1
    context.maximum_version = ssl.TLSVersion.TLSv1_1
    context.set_ciphers("DEFAULT:@SECLEVEL=0")
    connection = http.client.HTTPSConnection("localhost", port, context=context, timeout=3)
    try:
        connection.connect()
        assert connection.sock.version() == "TLSv1.1", "legacy server did not negotiate TLS1.1"
        connection.request("GET", "/legacy-control")
        response = connection.getresponse()
        assert (response.status, response.getheader("X-Fixture"), response.read()) == (200, "legacy", b"1234")
    finally:
        connection.close()
    print("PASS: certificate-verified legacy client negotiated TLS1.1 and received exact origin response")


def public_trust_case(fixture, directory):
    context, cert = certificate(directory)
    env = trusted_env(cert)
    port = fixture.start("tls", context)
    context_b, cert_b = certificate(directory, "b")
    port_b = fixture.start("tls-b", context_b)
    control_b = f"""
publicB <- Task.runResult (H.sendSecure H.Get (H.withUrl "https://localhost:{port_b}/trusted-b" H.request) B.empty)
P.print ("PUBLIC_B_TRUSTED" :: P.String, case publicB of Result.Ok response -> responseIs "tls-b" "1234" response; _ -> P.False)
"""
    output_b = repl(fixture, directory, "public-trust-b", control_b, trusted_env(cert_b))
    assert '("PUBLIC_B_TRUSTED",True)' in output_b, "independent B trust positive control failed"
    legacy = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    legacy.minimum_version = ssl.TLSVersion.TLSv1_1
    legacy.maximum_version = ssl.TLSVersion.TLSv1_1
    legacy.set_ciphers("DEFAULT:@SECLEVEL=0")
    legacy.load_cert_chain(cert, directory / "a-key.pem")
    legacy_port = fixture.start("legacy", legacy)
    legacy_control(legacy_port, cert)
    text = f"""
controlManager <- HC.newManager (HC.managerSetProxy HC.noProxy HT.tlsManagerSettings)
controlRequest <- HC.parseRequest "https://localhost:{port}/library-control"
control <- E.try @HC.HttpException (HC.httpLbs controlRequest controlManager)
P.print ("SYSTEM_BUNDLE_CONTROL" :: P.String, P.either (P.const P.False) (\\response -> HC.responseStatus response P.== Status.status200 P.&& P.lookup "X-Fixture" (HC.responseHeaders response) P.== P.Just "tls" P.&& HC.responseBody response P.== "1234") control)
public <- Task.runResult (H.sendSecure H.Get (H.withUrl "https://localhost:{port}/public-trusted" H.request) B.empty)
P.print ("PUBLIC_TLS12_TRUSTED" :: P.String, case public of Result.Ok response -> responseIs "tls" "1234" response; Result.Err _ -> P.False)
untrusted <- Task.runResult (H.sendSecure H.Get (H.withUrl "https://localhost:{port_b}/untrusted-b" H.request) B.empty)
P.print ("PUBLIC_UNTRUSTED_B" :: P.String, case untrusted of Result.Err (H.Error _) -> P.True; _ -> P.False)
wrongHost <- Task.runResult (H.sendSecure H.Get (H.withUrl "https://127.0.0.1:{port}/wrong-host" H.request) B.empty)
P.print ("PUBLIC_WRONG_HOST" :: P.String, case wrongHost of Result.Err (H.Error _) -> P.True; _ -> P.False)
oldTls <- Task.runResult (H.sendSecure H.Get (H.withUrl "https://localhost:{legacy_port}/public-legacy" H.request) B.empty)
P.print ("PUBLIC_TLS11_REJECTED" :: P.String, case oldTls of Result.Err (H.Error _) -> P.True; _ -> P.False)
"""
    output = repl(fixture, directory, "public-trust-a", text, env)
    for marker in ("SYSTEM_BUNDLE_CONTROL", "PUBLIC_TLS12_TRUSTED", "PUBLIC_UNTRUSTED_B",
                   "PUBLIC_WRONG_HOST", "PUBLIC_TLS11_REJECTED"):
        assert f'("{marker}",True)' in output, "REGRESSION_ASSERT:" + marker
    rejected_paths = {"/untrusted-b", "/wrong-host", "/public-legacy"}
    assert not any(e["path"] in rejected_paths for e in fixture.events), "rejected peer received an HTTP request"
    print("PASS: public trust, hostname and TLS version boundaries with independent positive controls")


def main():
    def cancel(signum, _frame):
        raise SystemExit(128 + signum)

    signal.signal(signal.SIGTERM, cancel)
    parser = argparse.ArgumentParser()
    parser.add_argument("mode", choices=("tls", "proxy", "body", "secure-internal-body", "public-trust", "public-body"))
    parser.add_argument("--evidence-dir", type=Path)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="nh-http-fixture-") as temp:
        directory = args.evidence_dir.resolve() if args.evidence_dir else Path(temp)
        directory.mkdir(parents=True, exist_ok=True)
        fixture = Fixture(directory)
        try:
            if args.mode in ("body", "secure-internal-body", "public-body"):
                body_case(fixture, directory, mode=args.mode)
            else:
                {"tls": tls_case, "proxy": proxy_case,
                 "public-trust": public_trust_case}[args.mode](fixture, directory)
        finally:
            fixture.close()
            (directory / "server-events.json").write_text(json.dumps(fixture.events, indent=2))
            for name in ("a-key.pem", "b-key.pem"):
                with contextlib.suppress(FileNotFoundError):
                    (directory / name).unlink()

    relative_path = Path(__file__).resolve().relative_to(ROOT).as_posix()
    print(f"PASS script:{relative_path}#{args.mode}")


if __name__ == "__main__":
    main()
