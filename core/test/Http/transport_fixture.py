#!/usr/bin/env python3
"""Synthetic loopback transport regressions. Invoke from the repository root.

Adapted from the PR905 audit server/probes. This executable uses the repository's
native ./dev toolchain, never an external provider or globally installed CA.
The TLS control injects trust into a test manager, not the public cached manager.
"""
import argparse
import contextlib
import gzip
import http.server
import json
import os
from pathlib import Path
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
                owner.event(kind=kind, method=self.command, path=self.path)
                if self.path.startswith("/paced"):
                    self.send_response(200)
                    if self.path.endswith("chunked"):
                        self.send_header("Transfer-Encoding", "chunked")
                        data = b"5\r\n12345\r\n"
                    else:
                        self.send_header("Connection", "close")
                        self.close_connection = True
                        data = b"12345"
                    self.end_headers()
                    self.wfile.write(data)
                    self.wfile.flush()
                    owner.event(kind=kind, method="FLUSHED", path=self.path)
                    started = time.monotonic()
                    self.connection.settimeout(0.1)
                    while not owner.release.is_set() and time.monotonic() - started < 15:
                        try:
                            data = self.connection.recv(1)
                        except socket.timeout:
                            continue
                        if not data:
                            owner.event(kind=kind, method="CLOSED", path=self.path,
                                        elapsed=time.monotonic() - started)
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
                length = 10000 if self.path in ("/head", "/advertised") else len(body)
                if self.path == "/compressed":
                    self.send_header("Content-Encoding", "gzip")
                if self.path == "/advertised":
                    self.send_header("Connection", "close")
                    self.close_connection = True
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


def repl(directory, name, expressions, env):
    source = directory / (name + ".ghci")
    source.write_text(COMMON + expressions + "\n:quit\n")
    command = [str(ROOT / "dev"), "exec", "cabal", "repl", "lib:nhcore",
               "--project-file=cabal.project.dev", "--offline", "-j1", "-v0",
               "--repl-options=-package crypton-x509-store"]
    with source.open() as stream:
        result = subprocess.run(command, cwd=ROOT, env=env, stdin=stream,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                text=True, timeout=120)
    (directory / (name + ".stdout")).write_text(result.stdout)
    (directory / (name + ".stderr")).write_text(result.stderr)
    print(result.stdout, end="")
    print(result.stderr, end="")
    if result.returncode != 0 or "error:" in result.stderr or "*** Exception" in result.stdout:
        raise AssertionError("native REPL compilation/execution failed, not a transport regression")
    return result.stdout


COMMON = """:set -XOverloadedStrings -XTypeApplications -XOverloadedRecordDot
import Prelude qualified as P
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
import Result qualified as Result
"""


def certificate(directory):
    cert, key = directory / "cert.pem", directory / "key.pem"
    command = ["openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes",
               "-keyout", str(key), "-out", str(cert), "-days", "2", "-subj",
               "/CN=localhost", "-addext", "subjectAltName=DNS:localhost"]
    subprocess.run(command, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.minimum_version = ssl.TLSVersion.TLSv1_2
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
    output = repl(directory, "tls", text, clean_env())
    for marker in ("TLS_TRUSTED", "TLS_UNTRUSTED", "TLS_WRONG_HOST", "PUBLIC_UNTRUSTED"):
        assert f'("{marker}",True)' in output, marker
    print("PASS: injected-manager trust and hostname controls; public untrusted rejection")
    print("NOT PROVED: trusted public cached-manager success or system-store integration")


def proxy_case(fixture, directory):
    origin = fixture.start("origin")
    context, _ = certificate(directory)
    secure = fixture.start("tls", context)
    proxy = fixture.start("proxy")
    failures = []
    for case in ("lower", "upper"):
        env = clean_env()
        for scheme in ("http", "https"):
            name = scheme + "_proxy"
            env[name if case == "lower" else name.upper()] = f"http://127.0.0.1:{proxy}"
        text = f"""
controlManager <- HC.newManager HT.tlsManagerSettings
control <- HC.parseRequest "http://127.0.0.1:{origin}/control-{case}"
E.try @HC.HttpException (HC.httpLbs control controlManager) P.>>= P.print
controlTls <- HC.parseRequest "https://localhost:{secure}/control-{case}"
E.try @HC.HttpException (HC.httpLbs controlTls controlManager) P.>>= P.print
P.putStrLn "PROXY_CONTROL_COMPLETE"
Task.runResult (R.sendRaw H.Put (H.addHeader "Authorization" "synthetic-test" (H.withUrl "http://127.0.0.1:{origin}/actual-{case}" H.request)) (T.toBytes "synthetic")) P.>>= P.print
P.putStrLn "RAW_COMPLETE"
Task.runResult (H.sendSecure H.Get (H.withUrl "https://localhost:{secure}/actual-{case}" H.request) B.empty) P.>>= P.print
P.putStrLn "SECURE_COMPLETE"
"""
        before = len(fixture.events)
        output = repl(directory, "proxy-" + case, text, env)
        assert all(marker in output for marker in ("PROXY_CONTROL_COMPLETE", "RAW_COMPLETE", "SECURE_COMPLETE"))
        events = fixture.events[before:]
        control_http = [e for e in events if e["kind"] == "proxy" and e["path"].endswith("/control-" + case)]
        connects = [e for e in events if e["kind"] == "proxy" and e["method"] == "CONNECT"]
        assert control_http and connects, "proxy positive control failed"
        actual_proxy = [e for e in events if e["kind"] == "proxy" and e["path"].endswith("/actual-" + case)]
        actual_origin = [e for e in events if e["kind"] == "origin" and e["path"].endswith("/actual-" + case)]
        if actual_proxy or len(connects) != 1 or not actual_origin:
            failures.append(case)
    assert not failures, "REGRESSION_ASSERT:environment-proxy-traversal:" + ",".join(failures)
    print("PASS: raw origin reached and no actual proxy/CONNECT traversal")
    print("NOT PROVED: secure trusted-origin success; TLS trust is a separate prerequisite")


def body_case(fixture, directory, secure_internal=False):
    port = fixture.start("origin")
    text = f"""
let raw method path cap = Task.runResult (R.sendRaw method (H.withMaxResponseSize cap (H.withUrl (T.fromLinkedList ("http://127.0.0.1:{port}" P.++ path)) H.request)) B.empty)
exact <- raw H.Get "/exact" 4
empty <- raw H.Get "/empty" 1
headOnly <- raw H.Head "/head" 1
oversized <- raw H.Get "/over" 4
advertised <- raw H.Get "/advertised" 4
decoded <- raw H.Get "/compressed" 64
P.print ("OVER" :: P.String, case oversized of Result.Err (H.ResponseTooLarge 4) -> P.True; _ -> P.False)
P.print ("ADVERTISED" :: P.String, case advertised of Result.Err (H.ResponseTooLarge 4) -> P.True; _ -> P.False)
P.print ("DECOMPRESSED" :: P.String, case decoded of Result.Err (H.ResponseTooLarge 64) -> P.True; _ -> P.False)
zeroBuilder <- E.try @E.ErrorCall (E.evaluate (H.withMaxResponseSize 0 H.request))
P.print ("ZERO_REJECTED" :: P.String, P.either (P.const P.True) (P.const P.False) zeroBuilder)
P.print ("EXACT" :: P.String, case exact of Result.Ok response -> B.length response.body P.== 4; Result.Err _ -> P.False)
P.print ("ZERO" :: P.String, case empty of Result.Ok response -> B.length response.body P.== 0; Result.Err _ -> P.False)
P.print ("HEAD" :: P.String, case headOnly of Result.Ok response -> B.length response.body P.== 0; Result.Err _ -> P.False)
P.putStrLn "BODY_CONTROLS_COMPLETE"
Timeout.timeout 2000000 (raw H.Get "/paced-chunked" 4) P.>>= P.print
P.putStrLn "CHUNKED_COMPLETE"
Timeout.timeout 2000000 (raw H.Get "/paced-close" 4) P.>>= P.print
P.putStrLn "UNKNOWN_COMPLETE"
"""
    if secure_internal:
        text = ":module + *Http.Client\n" + text.replace(
            "Task.runResult (R.sendRaw method", "sendSecureRawIO method").replace(
            ")) B.empty)", ")) B.empty")
    output = repl(directory, "secure-internal-body" if secure_internal else "body", text, clean_env())
    control_output = output.split("BODY_CONTROLS_COMPLETE")[0]
    assert all(f'("{name}",True)' in control_output for name in ("EXACT", "ZERO", "HEAD", "ZERO_REJECTED", "OVER", "ADVERTISED", "DECOMPRESSED")), "exact/empty/HEAD/zero-builder control failed"
    for path in ("/paced-chunked", "/paced-close"):
        assert any(e["method"] == "FLUSHED" and e["path"] == path for e in fixture.events), "source never flushed"
    assert output.count("Just (Err (ResponseTooLarge 4))") == 2, "REGRESSION_ASSERT:body-read-awaits-EOF"
    for path in ("/paced-chunked", "/paced-close"):
        # The Just/ResponseTooLarge assertion above proves the client completed
        # without Timeout.timeout cancelling it. Socket timing alone cannot prove
        # that: a cancelled unbounded read also closes at roughly two seconds.
        assert any(e["method"] == "CLOSED" and e["path"] == path
                   for e in fixture.events), "response did not close after bounded completion"
        assert not any(e["method"] == "SOURCE_EOF" and e["path"] == path
                       for e in fixture.events), "source completed before client rejection"
    print("PASS: exact/empty/HEAD, zero-builder rejection, early cutoff and socket close")
    if secure_internal:
        print("DIAGNOSTIC ONLY: real secure reader over plaintext; public HTTPS trust not exercised")


def public_trust_case(fixture, directory):
    if sys.platform == "darwin" or sys.platform == "win32":
        print("UNSUPPORTED: pinned native trust loader has no isolated environment bundle override")
        raise SystemExit(77)
    context, cert = certificate(directory)
    port = fixture.start("tls", context)
    env = clean_env()
    env["SYSTEM_CERTIFICATE_PATH"] = str(cert)
    text = f"""
controlManager <- HC.newManager (HC.managerSetProxy HC.noProxy HT.tlsManagerSettings)
controlRequest <- HC.parseRequest "https://localhost:{port}/library-control"
control <- E.try @HC.HttpException (HC.httpLbs controlRequest controlManager)
P.print ("SYSTEM_BUNDLE_CONTROL" :: P.String, P.either (P.const P.False) (\\response -> HC.responseStatus response P.== Status.status200 P.&& HC.responseBody response P.== "1234") control)
public <- Task.runResult (H.sendSecure H.Get (H.withUrl "https://localhost:{port}/public-trusted" H.request) B.empty)
P.print ("PUBLIC_TRUSTED" :: P.String, case public of Result.Ok response -> response.statusCode P.== 200 P.&& response.body P.== T.toBytes "1234"; Result.Err _ -> P.False)
"""
    output = repl(directory, "public-trust", text, env)
    assert '("SYSTEM_BUNDLE_CONTROL",True)' in output, "isolated system bundle positive control failed"
    assert '("PUBLIC_TRUSTED",True)' in output, "REGRESSION_ASSERT:public-trust-store-not-loaded"
    print("PASS: actual public manager accepts the isolated native system bundle")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("mode", choices=("tls", "proxy", "body", "secure-internal-body", "public-trust"))
    parser.add_argument("--evidence-dir", type=Path)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="nh-http-fixture-") as temp:
        directory = args.evidence_dir.resolve() if args.evidence_dir else Path(temp)
        directory.mkdir(parents=True, exist_ok=True)
        fixture = Fixture(directory)
        try:
            if args.mode == "secure-internal-body":
                body_case(fixture, directory, secure_internal=True)
            else:
                {"tls": tls_case, "proxy": proxy_case, "body": body_case,
                 "public-trust": public_trust_case}[args.mode](fixture, directory)
        finally:
            fixture.close()
            (directory / "server-events.json").write_text(json.dumps(fixture.events, indent=2))
            with contextlib.suppress(FileNotFoundError):
                (directory / "key.pem").unlink()


if __name__ == "__main__":
    main()
