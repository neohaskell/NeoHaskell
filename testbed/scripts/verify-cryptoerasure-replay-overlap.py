#!/usr/bin/env python3
"""Exercise an authenticated query held across PostgreSQL replay completion."""

import json
import os
import platform
import socket
import subprocess
import tempfile
import time
import uuid
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen


ROOT = Path.cwd()
REPORT = Path(os.environ.get("CRYPTOERASURE_REPLAY_REPORT_DIR", str(Path(tempfile.gettempdir()) / "cryptoerasure-replay"))) / uuid.uuid4().hex
REPORT.mkdir(parents=True)
GATE = REPORT / "gate"
GATE.mkdir()
HTTP_PORT = int(os.environ["TESTBED_FAULT_HTTP_PORT"])
FIXTURE_PORT = int(os.environ["TESTBED_FAULT_FIXTURE_PORT"])
API = f"http://127.0.0.1:{HTTP_PORT}"
FIXTURE = f"http://127.0.0.1:{FIXTURE_PORT}"
DB_HOST = os.environ["DB_HOST"]
DB_PORT = os.environ.get("DB_PORT", "5432")
DB_USER = os.environ.get("DB_USER", "neohaskell")
DB_PASSWORD = os.environ["DB_PASSWORD"]
MARKER = "Álvaro"


def sql(database, statement):
    return subprocess.check_output(
        ["psql", "-h", DB_HOST, "-p", DB_PORT, "-U", DB_USER,
         "-d", database, "-v", "ON_ERROR_STOP=1", "-At", "-c", statement],
        text=True,
    ).strip()


def request(method, url, token=None, payload=None):
    headers = {}
    if token is not None:
        headers["Authorization"] = f"Bearer {token}"
    body = None
    if payload is not None:
        headers["Content-Type"] = "application/json"
        body = json.dumps(payload).encode()
    try:
        with urlopen(Request(url, data=body, headers=headers, method=method), timeout=20) as response:
            return response.status, response.read().decode("utf-8", errors="replace")
    except HTTPError as error:
        return error.code, error.read().decode("utf-8", errors="replace")


def wait_http(url):
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        try:
            status, _ = request("GET", url)
            if status == 200:
                return
        except (URLError, TimeoutError):
            pass
        time.sleep(0.05)
    raise AssertionError(f"Server did not respond at {url}")


def wait_file(path):
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        if path.exists():
            return
        time.sleep(0.02)
    raise AssertionError(f"Timed out waiting for {path.name}")


def wait_ready(expected_status, expected_state):
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        status, body = request("GET", f"{API}/ready")
        if status == expected_status and json.loads(body).get("status") == expected_state:
            return
        time.sleep(0.05)
    raise AssertionError(f"Readiness did not become {expected_state}: {status} {body}")


def wait_owner(case_id, token):
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        status, body = request("GET", f"{API}/queries/owned-protected-view", token)
        assert status == 200, (status, body)
        page = json.loads(body)
        matches = [item for item in page["items"] if item["caseId"] == case_id]
        if matches:
            assert len(matches) == 1, page
            return page, matches[0]
        time.sleep(0.05)
    raise AssertionError(f"Owned case {case_id} did not appear")


def stop(process):
    if process is not None and process.poll() is None:
        process.terminate()
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait(timeout=5)


for port in (HTTP_PORT, FIXTURE_PORT):
    with socket.socket() as probe:
        assert probe.connect_ex(("127.0.0.1", port)) != 0, f"Port {port} is occupied"

os.environ["PGPASSWORD"] = DB_PASSWORD
database = "nh_cryptoerasure_replay_" + uuid.uuid4().hex[:12]
assert sql("postgres", f"SELECT count(*) FROM pg_database WHERE datname = '{database}'") == "0"
subprocess.run(["createdb", "-h", DB_HOST, "-p", DB_PORT, "-U", DB_USER, database], check=True)
assert sql("postgres", f"SELECT count(*) FROM pg_database WHERE datname = '{database}'") == "1"
(REPORT / "database.txt").write_text(database + "\n")

environment = os.environ.copy()
environment.update({
    "TESTBED_FIXTURE_PORT": str(FIXTURE_PORT),
    "TESTBED_OAUTH_PORT": str(HTTP_PORT),
    "OAUTH2_TESTBED_STATE_KEY": "testbed-oauth-state-key-that-is-at-least-32-bytes",
    "DB_NAME": database,
})
bin_command = ["cabal"]
if os.environ.get("TESTBED_CABAL_BUILDDIR"):
    bin_command.append(f"--builddir={os.environ['TESTBED_CABAL_BUILDDIR']}")
bin_command.append("list-bin")
if platform.system() == "Darwin":
    bin_command.append("--enable-executable-dynamic")
bin_command.append("exe:nhoauthtestbed")
app_bin = subprocess.check_output(bin_command, text=True).strip()
app_log = (REPORT / "app.log").open("w")
fixture_log = (REPORT / "fixture.log").open("w")
app = None
fixture = None
try:
    fixture = subprocess.Popen(
        ["python3", "testbed/scripts/fake_external_services.py"],
        cwd=ROOT, env=environment, stdout=fixture_log, stderr=subprocess.STDOUT,
    )
    app = subprocess.Popen([app_bin], cwd=ROOT, env=environment,
                           stdout=app_log, stderr=subprocess.STDOUT)
    wait_http(f"{FIXTURE}/health")
    wait_http(f"{API}/health")
    wait_ready(200, "ready")
    token_status, token_body = request("GET", f"{FIXTURE}/fixture/jwt")
    assert token_status == 200, (token_status, token_body)
    token = json.loads(token_body)["accessToken"]
    create_status, create_body = request("POST", f"{API}/commands/create-owned-protected", token, [])
    assert create_status == 200, (create_status, create_body)
    case_id = str(uuid.UUID(json.loads(create_body)["entityId"]))
    _, seeded = wait_owner(case_id, token)
    assert seeded["firstSecret"]["tag"] == "Available", seeded
    assert MARKER in json.dumps(seeded, ensure_ascii=False), seeded
    stored = sql(database, f"SELECT eventData::text FROM Events WHERE entity = 'OwnedProtectedEntity' AND inlinedStreamId = '{case_id}'")
    assert stored and MARKER not in stored and "\\u00c1lvaro" not in stored, stored
    stop(app)
    app = None

    replay_environment = environment.copy()
    replay_environment["CRYPTOERASURE_REPLAY_GATE_DIR"] = str(GATE)
    app_log.write("\nRESTART WITH REPLAY GATE\n")
    app_log.flush()
    app = subprocess.Popen([app_bin], cwd=ROOT, env=replay_environment,
                           stdout=app_log, stderr=subprocess.STDOUT)
    wait_http(f"{API}/health")
    wait_file(GATE / "replay-paused")
    wait_ready(503, "rebuilding")
    with ThreadPoolExecutor(max_workers=1) as executor:
        pending = executor.submit(request, "GET", f"{API}/queries/owned-protected-view", token)
        try:
            wait_file(GATE / "query-captured")
            assert (GATE / "query-captured").read_text() == "0", "Query did not capture the empty replay projection"
            assert not pending.done(), "Query returned before replay advanced"
            wait_ready(503, "rebuilding")
            for denied_token in (None, "invalid-token"):
                denied_status, denied_body = request("GET", f"{API}/queries/owned-protected-view", denied_token)
                assert denied_status in (401, 403), (denied_status, denied_body)
                assert case_id not in denied_body and MARKER not in denied_body, denied_body
            (GATE / "replay-release").write_text("release\n")
            wait_ready(200, "ready")
        finally:
            (GATE / "replay-release").write_text("release\n")
            (GATE / "query-release").write_text("release\n")
        status, body = pending.result(timeout=20)
    assert status == 500, (status, body)
    assert case_id not in body and MARKER not in body and "\\u00c1lvaro" not in body, body
    page, recovered = wait_owner(case_id, token)
    assert page["total"] == 1 and len(page["items"]) == 1, page
    assert recovered["firstSecret"]["tag"] == "Available", recovered
    for denied_token in (None, "invalid-token"):
        denied_status, denied_body = request("GET", f"{API}/queries/owned-protected-view", denied_token)
        assert denied_status in (401, 403), (denied_status, denied_body)
        assert case_id not in denied_body and MARKER not in denied_body, denied_body
finally:
    (GATE / "replay-release").write_text("release\n")
    (GATE / "query-release").write_text("release\n")
    stop(app)
    stop(fixture)
    app_log.close()
    fixture_log.close()

for port in (HTTP_PORT, FIXTURE_PORT):
    with socket.socket() as probe:
        assert probe.connect_ex(("127.0.0.1", port)) != 0, f"Owned listener remains on {port}"
result = {
    "result": "PASS",
    "database": database,
    "replayPaused": True,
    "queryCaptured": True,
    "replayReadyBeforeQueryRelease": True,
    "interleavedHttpStatus": status,
    "freshPageTotal": page["total"],
    "protectedMarkerAbsentFromFailure": True,
    "ownedListenersStopped": True,
}
(REPORT / "result.json").write_text(json.dumps(result, sort_keys=True) + "\n")
print(json.dumps(result, sort_keys=True))
