import json
import os
import platform
import socket
import subprocess
import sys
import tempfile
import time
import uuid
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen


ROOT = Path.cwd()
REPORT = Path(os.environ.get("CRYPTOERASURE_FAULT_REPORT_DIR", str(Path(tempfile.gettempdir()) / "cryptoerasure-faults"))) / uuid.uuid4().hex
REPORT.mkdir(parents=True)
HTTP_PORT = int(os.environ["TESTBED_FAULT_HTTP_PORT"])
FIXTURE_PORT = int(os.environ["TESTBED_FAULT_FIXTURE_PORT"])
API = f"http://127.0.0.1:{HTTP_PORT}"
FIXTURE = f"http://127.0.0.1:{FIXTURE_PORT}"
DB_HOST = os.environ["DB_HOST"]
DB_PORT = os.environ.get("DB_PORT", "5432")
DB_USER = os.environ.get("DB_USER", "neohaskell")
DB_PASSWORD = os.environ["DB_PASSWORD"]
FAULT = sys.argv[1]
assert FAULT in {"missing-key", "version", "tag", "missing-envelope", "bad-nonce", "bad-ciphertext", "unavailable-store"}, FAULT
MARKER = "Álvaro"
MARKER_ENCODINGS = (MARKER, json.dumps(MARKER)[1:-1])


def sql(database, statement):
    return subprocess.check_output(
        ["psql", "-h", DB_HOST, "-p", DB_PORT, "-U", DB_USER,
         "-d", database, "-v", "ON_ERROR_STOP=1", "-At", "-c", statement],
        text=True,
    ).strip()


def request(method, url, token=None, payload=None):
    headers = {"Content-Type": "application/json"}
    if token is not None:
        headers["Authorization"] = f"Bearer {token}"
    body = json.dumps(payload).encode() if payload is not None else None
    try:
        with urlopen(Request(url, data=body, headers=headers, method=method), timeout=4) as response:
            return response.status, response.read().decode("utf-8", errors="replace")
    except HTTPError as error:
        return error.code, error.read().decode("utf-8", errors="replace")


def wait_health(url):
    for _ in range(60):
        try:
            status, _ = request("GET", url)
            if status == 200:
                return
        except (URLError, TimeoutError):
            pass
        time.sleep(0.25)
    raise AssertionError(f"Server did not become ready at {url}")


def wait_replay():
    for _ in range(80):
        status, body = request("GET", f"{API}/ready")
        if status == 200 or (status == 503 and json.loads(body).get("status") == "failed"):
            return status, body
        time.sleep(0.1)
    raise AssertionError("Query replay did not settle")


def wait_owner(case_id, token):
    for _ in range(80):
        status, body = request("GET", f"{API}/queries/owned-protected-view", token)
        assert status == 200, (status, body)
        page = json.loads(body)
        matching = [item for item in page["items"] if item["caseId"] == case_id]
        if matching:
            return matching[0]
        time.sleep(0.1)
    raise AssertionError(f"Created case {case_id} was not visible")


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

database = "nh_cryptoerasure_fault_" + uuid.uuid4().hex[:12]
os.environ["PGPASSWORD"] = DB_PASSWORD
assert sql("postgres", f"SELECT count(*) FROM pg_database WHERE datname = '{database}'") == "0"
subprocess.run(["createdb", "-h", DB_HOST, "-p", DB_PORT, "-U", DB_USER, database], check=True)
assert sql("postgres", f"SELECT count(*) FROM pg_database WHERE datname = '{database}'") == "1"
(REPORT / "fault-db-name.txt").write_text(database + "\n")

environment = os.environ.copy()
environment.update({
    "TESTBED_FIXTURE_PORT": str(FIXTURE_PORT),
    "TESTBED_OAUTH_PORT": str(HTTP_PORT),
    "OAUTH2_TESTBED_STATE_KEY": "testbed-oauth-state-key-that-is-at-least-32-bytes",
    "DB_HOST": DB_HOST,
    "DB_PORT": DB_PORT,
    "DB_USER": DB_USER,
    "DB_PASSWORD": DB_PASSWORD,
    "DB_NAME": database,
})
bin_command = ["cabal", "list-bin"]
if platform.system() == "Darwin":
    bin_command.append("--enable-executable-dynamic")
bin_command.append("exe:nhoauthtestbed")
app_bin = subprocess.check_output(bin_command, text=True).strip()
app_log = (REPORT / "fault-app.log").open("w")
fixture_log = (REPORT / "fault-fixture.log").open("w")
app = None
fixture = None
try:
    fixture = subprocess.Popen(
        ["python3", "testbed/scripts/fake_external_services.py"],
        cwd=ROOT, env=environment, stdout=fixture_log, stderr=subprocess.STDOUT,
    )
    app = subprocess.Popen([app_bin], cwd=ROOT, env=environment,
                           stdout=app_log, stderr=subprocess.STDOUT)
    wait_health(f"{FIXTURE}/health")
    wait_health(f"{API}/health")
    jwt_status, jwt_body = request("GET", f"{FIXTURE}/fixture/jwt")
    assert jwt_status == 200, (jwt_status, jwt_body)
    token = json.loads(jwt_body)["accessToken"]
    create_status, create_body = request(
        "POST", f"{API}/commands/create-owned-protected", token, []
    )
    assert create_status == 200, (create_status, create_body)
    case_id = str(uuid.UUID(json.loads(create_body)["entityId"]))
    before = wait_owner(case_id, token)
    assert before["firstSecret"]["tag"] == "Available", before
    original_reference = str(uuid.UUID(before["firstReference"]))
    assert MARKER in json.dumps(before, ensure_ascii=False), before
    page_status, page_body = request("GET", f"{API}/queries/owned-protected-view", token)
    assert page_status == 200, (page_status, page_body)
    page = json.loads(page_body)
    assert set(page) == {"items", "total", "hasMore", "effectiveLimit"}, page
    assert page["total"] == 1 and len(page["items"]) == 1 and page["hasMore"] is False, page
    assert isinstance(page["effectiveLimit"], int) and page["effectiveLimit"] >= 1, page
    for forbidden in ("$erasable", "envelope", "context", "key_bytes", "ciphertext"):
        assert forbidden not in page_body, forbidden
    raw_events = sql(database, f"""
        SELECT eventData::text FROM Events
        WHERE entity = 'OwnedProtectedEntity' AND inlinedStreamId = '{case_id}'
    """)
    assert all(marker not in raw_events for marker in MARKER_ENCODINGS), raw_events
    for denied_token in (None, "invalid-token"):
        denied_status, denied_body = request("GET", f"{API}/queries/owned-protected-view", denied_token)
        assert denied_status in (401, 403), (denied_status, denied_body)
        assert case_id not in denied_body and all(marker not in denied_body for marker in MARKER_ENCODINGS), denied_body

    event_rows_changed = 0
    if FAULT == "unavailable-store":
        renamed = sql(database, "ALTER TABLE erasable_keys RENAME TO erasable_keys_unavailable")
        assert renamed == "ALTER TABLE", renamed
        original_key_active = sql(database, f"""
            SELECT count(*) FROM erasable_keys_unavailable
            WHERE reference = '{original_reference}' AND key_bytes IS NOT NULL
        """)
        assert original_key_active == "1", original_key_active
        old_table_absent = sql(database, """
            SELECT count(*) FROM pg_tables
            WHERE schemaname = 'public' AND tablename = 'erasable_keys'
        """)
        assert old_table_absent == "0", old_table_absent
    else:
        stop(app)
        app = None
        missing_reference = str(uuid.uuid4())
        changes = {
            "missing-key": ("{first,reference}", f"to_jsonb('{missing_reference}'::text)"),
            "version": ("{first,envelope,version}", "'99'::jsonb"),
            "tag": ("{first,envelope,tag}", "to_jsonb('invalid-tag'::text)"),
            "missing-envelope": ("{first,envelope}", "'null'::jsonb"),
            "bad-nonce": ("{first,envelope,nonce}", "to_jsonb('invalid-nonce'::text)"),
            "bad-ciphertext": ("{first,envelope,ciphertext}", "to_jsonb('invalid-ciphertext'::text)"),
        }
        path, replacement = changes[FAULT]
        changed = sql(database, f"""
            UPDATE Events
            SET eventData = jsonb_set(eventData, '{path}', {replacement}, false)
            WHERE entity = 'OwnedProtectedEntity'
              AND inlinedStreamId = '{case_id}'
              AND eventData->>'tag' = 'OwnedProtectedCreated'
            RETURNING eventId
        """)
        changed_lines = changed.splitlines()
        assert len(changed_lines) == 2 and changed_lines[1] == "UPDATE 1", changed
        key_states = sql(database, f"""
            SELECT
              (SELECT count(*) FROM erasable_keys WHERE reference = '{original_reference}' AND key_bytes IS NOT NULL),
              (SELECT count(*) FROM erasable_keys WHERE reference = '{missing_reference}')
        """)
        assert key_states == "1|0", key_states
        event_rows_changed = 1

        app_log.write("\nRESTART\n")
        app_log.flush()
        app = subprocess.Popen([app_bin], cwd=ROOT, env=environment,
                               stdout=app_log, stderr=subprocess.STDOUT)
        wait_health(f"{API}/health")
    replay_status, replay_body = wait_replay()
    if FAULT == "missing-envelope":
        assert replay_status == 503 and json.loads(replay_body).get("status") == "failed", (replay_status, replay_body)
    else:
        assert replay_status == 200, (replay_status, replay_body)
    failure_status, failure_body = request(
        "GET", f"{API}/queries/owned-protected-view", token
    )
    assert failure_status == 500, (failure_status, failure_body)
    assert case_id not in failure_body, failure_body
    assert all(marker not in failure_body for marker in MARKER_ENCODINGS), failure_body
    assert original_reference not in failure_body, failure_body
    for denied_token in (None, "invalid-token"):
        denied_status, denied_body = request("GET", f"{API}/queries/owned-protected-view", denied_token)
        assert denied_status in (401, 403), (denied_status, denied_body)
        assert case_id not in denied_body and all(marker not in denied_body for marker in MARKER_ENCODINGS), denied_body
    stop(app)
    app = None
    app_log.flush()
    logged = Path(app_log.name).read_text()
    assert all(marker not in logged for marker in MARKER_ENCODINGS), app_log.name
finally:
    stop(app)
    stop(fixture)
    app_log.close()
    fixture_log.close()

for port in (HTTP_PORT, FIXTURE_PORT):
    with socket.socket() as probe:
        assert probe.connect_ex(("127.0.0.1", port)) != 0, f"Owned listener remains on {port}"
result = {
    "result": "PASS",
    "fault": FAULT,
    "database": database,
    "eventRowsChanged": event_rows_changed,
    "originalKeyActive": True,
    "missingKeyAbsent": True if FAULT == "missing-key" else None,
    "replayStatus": replay_status,
    "httpStatus": failure_status,
    "protectedMarkerAbsent": True,
    "ownedListenersStopped": True,
}
(REPORT / "result.json").write_text(json.dumps(result, sort_keys=True) + "\n")
print(json.dumps(result, sort_keys=True))
