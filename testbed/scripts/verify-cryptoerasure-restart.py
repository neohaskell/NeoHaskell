#!/usr/bin/env python3
"""Assert that an application retries a committed erasure after its process restarts."""

import json
import os
import subprocess
import sys
import time
import uuid
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from urllib.error import HTTPError
from urllib.request import Request, urlopen


API = f"http://127.0.0.1:{os.environ.get('TESTBED_OAUTH_PORT', '8081')}"
FIXTURE = f"http://127.0.0.1:{os.environ.get('TESTBED_FIXTURE_PORT', '8181')}"


def request(method, url, token=None, body=None):
    headers = {}
    if token:
        headers["Authorization"] = f"Bearer {token}"
    if body is not None:
        headers["Content-Type"] = "application/json"
        body = json.dumps(body).encode()
    try:
        with urlopen(Request(url, body, headers, method=method), timeout=5) as response:
            return response.status, json.load(response)
    except HTTPError as error:
        return error.code, json.load(error)


def owner_token():
    status, body = request("GET", f"{FIXTURE}/fixture/jwt")
    assert status == 200, (status, body)
    return body["accessToken"]


def view_for(case_id, token):
    status, body = request("GET", f"{API}/queries/owned-protected-view", token)
    assert status == 200, (status, body)
    matches = [item for item in body["items"] if item["caseId"] == case_id]
    assert len(matches) <= 1, (case_id, body)
    return matches[0] if matches else None


def wait_for(case_id, token, predicate):
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        view = view_for(case_id, token)
        if view is not None and predicate(view):
            return view
        time.sleep(0.1)
    raise AssertionError(f"Timed out waiting for erasure state: {view}")


def assert_erased_wire(view):
    assert view["firstSecret"] == {"tag": "Erased", "contents": None}, view
    assert "$erasable" not in json.dumps(view), view
    assert "envelope" not in json.dumps(view), view


def verify_callback_log(path, log_path):
    case_id = json.loads(path.read_text())["caseId"]
    deadline = time.monotonic() + 10
    while time.monotonic() < deadline:
        log_text = log_path.read_text()
        assert "Álvaro" not in log_text and "\\u00c1lvaro" not in log_text, "Protected value appeared in application log"
        assert "$erasable" not in log_text, "Protected envelope appeared in application log"
        records = []
        for line in log_text.splitlines():
            if line.startswith("{") and line.endswith("}"):
                records.append(json.loads(line))
        failures = [record for record in records
                    if record.get("kind") == "integration_result"
                    and record.get("streamId") == case_id
                    and record.get("failureStage") == "callback_dispatch"]
        if failures:
            assert len(failures) == 1 and failures[0]["result"] == "failed", failures
            print("Rejected result command was logged as callback dispatch failure without protected data")
            return
        time.sleep(0.1)
    raise AssertionError("Rejected result command was not logged as callback dispatch failure")


def persisted_case(database, case_id, reference):
    safe_id = str(uuid.UUID(case_id))
    command = ["psql", "-h", os.environ.get("DB_HOST", "localhost"),
               "-p", os.environ.get("DB_PORT", "5432"),
               "-U", os.environ.get("DB_USER", "neohaskell"),
               "-d", database, "-At"]
    keys = subprocess.check_output(command + ["-F", "|", "-c",
        "SELECT reference, key_bytes IS NULL FROM erasable_keys"], text=True)
    key_states = dict(line.split("|", 1) for line in keys.splitlines())
    assert key_states.get(reference) == "t", (case_id, key_states.get(reference))
    events_text = subprocess.check_output(command + ["-c",
        f"SELECT eventData::text FROM Events WHERE inlinedStreamId = '{safe_id}' "
        "AND entity = 'OwnedProtectedEntity' ORDER BY localPosition"], text=True)
    events = [json.loads(line) for line in events_text.splitlines()]
    assert any(event.get("tag") == "OwnedProtectedCreated" and event.get("caseStatus") == "open" for event in events), (case_id, events)
    completions = [event for event in events if event.get("tag") == "ErasureCompleted"]
    assert len(events) >= 3 and completions, (case_id, events)
    assert all("staleFirst" in event and reference in json.dumps(event)
               for event in completions), (case_id, completions)
    assert "Álvaro" not in events_text, (case_id, events_text)


def assert_key_tombstoned(database, reference):
    safe_reference = str(uuid.UUID(reference))
    command = ["psql", "-h", os.environ.get("DB_HOST", "localhost"),
               "-p", os.environ.get("DB_PORT", "5432"),
               "-U", os.environ.get("DB_USER", "neohaskell"),
               "-d", database, "-At", "-c",
               f"SELECT count(*), count(*) FILTER (WHERE key_bytes IS NULL) "
               f"FROM erasable_keys WHERE reference = '{safe_reference}'"]
    assert subprocess.check_output(command, text=True).strip() == "1|1", reference


def concurrent_case(token):
    status, result = request("POST", f"{API}/commands/create-owned-protected", token, [])
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    case_id = result["entityId"]
    loaded = wait_for(case_id, token, lambda item: bool(item.get("firstReference")))
    reference = loaded["firstReference"]
    assert loaded["firstSecret"]["tag"] == "Available", loaded
    assert loaded["caseStatus"] == "open", loaded
    status, result = request("POST", f"{API}/commands/request-erasure", token, {
        "tag": "RequestFirstErasure", "caseId": case_id,
        "reference": reference, "rejectResult": False,
        "simulateUnknownReference": False, "pauseAfterCommit": True,
    })
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    completed = wait_for(case_id, token, lambda item: item["erasureStatus"] == "completed")
    assert_erased_wire(completed)
    assert completed["firstSecret"]["tag"] == "Erased", completed
    assert completed["staleSecret"]["tag"] == "Erased", completed
    assert completed["secondSecret"]["tag"] == "Available", completed
    assert completed["caseStatus"] == "open", completed
    assert completed["wasAlreadyErased"] is False, completed
    return case_id, reference, loaded["ownerId"]


def concurrent(database):
    token = owner_token()
    with ThreadPoolExecutor(max_workers=4) as executor:
        cases = list(executor.map(lambda _: concurrent_case(token), range(4)))
    references = [reference for _, reference, _ in cases]
    assert len(set(references)) == len(references), cases
    with ThreadPoolExecutor(max_workers=4) as executor:
        retries = list(executor.map(lambda item: request("POST", f"{API}/commands/request-erasure", token, {
            "tag": "RetryFirstErasure", "caseId": item[0],
        }), cases))
    assert all(status == 200 and body["tag"] == "Accepted" for status, body in retries), retries
    for case_id, reference, owner in cases:
        completed = wait_for(case_id, token, lambda item: item["erasureStatus"] == "completed" and item["wasAlreadyErased"] is True)
        assert completed["firstReference"] == reference, completed
        assert completed["ownerId"] == owner, completed
        assert completed["firstSecret"]["tag"] == "Erased", completed
        assert_erased_wire(completed)
        assert completed["staleSecret"]["tag"] == "Erased", completed
        assert completed["secondSecret"]["tag"] == "Available", completed
        assert completed["caseStatus"] == "open", completed
        persisted_case(database, case_id, reference)
    print("Four concurrent application requests and explicit retries kept independent tombstones")


def prepare(path, pause_after_commit=False, marker_path=None, selector_path=None):
    token = owner_token()
    status, result = request("POST", f"{API}/commands/create-owned-protected", token, [])
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    case_id = result["entityId"]
    view = wait_for(case_id, token, lambda item: bool(item.get("firstReference")))
    reference = view["firstReference"]
    assert view["firstSecret"]["tag"] == "Available", view
    assert view["secondSecret"]["tag"] == "Available", view
    if pause_after_commit:
        selector_path.write_text(case_id)
    status, result = request("POST", f"{API}/commands/request-erasure", token, {
        "tag": "RequestFirstErasure", "caseId": case_id,
        "reference": reference, "rejectResult": not pause_after_commit,
        "simulateUnknownReference": False,
    })
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    if pause_after_commit:
        deadline = time.monotonic() + 15
        while time.monotonic() < deadline:
            if marker_path.exists() and marker_path.read_text() == reference:
                break
            time.sleep(0.1)
        else:
            raise AssertionError("Erasure did not reach the committed-key pause")
    wait_for(case_id, token, lambda item: item["firstSecret"]["tag"] == "Erased" and item["erasureStatus"] == "pending")
    assert_erased_wire(view_for(case_id, token))
    path.write_text(json.dumps({"caseId": case_id, "reference": reference, "ownerId": view["ownerId"], "caseStatus": view["caseStatus"]}))
    if pause_after_commit:
        print("Key deletion committed and the result callback is paused before process death")
    else:
        print("Rejected result command left the application request pending before restart")


def verify(path):
    token = owner_token()
    state = json.loads(path.read_text())
    case_id = state["caseId"]
    view = wait_for(case_id, token, lambda item: True)
    assert view["firstReference"] == state["reference"], view
    assert view["ownerId"] == state["ownerId"], view
    assert view["caseStatus"] == state["caseStatus"] == "open", view
    assert view["firstSecret"]["tag"] == "Erased", view
    assert_erased_wire(view)
    assert view["secondSecret"]["tag"] == "Available", view
    assert view["erasureStatus"] == "pending", view
    status, result = request("POST", f"{API}/commands/request-erasure", token, {
        "tag": "RetryFirstErasure", "caseId": case_id,
    })
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    completed = wait_for(case_id, token, lambda item: item["erasureStatus"] == "completed")
    assert completed["wasAlreadyErased"] is True, completed
    assert completed["firstSecret"]["tag"] == "Erased", completed
    assert_erased_wire(completed)
    assert completed["staleSecret"]["tag"] == "Erased", completed
    assert completed["secondSecret"]["tag"] == "Available", completed
    print("Explicit retry completed after restart with AlreadyErased")


def verify_completed(path, database):
    token = owner_token()
    state = json.loads(path.read_text())
    completed = wait_for(state["caseId"], token, lambda item: item["erasureStatus"] == "completed")
    assert completed["firstReference"] == state["reference"], completed
    assert completed["ownerId"] == state["ownerId"], completed
    assert completed["caseStatus"] == state["caseStatus"] == "open", completed
    assert completed["firstSecret"]["tag"] == "Erased", completed
    assert completed["secondSecret"]["tag"] == "Available", completed
    assert completed["wasAlreadyErased"] is True, completed
    persisted_case(database, state["caseId"], state["reference"])
    print("Reconstructed application completion and erased projection after second restart")


def prepare_pre_action(path, marker_path, selector_path):
    token = owner_token()
    status, result = request("POST", f"{API}/commands/create-owned-protected", token, [])
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    case_id = result["entityId"]
    view = wait_for(case_id, token, lambda item: bool(item.get("firstReference")))
    reference = view["firstReference"]
    assert view["firstSecret"]["tag"] == "Available", view
    selector_path.write_text(case_id)
    status, result = request("POST", f"{API}/commands/request-erasure", token, {
        "tag": "RequestFirstErasure", "caseId": case_id,
        "reference": reference, "rejectResult": False,
        "simulateUnknownReference": False,
    })
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        if marker_path.exists() and marker_path.read_text() == reference:
            break
        time.sleep(0.1)
    else:
        raise AssertionError("Erasure did not pause before its key action")
    pending = wait_for(case_id, token, lambda item: item["erasureStatus"] == "pending")
    assert pending["firstSecret"]["tag"] == "Available", pending
    path.write_text(json.dumps({"caseId": case_id, "reference": reference}))
    print("Request persisted before the key action and process death")


def verify_pre_action(path, database, message="A pre-action crash left the key active, then explicit retry erased it"):
    token = owner_token()
    state = json.loads(path.read_text())
    case_id = state["caseId"]
    reference = state["reference"]
    pending = wait_for(case_id, token, lambda item: item["erasureStatus"] == "pending")
    assert pending["firstSecret"]["tag"] == "Available", pending
    assert pending["secondSecret"]["tag"] == "Available", pending
    safe_reference = str(uuid.UUID(reference))
    command = ["psql", "-h", os.environ.get("DB_HOST", "localhost"),
               "-p", os.environ.get("DB_PORT", "5432"),
               "-U", os.environ.get("DB_USER", "neohaskell"),
               "-d", database, "-At", "-c",
               f"SELECT count(*) FROM erasable_keys WHERE reference = '{safe_reference}' AND key_bytes IS NOT NULL"]
    assert subprocess.check_output(command, text=True).strip() == "1"
    status, result = request("POST", f"{API}/commands/request-erasure", token, {
        "tag": "RetryFirstErasure", "caseId": case_id,
    })
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    completed = wait_for(case_id, token, lambda item: item["erasureStatus"] == "completed")
    assert completed["wasAlreadyErased"] is False, completed
    assert_erased_wire(completed)
    assert completed["secondSecret"]["tag"] == "Available", completed
    print(message)


def prepare_queue_timeout(path, marker_path, selector_path, log_path, database):
    prepare_pre_action(path, marker_path, selector_path)
    state = json.loads(path.read_text())
    case_id = state["caseId"]
    token = owner_token()
    for _ in range(2):
        status, result = request("POST", f"{API}/commands/request-erasure", token, {
            "tag": "RetryFirstErasure", "caseId": case_id,
        })
        assert status == 200 and result["tag"] == "Accepted", (status, result)
    deadline = time.monotonic() + 10
    timeout_line = f"Dropped event for stream {case_id} (channel write timeout)"
    while time.monotonic() < deadline:
        log_text = log_path.read_text()
        assert "Álvaro" not in log_text and "\\u00c1lvaro" not in log_text, "Protected value appeared in application log"
        assert "$erasable" not in log_text, "Protected envelope appeared in application log"
        if timeout_line in log_text:
            break
        time.sleep(0.1)
    else:
        raise AssertionError(f"Protected erasure queue timeout was not observed for {case_id}")
    pending = wait_for(case_id, token, lambda item: item["erasureStatus"] == "pending")
    assert pending["firstSecret"]["tag"] == "Available", pending
    assert pending["secondSecret"]["tag"] == "Available", pending
    safe_id = str(uuid.UUID(case_id))
    safe_reference = str(uuid.UUID(state["reference"]))
    command = ["psql", "-h", os.environ.get("DB_HOST", "localhost"),
               "-p", os.environ.get("DB_PORT", "5432"),
               "-U", os.environ.get("DB_USER", "neohaskell"),
               "-d", database, "-At", "-c",
               f"SELECT (SELECT count(*) FROM Events WHERE inlinedStreamId = '{safe_id}' "
               "AND eventData->>'tag' = 'ErasureRequested'), "
               f"(SELECT count(*) FROM erasable_keys WHERE reference = '{safe_reference}' "
               "AND key_bytes IS NOT NULL)"]
    assert subprocess.check_output(command, text=True).strip() == "3|1", case_id
    print("A full protected queue dropped one request while the key stayed active and the application stayed pending")


def prepare_callback_exception(path, selector_path, log_path, database):
    token = owner_token()
    status, result = request("POST", f"{API}/commands/create-owned-protected", token, [])
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    case_id = result["entityId"]
    view = wait_for(case_id, token, lambda item: bool(item.get("firstReference")))
    reference = view["firstReference"]
    selector_path.write_text(case_id)
    status, result = request("POST", f"{API}/commands/request-erasure", token, {
        "tag": "RequestFirstErasure", "caseId": case_id,
        "reference": reference, "rejectResult": False,
        "simulateUnknownReference": False,
    })
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        records = [json.loads(line) for line in log_path.read_text().splitlines()
                   if line.startswith("{") and line.endswith("}")]
        dispatches = [record for record in records
                      if record.get("kind") == "integration_result"
                      and record.get("streamId") == case_id
                      and record.get("errorClass") == "CallbackDispatchFailure"
                      and record.get("failureStage") == "callback_dispatch"
                      and record.get("result") == "failed"]
        commands = [record for record in records
                    if record.get("kind") == "command_result"
                    and record.get("commandName") == "RecordErasureResult"
                    and record.get("errorClass") == "SynchronousException"
                    and record.get("failureStage") == "execution"
                    and record.get("eventsAppended") == 0
                    and record.get("result") == "execution_failed"]
        if len(dispatches) == 1 and len([record for record in commands
                                        if record.get("correlationId") == dispatches[0].get("correlationId")
                                        and record.get("causationId") == dispatches[0].get("eventId")]) == 1:
            break
        time.sleep(0.1)
    else:
        raise AssertionError(f"Result callback exception records were not observed for {case_id}")
    pending = wait_for(case_id, token, lambda item: item["erasureStatus"] == "pending")
    assert_erased_wire(pending)
    assert pending["secondSecret"]["tag"] == "Available", pending
    assert_key_tombstoned(database, reference)
    path.write_text(json.dumps({"caseId": case_id, "reference": reference, "ownerId": view["ownerId"], "caseStatus": view["caseStatus"]}))
    print("Result callback exception left the committed erasure pending")


def verify_callback_exception(path, database):
    verify(path)
    state = json.loads(path.read_text())
    persisted_case(database, state["caseId"], state["reference"])


def prepare_action_failure(path, selector_path, log_path, database):
    token = owner_token()
    status, result = request("POST", f"{API}/commands/create-owned-protected", token, [])
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    case_id = result["entityId"]
    view = wait_for(case_id, token, lambda item: bool(item.get("firstReference")))
    reference = view["firstReference"]
    selector_path.write_text(case_id)
    status, result = request("POST", f"{API}/commands/request-erasure", token, {
        "tag": "RequestFirstErasure", "caseId": case_id,
        "reference": reference, "rejectResult": False,
        "simulateUnknownReference": False,
    })
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        records = [json.loads(line) for line in log_path.read_text().splitlines()
                   if line.startswith("{") and line.endswith("}")]
        failures = [record for record in records
                    if record.get("kind") == "integration_result"
                    and record.get("streamId") == case_id
                    and record.get("errorClass") == "RunnerProcessingFailure"
                    and record.get("failureStage") == "runner_processing"
                    and record.get("result") == "failed"]
        if len(failures) == 1:
            break
        time.sleep(0.1)
    else:
        raise AssertionError(f"Typed erasure action failure was not logged for {case_id}")
    log_text = log_path.read_text()
    assert "Álvaro" not in log_text and "\\u00c1lvaro" not in log_text, "Protected value appeared in application log"
    assert "$erasable" not in log_text, "Protected envelope appeared in application log"
    pending = wait_for(case_id, token, lambda item: item["erasureStatus"] == "pending")
    assert pending["firstSecret"]["tag"] == "Available", pending
    assert pending["secondSecret"]["tag"] == "Available", pending
    safe_reference = str(uuid.UUID(reference))
    command = ["psql", "-h", os.environ.get("DB_HOST", "localhost"),
               "-p", os.environ.get("DB_PORT", "5432"),
               "-U", os.environ.get("DB_USER", "neohaskell"),
               "-d", database, "-At", "-c",
               f"SELECT count(*) FROM erasable_keys WHERE reference = '{safe_reference}' AND key_bytes IS NOT NULL"]
    assert subprocess.check_output(command, text=True).strip() == "1"
    path.write_text(json.dumps({"caseId": case_id, "reference": reference}))
    print("Typed erasure action failure left the application request pending and key active")


def verify_action_failure(path, database):
    verify_pre_action(path, database, "A typed action failure left the key active, then explicit retry erased it")
    state = json.loads(path.read_text())
    persisted_case(database, state["caseId"], state["reference"])


if __name__ == "__main__":
    action = sys.argv[1]
    if action == "concurrent":
        concurrent(sys.argv[2])
    elif action == "prepare":
        state_path = sys.argv[2]
        prepare(Path(state_path))
    elif action == "prepare-crash":
        prepare(Path(sys.argv[2]), True, Path(sys.argv[3]), Path(sys.argv[4]))
    elif action == "verify":
        state_path = sys.argv[2]
        verify(Path(state_path))
    elif action == "verify-completed":
        verify_completed(Path(sys.argv[2]), sys.argv[3])
    elif action == "prepare-pre-action":
        prepare_pre_action(Path(sys.argv[2]), Path(sys.argv[3]), Path(sys.argv[4]))
    elif action == "prepare-queue-timeout":
        prepare_queue_timeout(Path(sys.argv[2]), Path(sys.argv[3]), Path(sys.argv[4]), Path(sys.argv[5]), sys.argv[6])
    elif action == "verify-pre-action":
        verify_pre_action(Path(sys.argv[2]), sys.argv[3])
    elif action == "prepare-callback-exception":
        prepare_callback_exception(Path(sys.argv[2]), Path(sys.argv[3]), Path(sys.argv[4]), sys.argv[5])
    elif action == "verify-callback-exception":
        verify_callback_exception(Path(sys.argv[2]), sys.argv[3])
    elif action == "prepare-action-failure":
        prepare_action_failure(Path(sys.argv[2]), Path(sys.argv[3]), Path(sys.argv[4]), sys.argv[5])
    elif action == "verify-action-failure":
        verify_action_failure(Path(sys.argv[2]), sys.argv[3])
    elif action == "verify-callback-log":
        verify_callback_log(Path(sys.argv[2]), Path(sys.argv[3]))
    else:
        raise SystemExit("unknown cryptoerasure restart assertion")
