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


def verify_other_owner_denial(database):
    token = owner_token()
    status, created = request("POST", f"{API}/commands/create-owned-protected", token, [])
    assert status == 200 and created["tag"] == "Accepted", (status, created)
    case_id = str(uuid.UUID(created["entityId"]))
    owned = wait_for(case_id, token, lambda item: bool(item.get("firstReference")))
    reference = str(uuid.UUID(owned["firstReference"]))

    status, fixture = request("GET", f"{FIXTURE}/fixture/other-jwt")
    assert status == 200, (status, fixture)
    other_token = fixture["accessToken"]
    status, other_page = request("GET", f"{API}/queries/owned-protected-view", other_token)
    assert status == 200, (status, other_page)
    assert all(item["caseId"] != case_id for item in other_page["items"]), other_page
    status, rejected = request("POST", f"{API}/commands/request-erasure", other_token, {
        "tag": "RequestFirstErasure", "caseId": case_id, "reference": reference,
        "rejectResult": False, "simulateUnknownReference": False,
    })
    assert status == 400 and rejected["tag"] == "Rejected", (status, rejected)
    unchanged = view_for(case_id, token)
    assert unchanged["firstSecret"]["tag"] == "Available", unchanged
    assert unchanged["erasureStatus"] == "not-requested", unchanged
    command = ["psql", "-h", os.environ.get("DB_HOST", "localhost"),
               "-p", os.environ.get("DB_PORT", "5432"),
               "-U", os.environ.get("DB_USER", "neohaskell"),
               "-d", database, "-At", "-c",
               f"SELECT count(*), count(*) FILTER (WHERE key_bytes IS NOT NULL) "
               f"FROM erasable_keys WHERE reference = '{reference}'"]
    assert subprocess.check_output(command, text=True).strip() == "1|1", reference
    print("A valid other user could neither read nor erase the owner's protected case")


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


def prepare(path, database, pause_after_commit=False, marker_path=None, selector_path=None):
    token = owner_token()
    status, result = request("POST", f"{API}/commands/create-owned-protected", token, [])
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    case_id = result["entityId"]
    view = wait_for(case_id, token, lambda item: bool(item.get("firstReference")))
    reference = view["firstReference"]
    assert view["firstSecret"]["tag"] == "Available", view
    assert view["secondSecret"]["tag"] == "Available", view
    expected = {"name": "Álvaro ñ 😀", "aliases": [None, "café"]}
    assert view["firstSecret"]["contents"] == expected, view
    assert view["secondSecret"]["contents"] == expected, view
    safe_id = str(uuid.UUID(case_id))
    command = ["psql", "-h", os.environ.get("DB_HOST", "localhost"),
               "-p", os.environ.get("DB_PORT", "5432"),
               "-U", os.environ.get("DB_USER", "neohaskell"),
               "-d", database, "-At", "-c",
               f"SELECT eventData::text FROM Events WHERE inlinedStreamId = '{safe_id}' "
               "AND entity = 'OwnedProtectedEntity' ORDER BY localPosition LIMIT 1"]
    event = json.loads(subprocess.check_output(command, text=True).strip())
    assert event["tag"] == "OwnedProtectedCreated", event
    first_reference = str(uuid.UUID(event["first"]["reference"]))
    second_reference = str(uuid.UUID(event["second"]["reference"]))
    assert first_reference == reference and first_reference != second_reference, event
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
    path.write_text(json.dumps({"caseId": case_id, "reference": reference,
                                "secondReference": second_reference,
                                "ownerId": view["ownerId"], "caseStatus": view["caseStatus"]}))
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
    assert completed["secondSecret"]["contents"] == {"name": "Álvaro ñ 😀", "aliases": [None, "café"]}, completed
    assert completed["wasAlreadyErased"] is True, completed
    persisted_case(database, state["caseId"], state["reference"])
    safe_reference = str(uuid.UUID(state["secondReference"]))
    command = ["psql", "-h", os.environ.get("DB_HOST", "localhost"),
               "-p", os.environ.get("DB_PORT", "5432"),
               "-U", os.environ.get("DB_USER", "neohaskell"),
               "-d", database, "-At", "-c",
               f"SELECT count(*), count(*) FILTER (WHERE key_bytes IS NOT NULL) "
               f"FROM erasable_keys WHERE reference = '{safe_reference}'"]
    assert subprocess.check_output(command, text=True).strip() == "1|1", safe_reference
    print("Equal protected values kept distinct references and only one was erased across restart")
    print("Reconstructed application completion and erased projection after second restart")


def prepare_erased_reconstruction(path):
    token = owner_token()
    state = json.loads(path.read_text())
    case_id = state["caseId"]
    completed = wait_for(case_id, token, lambda item: item["erasureStatus"] == "completed")
    derivative = completed["derivedSecret"]
    assert derivative == {"tag": "Available", "contents": {"name": "Derived Álvaro ñ 😀", "aliases": [None, "東京"]}}, completed
    status, result = request("POST", f"{API}/commands/map-erased-secret", token, {"caseId": case_id})
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    mapped = wait_for(case_id, token, lambda item: item.get("mappedErasedSecret") is not None)
    assert mapped["mappedErasedSecret"] == {"tag": "Erased", "contents": None}, mapped
    assert mapped["staleSecret"] == {"tag": "Erased", "contents": None}, mapped
    assert mapped["secondSecret"]["tag"] == "Available", mapped
    print("Mapping an erased copied reference completed without evaluating its transform")


def verify_erased_reconstruction(path, database):
    token = owner_token()
    state = json.loads(path.read_text())
    case_id = state["caseId"]
    rebuilt = wait_for(case_id, token, lambda item: item.get("mappedErasedSecret") is not None)
    assert rebuilt["ownerId"] == state["ownerId"] and rebuilt["caseStatus"] == "open", rebuilt
    assert rebuilt["firstReference"] == state["reference"], rebuilt
    assert rebuilt["firstSecret"] == {"tag": "Erased", "contents": None}, rebuilt
    assert rebuilt["staleSecret"] == {"tag": "Erased", "contents": None}, rebuilt
    assert rebuilt["mappedErasedSecret"] == {"tag": "Erased", "contents": None}, rebuilt
    assert rebuilt["secondSecret"] == {"tag": "Available", "contents": {"name": "Álvaro ñ 😀", "aliases": [None, "café"]}}, rebuilt
    assert rebuilt["derivedSecret"] == {"tag": "Available", "contents": {"name": "Derived Álvaro ñ 😀", "aliases": [None, "東京"]}}, rebuilt
    safe_id = str(uuid.UUID(case_id))
    command = ["psql", "-h", os.environ.get("DB_HOST", "localhost"),
               "-p", os.environ.get("DB_PORT", "5432"),
               "-U", os.environ.get("DB_USER", "neohaskell"),
               "-d", database, "-At", "-c",
               f"SELECT eventData::text FROM Events WHERE inlinedStreamId = '{safe_id}' "
               "AND entity = 'OwnedProtectedEntity' ORDER BY localPosition"]
    events = [json.loads(line) for line in subprocess.check_output(command, text=True).splitlines()]
    assert [event["tag"] for event in events] == ["OwnedProtectedCreated", "ErasureRequested", "ErasureRequested", "ErasureCompleted", "ErasedCopyRecorded"], events
    first, copied = events[0], events[-1]
    assert first["first"]["reference"] == state["reference"], first
    assert first["second"]["reference"] == state["secondReference"], first
    assert first["derivative"]["reference"] not in (state["reference"], state["secondReference"]), first
    assert copied["mappedErased"]["reference"] == state["reference"], copied
    assert_key_tombstoned(database, state["reference"])
    print("Fresh process rebuilt every persisted event, retained state, erased copies, and independent derivative")


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
    while time.monotonic() < deadline:
        log_text = log_path.read_text()
        assert "Álvaro" not in log_text and "\\u00c1lvaro" not in log_text, "Protected value appeared in application log"
        assert "$erasable" not in log_text, "Protected envelope appeared in application log"
        records = [json.loads(line) for line in log_text.splitlines()
                   if line.startswith("{") and line.endswith("}")]
        requests = [record for record in records
                    if record.get("kind") == "event_appended"
                    and record.get("commandName") == "RequestErasure"
                    and record.get("streamId") == case_id]
        drops = [record for record in records
                 if record.get("kind") == "integration_result"
                 and record.get("entityName") == "OwnedProtectedEntity"
                 and record.get("streamId") == case_id
                 and record.get("result") == "dropped"]
        if drops:
            assert len(requests) == 3 and len(drops) == 1, (requests, drops)
            assert drops[0]["eventId"] == requests[-1]["eventId"], (requests, drops)
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


def verify_queue_timeout(path, database):
    verify_pre_action(path, database, "A queue timeout left the key active, then explicit retry erased it")
    state = json.loads(path.read_text())
    persisted_case(database, state["caseId"], state["reference"])


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


def prepare_missing_callback(path, selector_path, log_path, database):
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
    safe_id = str(uuid.UUID(case_id))
    requested_event = subprocess.check_output(callback_db_command(database) + [
        f"SELECT eventid FROM events WHERE inlinedstreamid = '{safe_id}' "
        "AND eventdata->>'tag' = 'ErasureRequested' ORDER BY localposition DESC LIMIT 1"], text=True).strip()
    assert requested_event, case_id
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        log_text = log_path.read_text()
        assert "Álvaro" not in log_text and "\\u00c1lvaro" not in log_text, "Protected value appeared in application log"
        assert "$erasable" not in log_text, "Protected envelope appeared in application log"
        records = [json.loads(line) for line in log_text.splitlines()
                   if line.startswith("{") and line.endswith("}")]
        outcomes = [record for record in records
                    if record.get("kind") == "integration_result"
                    and record.get("eventId") == requested_event
                    and record.get("runnerKind") == "outbound"
                    and record.get("result") == "processed"]
        if outcomes:
            assert len(outcomes) == 1 and outcomes[0]["commandsEmitted"] == 0, outcomes
            break
        time.sleep(0.1)
    else:
        raise AssertionError(f"Missing result callback was not exercised for {case_id}")
    assert_key_tombstoned(database, reference)
    committed = subprocess.check_output(callback_db_command(database) + [
        f"SELECT count(*) FROM events WHERE inlinedstreamid = '{safe_id}' "
        "AND eventdata->>'tag' = 'ErasureCompleted'"], text=True).strip()
    assert committed == "0", (case_id, committed)
    pending = wait_for(case_id, token, lambda item: item["erasureStatus"] == "pending")
    assert_erased_wire(pending)
    assert pending["secondSecret"]["tag"] == "Available", pending
    path.write_text(json.dumps({"caseId": case_id, "reference": reference,
                                "ownerId": view["ownerId"], "caseStatus": view["caseStatus"]}))
    print("Omitted result callback left a tombstone and pending application request")


def verify_missing_callback(path, database):
    verify(path)
    state = json.loads(path.read_text())
    persisted_case(database, state["caseId"], state["reference"])


def callback_db_command(database):
    return ["psql", "-h", os.environ.get("DB_HOST", "localhost"),
            "-p", os.environ.get("DB_PORT", "5432"),
            "-U", os.environ.get("DB_USER", "neohaskell"),
            "-d", database, "-v", "ON_ERROR_STOP=1", "-At", "-c"]


def prepare_callback_death(path, database):
    token = owner_token()
    status, result = request("POST", f"{API}/commands/create-owned-protected", token, [])
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    case_id = result["entityId"]
    view = wait_for(case_id, token, lambda item: bool(item.get("firstReference")))
    reference = view["firstReference"]
    safe_id = str(uuid.UUID(case_id))
    command = callback_db_command(database)
    subprocess.check_call(command + [f"""
        CREATE FUNCTION nh140_pause_result_callback() RETURNS trigger LANGUAGE plpgsql AS $$
        BEGIN
            IF NEW.entity = 'OwnedProtectedEntity'
               AND NEW.inlinedstreamid = '{safe_id}'
               AND NEW.eventdata->>'tag' = 'ErasureCompleted' THEN
                PERFORM pg_sleep(120);
            END IF;
            RETURN NEW;
        END
        $$;
        CREATE TRIGGER nh140_pause_result_callback BEFORE INSERT ON events
        FOR EACH ROW EXECUTE FUNCTION nh140_pause_result_callback();
    """])
    status, result = request("POST", f"{API}/commands/request-erasure", token, {
        "tag": "RequestFirstErasure", "caseId": case_id,
        "reference": reference, "rejectResult": False,
        "simulateUnknownReference": False,
    })
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        active = subprocess.check_output(command + [
            "SELECT pid FROM pg_stat_activity WHERE datname = current_database() "
            "AND wait_event = 'PgSleep'"], text=True).splitlines()
        if len(active) == 1:
            backend_pid = int(active[0])
            break
        time.sleep(0.1)
    else:
        raise AssertionError(f"Result callback did not enter its database insert for {case_id}")
    pending = wait_for(case_id, token, lambda item: item["erasureStatus"] == "pending")
    assert_erased_wire(pending)
    assert_key_tombstoned(database, reference)
    committed = subprocess.check_output(command + [
        f"SELECT count(*) FROM events WHERE inlinedstreamid = '{safe_id}' "
        "AND eventdata->>'tag' = 'ErasureCompleted'"], text=True).strip()
    assert committed == "0", (case_id, committed)
    path.write_text(json.dumps({"caseId": case_id, "reference": reference,
                                "ownerId": view["ownerId"], "caseStatus": view["caseStatus"],
                                "backendPid": backend_pid}))
    print("Result callback entered its event insert before process death")


def clear_callback_death(path, database):
    state = json.loads(path.read_text())
    backend_pid = int(state["backendPid"])
    command = callback_db_command(database)
    stopped = subprocess.check_output(command + [
        f"SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE pid = {backend_pid} "
        "AND datname = current_database() AND wait_event = 'PgSleep'"], text=True).strip()
    assert stopped in ("", "t"), stopped
    subprocess.check_call(command + ["DROP TRIGGER nh140_pause_result_callback ON events; "
                                     "DROP FUNCTION nh140_pause_result_callback()"])
    safe_id = str(uuid.UUID(state["caseId"]))
    committed = subprocess.check_output(command + [
        f"SELECT count(*) FROM events WHERE inlinedstreamid = '{safe_id}' "
        "AND eventdata->>'tag' = 'ErasureCompleted'"], text=True).strip()
    assert committed == "0", (state["caseId"], committed)


def verify_callback_death(path, database):
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
        prepare(Path(state_path), sys.argv[3])
    elif action == "prepare-crash":
        prepare(Path(sys.argv[2]), sys.argv[5], True, Path(sys.argv[3]), Path(sys.argv[4]))
    elif action == "verify":
        state_path = sys.argv[2]
        verify(Path(state_path))
    elif action == "verify-completed":
        verify_completed(Path(sys.argv[2]), sys.argv[3])
    elif action == "prepare-erased-reconstruction":
        prepare_erased_reconstruction(Path(sys.argv[2]))
    elif action == "verify-erased-reconstruction":
        verify_erased_reconstruction(Path(sys.argv[2]), sys.argv[3])
    elif action == "prepare-pre-action":
        prepare_pre_action(Path(sys.argv[2]), Path(sys.argv[3]), Path(sys.argv[4]))
    elif action == "prepare-queue-timeout":
        prepare_queue_timeout(Path(sys.argv[2]), Path(sys.argv[3]), Path(sys.argv[4]), Path(sys.argv[5]), sys.argv[6])
    elif action == "verify-pre-action":
        verify_pre_action(Path(sys.argv[2]), sys.argv[3])
    elif action == "verify-queue-timeout":
        verify_queue_timeout(Path(sys.argv[2]), sys.argv[3])
    elif action == "prepare-callback-exception":
        prepare_callback_exception(Path(sys.argv[2]), Path(sys.argv[3]), Path(sys.argv[4]), sys.argv[5])
    elif action == "verify-callback-exception":
        verify_callback_exception(Path(sys.argv[2]), sys.argv[3])
    elif action == "prepare-missing-callback":
        prepare_missing_callback(Path(sys.argv[2]), Path(sys.argv[3]), Path(sys.argv[4]), sys.argv[5])
    elif action == "verify-missing-callback":
        verify_missing_callback(Path(sys.argv[2]), sys.argv[3])
    elif action == "prepare-callback-death":
        prepare_callback_death(Path(sys.argv[2]), sys.argv[3])
    elif action == "clear-callback-death":
        clear_callback_death(Path(sys.argv[2]), sys.argv[3])
    elif action == "verify-callback-death":
        verify_callback_death(Path(sys.argv[2]), sys.argv[3])
    elif action == "prepare-action-failure":
        prepare_action_failure(Path(sys.argv[2]), Path(sys.argv[3]), Path(sys.argv[4]), sys.argv[5])
    elif action == "verify-action-failure":
        verify_action_failure(Path(sys.argv[2]), sys.argv[3])
    elif action == "verify-callback-log":
        verify_callback_log(Path(sys.argv[2]), Path(sys.argv[3]))
    elif action == "verify-other-owner-denial":
        verify_other_owner_denial(sys.argv[2])
    else:
        raise SystemExit("unknown cryptoerasure restart assertion")
