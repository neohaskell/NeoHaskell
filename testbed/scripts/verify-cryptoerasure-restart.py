#!/usr/bin/env python3
"""Assert that an application retries a committed erasure after its process restarts."""

import json
import sys
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from urllib.error import HTTPError
from urllib.request import Request, urlopen


API = "http://127.0.0.1:8081"
FIXTURE = "http://127.0.0.1:8181"


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


def concurrent_case(token):
    status, result = request("POST", f"{API}/commands/create-owned-protected", token, [])
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    case_id = result["entityId"]
    loaded = wait_for(case_id, token, lambda item: bool(item.get("firstReference")))
    reference = loaded["firstReference"]
    assert loaded["firstSecret"]["tag"] == "Available", loaded
    status, result = request("POST", f"{API}/commands/request-erasure", token, {
        "tag": "RequestFirstErasure", "caseId": case_id,
        "reference": reference, "rejectResult": False,
        "simulateUnknownReference": False,
    })
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    completed = wait_for(case_id, token, lambda item: item["erasureStatus"] == "completed")
    assert completed["firstSecret"]["tag"] == "Erased", completed
    assert completed["secondSecret"]["tag"] == "Available", completed
    assert completed["wasAlreadyErased"] is False, completed
    return case_id, reference, loaded["ownerId"]


def concurrent():
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
        assert completed["secondSecret"]["tag"] == "Available", completed
    print("Four concurrent application requests and explicit retries kept independent tombstones")


def prepare(path):
    token = owner_token()
    status, result = request("POST", f"{API}/commands/create-owned-protected", token, [])
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    case_id = result["entityId"]
    view = wait_for(case_id, token, lambda item: bool(item.get("firstReference")))
    reference = view["firstReference"]
    assert view["firstSecret"]["tag"] == "Available", view
    assert view["secondSecret"]["tag"] == "Available", view
    status, result = request("POST", f"{API}/commands/request-erasure", token, {
        "tag": "RequestFirstErasure", "caseId": case_id,
        "reference": reference, "rejectResult": True,
        "simulateUnknownReference": False,
    })
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    wait_for(case_id, token, lambda item: item["firstSecret"]["tag"] == "Erased" and item["erasureStatus"] == "pending")
    path.write_text(json.dumps({"caseId": case_id, "reference": reference, "ownerId": view["ownerId"]}))
    print("Committed key deletion left the application request pending before restart")


def verify(path):
    token = owner_token()
    state = json.loads(path.read_text())
    case_id = state["caseId"]
    view = wait_for(case_id, token, lambda item: True)
    assert view["firstReference"] == state["reference"], view
    assert view["ownerId"] == state["ownerId"], view
    assert view["firstSecret"]["tag"] == "Erased", view
    assert view["secondSecret"]["tag"] == "Available", view
    assert view["erasureStatus"] == "pending", view
    status, result = request("POST", f"{API}/commands/request-erasure", token, {
        "tag": "RetryFirstErasure", "caseId": case_id,
    })
    assert status == 200 and result["tag"] == "Accepted", (status, result)
    completed = wait_for(case_id, token, lambda item: item["erasureStatus"] == "completed")
    assert completed["wasAlreadyErased"] is True, completed
    assert completed["firstSecret"]["tag"] == "Erased", completed
    assert completed["secondSecret"]["tag"] == "Available", completed
    print("Explicit retry completed after restart with AlreadyErased")


if __name__ == "__main__":
    action = sys.argv[1]
    if action == "concurrent":
        concurrent()
    elif action == "prepare":
        state_path = sys.argv[2]
        prepare(Path(state_path))
    elif action == "verify":
        state_path = sys.argv[2]
        verify(Path(state_path))
    else:
        raise SystemExit("expected prepare or verify")
