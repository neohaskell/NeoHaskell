from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
PATTERN = "executes command that creates new stream successfully"


def main() -> int:
    command = [
        str(ROOT / ".agents/skills/tdd/scripts/test-match"),
        PATTERN,
        "nhcore-test-service",
    ]
    run = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=600)
    output = run.stdout + run.stderr
    records = [json.loads(line) for line in output.splitlines() if line.startswith("{")]
    if run.returncode != 0:
        sys.stderr.write(output)
        return run.returncode

    results = [record for record in records if record.get("kind") == "command_result"]
    appended = [record for record in records if record.get("kind") == "event_appended"]
    if len(results) != 1 or len(appended) != 1:
        sys.stderr.write(
            f"Expected one command_result and one event_appended, got {len(results)} and {len(appended)}.\n"
        )
        return 1

    result = results[0]
    event = appended[0]
    required = ("requestId", "correlationId", "relatedUserSub")
    if any(result.get(key) != event.get(key) for key in required):
        sys.stderr.write("Command and saved event tracing fields differ.\n")
        return 1
    if result.get("result") != "accepted" or result.get("eventsAppended") != 1:
        sys.stderr.write("Command result or event count is wrong.\n")
        return 1
    if type(result["eventsAppended"]) is not int:
        sys.stderr.write("eventsAppended must be a JSON number.\n")
        return 1
    if "relatedUserSub" in result or "relatedUserSub" in event:
        sys.stderr.write("Anonymous command contains an actor.\n")
        return 1
    if "eventId" in result or "causationId" in result or "causationId" in event:
        sys.stderr.write("Root records contain an unavailable field.\n")
        return 1
    if not event.get("eventId"):
        sys.stderr.write("Saved event identity is missing.\n")
        return 1
    if any(value is None for record in (result, event) for value in record.values()):
        sys.stderr.write("An unavailable field was emitted as JSON null.\n")
        return 1
    print("1 command_result and 1 event_appended with matching anonymous correlation")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
