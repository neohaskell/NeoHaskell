from __future__ import annotations

import json
import os
import subprocess
import sys
import uuid
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
def records_for(
    pattern: str,
    suite: str = "nhcore-test-service",
    log_level: str | None = None,
) -> list[dict]:
    command = [
        str(ROOT / ".agents/skills/tdd/scripts/test-match"),
        pattern,
        suite,
    ]
    environment = None
    if log_level is not None:
        environment = os.environ.copy()
        environment["LOG_LEVEL"] = log_level
    run = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=600, env=environment)
    output = run.stdout + run.stderr
    if run.returncode != 0:
        raise AssertionError(f"Pattern {pattern!r} failed. {output}")
    records = [json.loads(line) for line in output.splitlines() if line.startswith("{")]
    if any(value is None for record in records for value in record.values()):
        raise AssertionError(f"Unavailable field emitted as null in {pattern}")
    return records


def one(records: list[dict], kind: str) -> dict:
    matches = [record for record in records if record.get("kind") == kind]
    if len(matches) != 1:
        raise AssertionError(f"Expected one {kind}, got {len(matches)}")
    return matches[0]


def check_forced_cancelled_worker(
    records: list[dict],
    runner_kind: str,
) -> None:
    active = [
        record
        for record in records
        if record.get("runnerKind") == runner_kind
        and record.get("result") == "cancelled"
    ]
    if len(active) != 1:
        raise AssertionError(f"Expected one cancelled {runner_kind} event, got {len(active)}")
    active_result = active[0]
    if active_result.get("commandsEmitted") != 0:
        raise AssertionError(f"Cancelled {runner_kind} event emitted a command")
    if active_result.get("failureStage") != "cancellation" or active_result.get("errorClass") != "ExecutionInterrupted":
        raise AssertionError(f"Cancelled {runner_kind} event classification is wrong")

    stream_results = [
        record for record in records if record.get("streamId") == active_result.get("streamId")
    ]
    dropped = [record for record in stream_results if record.get("result") == "dropped"]
    if len(dropped) != 1 or len(stream_results) != 2:
        raise AssertionError(f"Queued {runner_kind} event was not dropped exactly once")
    dropped_result = dropped[0]
    if dropped_result.get("commandsEmitted") != 0:
        raise AssertionError(f"Dropped {runner_kind} event emitted a command")
    if "runnerKind" in dropped_result:
        raise AssertionError(f"Dropped {runner_kind} event ran an integration callback")
    if dropped_result.get("eventId") == active_result.get("eventId"):
        raise AssertionError(f"Queued {runner_kind} event reused the active event ID")
    uuid.UUID(active_result["eventId"])
    uuid.UUID(dropped_result["eventId"])


def check_pair(records: list[dict], actor: str | None, cause: bool, expected: int = 1) -> None:
    results = [record for record in records if record.get("kind") == "command_result"]
    events = [record for record in records if record.get("kind") == "event_appended"]
    if len(results) != expected or len(events) != expected:
        raise AssertionError(f"Expected {expected} command/event pairs, got {len(results)} and {len(events)}")
    for result in results:
        matching_events = [event for event in events if event.get("requestId") == result.get("requestId")]
        if len(matching_events) != 1:
            raise AssertionError("Saved event did not match exactly one command request")
        check_one_pair(result, matching_events[0], actor, cause)


def check_one_pair(result: dict, event: dict, actor: str | None, cause: bool) -> None:
    if result.get("result") != "accepted" or result.get("eventsAppended") != 1:
        raise AssertionError("Accepted command result or count is wrong")
    if type(result["eventsAppended"]) is not int:
        raise AssertionError("eventsAppended must be a JSON number")
    uuid.UUID(result["requestId"])
    uuid.UUID(event["eventId"])
    for key in ("requestId", "correlationId", "causationId", "relatedUserSub"):
        if result.get(key) != event.get(key):
            raise AssertionError(f"Command and event {key} differ")
    if "eventId" in result:
        raise AssertionError("Command result covers an event ID")
    if result.get("relatedUserSub") != actor:
        raise AssertionError("Actor attribution is wrong")
    if actor is None and ("relatedUserSub" in result or "relatedUserSub" in event):
        raise AssertionError("Anonymous command contains an actor")
    if cause != ("causationId" in result and "causationId" in event):
        raise AssertionError("Cause presence is wrong")
    if not cause and result["correlationId"] != result["requestId"]:
        raise AssertionError("Root correlation does not match its request ID")


def check_web_failure(records: list[dict], error_class: str, sentinels: tuple[str, ...]) -> None:
    result = one(records, "command_result")
    uuid.UUID(result["requestId"])
    if result.get("result") != "execution_failed" or result.get("eventsAppended") != 0:
        raise AssertionError("Recognized Web command failure outcome or count is wrong")
    if type(result["eventsAppended"]) is not int:
        raise AssertionError("Web command eventsAppended must be a JSON number")
    if result.get("failureStage") != "execution" or result.get("errorClass") != error_class:
        raise AssertionError("Recognized Web command failure classification is wrong")
    if result.get("correlationId") != result["requestId"]:
        raise AssertionError("Recognized Web command failure correlation is wrong")
    if any(key in result for key in ("eventId", "streamId", "causationId")):
        raise AssertionError("Recognized Web command failure leaked unrelated metadata")
    if [record for record in records if record.get("kind") == "event_appended"]:
        raise AssertionError("Failed Web command reported a saved event")
    if any(sentinel in json.dumps(records) for sentinel in sentinels):
        raise AssertionError("Web command failure details leaked into JSON logs")


def main() -> int:
    try:
        check_pair(records_for("executes command that creates new stream successfully"), None, False, 2)
        check_pair(records_for("records the verified actor and server request correlation"), "metadata-test-user", False)
        check_pair(records_for("persists source attribution through an internal command dispatch"), "source-actor", True)

        decision = records_for("does not log rejected decision reasons", log_level="debug")
        if "private-domain-reason-sentinel" in json.dumps(decision):
            raise AssertionError("Rejected decision text leaked into a debug record")
        if not any(record.get("message") == "Decision: rejected" for record in decision):
            raise AssertionError("Debug decision record was not emitted")

        unauthorized = records_for("does not treat source actor attribution as authenticated claims")
        result = one(unauthorized, "command_result")
        if result.get("result") != "unauthorized" or result.get("eventsAppended") != 0:
            raise AssertionError("Unauthorized outcome or count is wrong")
        if [record for record in unauthorized if record.get("kind") == "event_appended"]:
            raise AssertionError("Unauthorized command reported a saved event")

        append_failure = records_for("does not report an event when append fails")
        failed_result = one(append_failure, "command_result")
        if failed_result.get("result") != "append_failed" or failed_result.get("eventsAppended") != 0:
            raise AssertionError("Append failure outcome or count is wrong")
        if failed_result.get("failureStage") != "append" or failed_result.get("errorClass") != "InsertionFailed":
            raise AssertionError("Append failure classification is wrong")
        if [record for record in append_failure if record.get("kind") == "event_appended"]:
            raise AssertionError("Failed append reported a saved event")
        if "private-store-error-sentinel" in json.dumps(append_failure):
            raise AssertionError("Store error detail leaked into JSON logs")

        io_failure = records_for("reports synchronous IO failures without scoped metadata or exception text")
        io_failure_result = one(io_failure, "command_result")
        if io_failure_result.get("result") != "execution_failed" or io_failure_result.get("eventsAppended") != 0:
            raise AssertionError("Synchronous IO failure outcome or count is wrong")
        if io_failure_result.get("failureStage") != "execution" or io_failure_result.get("errorClass") != "SynchronousException":
            raise AssertionError("Synchronous IO failure classification is wrong")
        if "eventId" in io_failure_result or "streamId" in io_failure_result:
            raise AssertionError("Ambient event or stream identity leaked into an early failure")
        if any(value in json.dumps(io_failure) for value in ("private-io-exception-sentinel", "ambient-event-id-sentinel", "ambient-stream-id-sentinel")):
            raise AssertionError("Private IO or ambient scope text leaked into JSON logs")

        cancelled = records_for("reports an interrupted command attempt when cancelled")
        cancelled_result = one(cancelled, "command_result")
        if cancelled_result.get("result") != "execution_failed" or cancelled_result.get("eventsAppended") != 0:
            raise AssertionError("Cancelled command outcome or count is wrong")
        if cancelled_result.get("failureStage") != "execution" or cancelled_result.get("errorClass") != "ExecutionInterrupted":
            raise AssertionError("Cancelled command classification is wrong")

        body_read_failure = records_for("returns a generic 500 when requestBody throws")
        body_read_failure_result = one(body_read_failure, "command_result")
        if body_read_failure_result.get("result") != "execution_failed" or body_read_failure_result.get("eventsAppended") != 0:
            raise AssertionError("Request body read failure outcome or count is wrong")
        if body_read_failure_result.get("failureStage") != "request_body" or body_read_failure_result.get("errorClass") != "SynchronousException":
            raise AssertionError("Request body read failure classification is wrong")
        if "private-body-read-details" in json.dumps(body_read_failure):
            raise AssertionError("Request body exception text leaked into JSON logs")

        body_read_cancelled = records_for("preserves cancellation while requestBody is blocked")
        body_read_cancelled_result = one(body_read_cancelled, "command_result")
        if body_read_cancelled_result.get("result") != "execution_failed" or body_read_cancelled_result.get("eventsAppended") != 0:
            raise AssertionError("Cancelled request body outcome or count is wrong")
        if body_read_cancelled_result.get("failureStage") != "request_body" or body_read_cancelled_result.get("errorClass") != "ExecutionInterrupted":
            raise AssertionError("Cancelled request body classification is wrong")

        decoder_failure = records_for("reports one safe result when a recognized command decoder throws")
        check_web_failure(decoder_failure, "SynchronousException", ("private-command-decoder-sentinel",))

        decoder_cancelled = records_for("reports one safe result when a recognized command decoder is cancelled")
        check_web_failure(decoder_cancelled, "ExecutionInterrupted", ("ThreadKilled",))

        web_execution_failure = records_for("keeps one result when a recognized Web command fails during execution")
        check_web_failure(web_execution_failure, "SynchronousException", ("private-web-fetch-sentinel",))

        retried = records_for("records the exact insertion precondition after refetch")
        retry_records = [record for record in retried if record.get("kind") == "retry"]
        retry_results = [record for record in retried if record.get("kind") == "command_result"]
        retry_events = [record for record in retried if record.get("kind") == "event_appended"]
        if len(retry_records) != 2 or len(retry_results) != 2 or len(retry_events) != 2:
            raise AssertionError("Conflict did not produce one retry, result, and saved event per backend")
        for retry in retry_records:
            if type(retry.get("attempt")) is not int or retry["attempt"] != 1:
                raise AssertionError("Retry attempt is not numeric and one-based")
            matching = [record for record in retry_results if record["requestId"] == retry["requestId"]]
            if len(matching) != 1 or matching[0]["correlationId"] != retry["correlationId"]:
                raise AssertionError("Retry lost its command chain")

        exhausted = records_for("stops after bounded append conflicts without saving a candidate")
        exhausted_result = one(exhausted, "command_result")
        exhausted_retries = [record for record in exhausted if record.get("kind") == "retry"]
        if exhausted_result.get("result") != "retries_exhausted" or exhausted_result.get("eventsAppended") != 0:
            raise AssertionError("Retry exhaustion outcome or count is wrong")
        if [record["attempt"] for record in exhausted_retries] != list(range(1, 12)):
            raise AssertionError("Retry attempts are not numeric and sequential")
        if any(record["requestId"] != exhausted_result["requestId"] for record in exhausted_retries):
            raise AssertionError("Retry exhaustion changed its request ID")
        if [record for record in exhausted if record.get("kind") == "event_appended"]:
            raise AssertionError("Discarded retry candidate was reported as saved")

        nil_request = records_for("allocates a distinct correlation for each nil request ID")
        results = [record for record in nil_request if record.get("kind") == "command_result"]
        events = [record for record in nil_request if record.get("kind") == "event_appended"]
        if len(results) != 2 or len(events) != 2:
            raise AssertionError("Two commands did not report two results and two saved events")
        if len({record["requestId"] for record in results}) != 2:
            raise AssertionError("Nil request IDs reused one generated ID")
        if any(record["requestId"] != record["correlationId"] for record in results):
            raise AssertionError("A nil request did not use its generated correlation")

        auth_failure = records_for("returns 401 without exposing the invalid token")
        auth_result = one(auth_failure, "command_result")
        uuid.UUID(auth_result["requestId"])
        if auth_result.get("result") != "unauthorized" or auth_result.get("correlationId") != auth_result["requestId"]:
            raise AssertionError("Authentication failure correlation does not match its request")
        if auth_result.get("failureStage") != "authentication" or auth_result.get("eventsAppended") != 0:
            raise AssertionError("Authentication failure outcome or count is wrong")

        auth_cancelled = records_for("preserves cancellation during JWKS refresh and reports one command result")
        auth_cancelled_result = one(auth_cancelled, "command_result")
        uuid.UUID(auth_cancelled_result["requestId"])
        if auth_cancelled_result.get("result") != "execution_failed" or auth_cancelled_result.get("eventsAppended") != 0:
            raise AssertionError("Canceled authentication outcome or count is wrong")
        if auth_cancelled_result.get("failureStage") != "authentication" or auth_cancelled_result.get("errorClass") != "ExecutionInterrupted":
            raise AssertionError("Canceled authentication classification is wrong")
        if auth_cancelled_result.get("correlationId") != auth_cancelled_result["requestId"]:
            raise AssertionError("Canceled authentication correlation does not match its request")
        if "relatedUserSub" in auth_cancelled_result or "causationId" in auth_cancelled_result:
            raise AssertionError("Canceled authentication reported actor or cause metadata")
        if [record for record in auth_cancelled if record.get("kind") == "event_appended"]:
            raise AssertionError("Canceled authentication reported a saved event")
        if any(value in json.dumps(auth_cancelled) for value in ("missing-key-for-refresh", "ThreadKilled")):
            raise AssertionError("Authentication request detail or raw cancellation text leaked into logs")

        processed = one(records_for("creates a dispatcher and processes a single event", "nhcore-test-integration"), "integration_result")
        if processed.get("result") != "processed" or processed.get("commandsEmitted") != 0:
            raise AssertionError("Zero-command integration outcome is wrong")
        if type(processed["commandsEmitted"]) is not int:
            raise AssertionError("commandsEmitted must be a JSON number")
        if processed.get("runnerKind") != "outbound" or processed.get("runnerOrdinal") != 0:
            raise AssertionError("Integration runner identity is wrong")
        uuid.UUID(processed["eventId"])

        legacy_dispatch = one(records_for("keeps the legacy Map Text EndpointHandler dispatcher and dispatch APIs", "nhcore-test-integration"), "integration_result")
        if legacy_dispatch.get("result") != "processed" or legacy_dispatch.get("commandsEmitted") != 1:
            raise AssertionError("Legacy dispatcher or dispatch wrapper did not process its command")

        skipped = one(records_for("skips events with no matching runner", "nhcore-test-integration"), "integration_result")
        if skipped.get("result") != "skipped" or skipped.get("commandsEmitted") != 0:
            raise AssertionError("Unmatched integration outcome is wrong")
        if "runnerOrdinal" in skipped or "runnerKind" in skipped:
            raise AssertionError("Unmatched integration reported a runner")

        recovered = records_for("catches IO exceptions at per-runner level and continues processing", "nhcore-test-integration")
        recovered_results = [record for record in recovered if record.get("kind") == "integration_result"]
        if [record["result"] for record in recovered_results] != ["failed", "processed"]:
            raise AssertionError("Sequential worker failure and recovery outcomes are wrong")
        if recovered_results[0].get("failureStage") != "runner_processing" or recovered_results[0].get("errorClass") != "RunnerProcessingFailure":
            raise AssertionError("Runner processing failure classification is wrong")
        if len({record["eventId"] for record in recovered_results}) != 2:
            raise AssertionError("Sequential worker reused a source event ID")
        if any(record.get("commandsEmitted") != 0 for record in recovered_results):
            raise AssertionError("Failure or zero-command recovery emitted a command")
        if any("relatedUserSub" in record for record in recovered_results):
            raise AssertionError("Sequential worker leaked an actor")
        if "private-runner-error-sentinel" in json.dumps(recovered):
            raise AssertionError("Runner processing error text leaked into JSON logs")

        timed_out = records_for("reports a timed out event runner without emitting a command", "nhcore-test-integration")
        timed_out_results = [record for record in timed_out if record.get("kind") == "integration_result"]
        if len(timed_out_results) != 2 or [record.get("runnerOrdinal") for record in timed_out_results] != [0, 1]:
            raise AssertionError("Timeout did not report every matching runner exactly once")
        timed_out_runner, unstarted_runner = timed_out_results
        if timed_out_runner.get("result") != "timed_out" or timed_out_runner.get("commandsEmitted") != 0:
            raise AssertionError("Timed out runner outcome is wrong")
        if timed_out_runner.get("failureStage") != "timeout" or timed_out_runner.get("errorClass") != "Timeout":
            raise AssertionError("Timeout classification is wrong")
        if unstarted_runner.get("result") != "cancelled" or unstarted_runner.get("commandsEmitted") != 0:
            raise AssertionError("Runner after a timeout was not classified as unstarted")
        if unstarted_runner.get("failureStage") != "cancellation" or unstarted_runner.get("errorClass") != "RunnerNotStarted":
            raise AssertionError("Unstarted runner classification after timeout is wrong")

        dispatch_failure = records_for("reports every matching runner when callback dispatch fails", "nhcore-test-integration")
        dispatch_failure_results = [record for record in dispatch_failure if record.get("kind") == "integration_result"]
        if len(dispatch_failure_results) != 2 or [record.get("runnerOrdinal") for record in dispatch_failure_results] != [0, 1]:
            raise AssertionError("Callback dispatch failure did not report every matching runner exactly once")
        failed_dispatch, unstarted_runner = dispatch_failure_results
        if failed_dispatch.get("result") != "failed" or failed_dispatch.get("failureStage") != "callback_dispatch" or failed_dispatch.get("errorClass") != "CallbackDispatchFailure":
            raise AssertionError("Callback dispatch failure classification is wrong")
        if unstarted_runner.get("result") != "cancelled" or unstarted_runner.get("failureStage") != "cancellation" or unstarted_runner.get("errorClass") != "RunnerNotStarted":
            raise AssertionError("Runner skipped after callback failure classification is wrong")
        if "private-dispatch-error-sentinel" in json.dumps(dispatch_failure):
            raise AssertionError("Callback dispatch error text leaked into JSON logs")

        forced_cancel = records_for(
            "force-cancels workers when Stop message cannot be delivered and reports queued events",
            "nhcore-test-integration",
        )
        forced_cancel_results = [
            record for record in forced_cancel if record.get("kind") == "integration_result"
        ]
        if len(forced_cancel_results) != 4:
            raise AssertionError(f"Expected four forced-cancellation results, got {len(forced_cancel_results)}")
        check_forced_cancelled_worker(forced_cancel_results, "outbound")
        check_forced_cancelled_worker(forced_cancel_results, "lifecycle")

        interrupted_stop = records_for(
            "completes an interrupted stop after admitted writers leave and reports queued events",
            "nhcore-test-integration",
        )
        interrupted_stop_results = [
            record for record in interrupted_stop if record.get("kind") == "integration_result"
        ]
        if len(interrupted_stop_results) != 4:
            raise AssertionError(f"Expected four interrupted-stop results, got {len(interrupted_stop_results)}")
        outbound_interrupted_stop = [
            record
            for record in interrupted_stop_results
            if record.get("streamId") == "outbound-interrupted-stop"
        ]
        lifecycle_interrupted_stop = [
            record
            for record in interrupted_stop_results
            if record.get("streamId") == "lifecycle-interrupted-stop"
        ]
        check_forced_cancelled_worker(outbound_interrupted_stop, "outbound")
        check_forced_cancelled_worker(lifecycle_interrupted_stop, "lifecycle")

        creation_race = records_for(
            "drops events when lifecycle worker creation races with shutdown",
            "nhcore-test-integration",
        )
        creation_race_results = [
            record for record in creation_race if record.get("kind") == "integration_result"
        ]
        if len(creation_race_results) != 1:
            raise AssertionError(f"Expected one shutdown-race result, got {len(creation_race_results)}")
        creation_race_drop = creation_race_results[0]
        if creation_race_drop.get("result") != "dropped" or creation_race_drop.get("commandsEmitted") != 0:
            raise AssertionError("Worker created after shutdown was not reported as dropped")
        if "runnerKind" in creation_race_drop:
            raise AssertionError("Worker created after shutdown ran an integration callback")
        uuid.UUID(creation_race_drop["eventId"])

        overloaded = records_for("drops events and continues when channel is full and timeout expires", "nhcore-test-integration")
        overloaded_results = [record for record in overloaded if record.get("kind") == "integration_result"]
        if [record["result"] for record in overloaded_results].count("dropped") != 1:
            raise AssertionError("A dropped event was not reported exactly once")
        if len({record["eventId"] for record in overloaded_results}) != len(overloaded_results):
            raise AssertionError("Dropped and processed event records overlap")

        integration_retry = records_for("retries on transient IO exception and succeeds", "nhcore-test-integration")
        retry = one(integration_retry, "retry")
        recovery = one(integration_retry, "integration_result")
        if type(retry.get("attempt")) is not int or retry["attempt"] != 1:
            raise AssertionError("Integration retry attempt is wrong")
        if retry["eventId"] != recovery["eventId"] or retry["correlationId"] != recovery["correlationId"]:
            raise AssertionError("Integration retry lost its source chain")
        if recovery.get("result") != "processed":
            raise AssertionError("Integration retry did not finish successfully")

        outbound_command = records_for("persists a command emitted by an outbound runner with its source event metadata", "nhcore-test-integration")
        source_result = one(outbound_command, "integration_result")
        if source_result.get("result") != "processed" or source_result.get("commandsEmitted") != 1:
            raise AssertionError("Outbound runner did not emit its command")
        if type(source_result["commandsEmitted"]) is not int:
            raise AssertionError("commandsEmitted must be a JSON number")
        if source_result.get("runnerKind") != "outbound" or source_result.get("runnerOrdinal") != 0:
            raise AssertionError("Source-aware outbound runner identity is wrong")
        uuid.UUID(source_result["eventId"])
        if source_result.get("relatedUserSub") != "source-actor" or source_result.get("correlationId") != "root-chain":
            raise AssertionError("Outbound integration result lost source actor or root correlation")
        if "causationId" in source_result or "failureStage" in source_result or "errorClass" in source_result:
            raise AssertionError("Successful outbound integration result has unexpected failure or cause fields")
        check_pair(outbound_command, "source-actor", True)
        outbound_command_result = one(outbound_command, "command_result")
        if outbound_command_result.get("correlationId") != "root-chain":
            raise AssertionError("Outbound command result lost its source correlation")
        if outbound_command_result.get("causationId") != source_result["eventId"]:
            raise AssertionError("Outbound command result lost its source cause")
        if outbound_command_result.get("relatedUserSub") != "source-actor":
            raise AssertionError("Outbound command result lost its source actor")
        appended_outbound_event = one(outbound_command, "event_appended")
        if any("failureStage" in record or "errorClass" in record for record in (outbound_command_result, appended_outbound_event)):
            raise AssertionError("Successful outbound command result has failure fields")
        if appended_outbound_event.get("eventId") == source_result["eventId"]:
            raise AssertionError("Outbound command did not persist a child event")
        if appended_outbound_event.get("correlationId") != "root-chain":
            raise AssertionError("Saved outbound child event lost its root correlation")
        if appended_outbound_event.get("causationId") != source_result["eventId"]:
            raise AssertionError("Saved outbound child event lost its immediate cause")
        if appended_outbound_event.get("relatedUserSub") != "source-actor":
            raise AssertionError("Saved outbound child event lost its source actor")

        autonomous_commands = records_for("allocates distinct request correlations for repeated autonomous command dispatches", "nhcore-test-integration")
        check_pair(autonomous_commands, None, False, 2)
        autonomous_results = [record for record in autonomous_commands if record.get("kind") == "command_result"]
        autonomous_events = [record for record in autonomous_commands if record.get("kind") == "event_appended"]
        if len({record["requestId"] for record in autonomous_results}) != 2:
            raise AssertionError("Repeated autonomous dispatches reused a request ID")
        if len({record["correlationId"] for record in autonomous_results}) != 2:
            raise AssertionError("Repeated autonomous dispatches reused a correlation ID")
        if any("failureStage" in record or "errorClass" in record for record in autonomous_results + autonomous_events):
            raise AssertionError("Successful autonomous dispatch emitted failure fields")
    except (AssertionError, KeyError, ValueError) as error:
        sys.stderr.write(f"Event log JSON verification failed. {error}\n")
        return 1
    print("Command, saved-event, retry, and integration JSON records passed attribution, count, failure, and omission checks")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
