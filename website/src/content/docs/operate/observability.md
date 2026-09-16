---
title: Observe a running application
description: Connect process health, projection progress, and business outcomes without exposing private data.
sidebar:
  order: 3
---

A user reports that an accepted change is not visible. You need to distinguish a rejected request, a delayed view, a wrong account, and an external failure. “The server is up” answers only one small part of that question. In the ecommerce practice project, the report might be “I added an item, but my cart is unchanged.”

Observe the application in layers: process, stored facts, views, and external outcomes. Give each alert a question a person can act on.

## Begin with the two built-in signals

For an application using the default web wiring on port 8080:

```sh
curl -i http://127.0.0.1:8080/health
curl -i http://127.0.0.1:8080/ready
```

The readiness response is an aggregate state:

```json
{"status":"ready"}
```

During catch-up it is `{"status":"rebuilding"}` with HTTP `503`. Failure also returns `503`, with `status` set to `failed` and a `reason`. The current HTTP response is not a dashboard of per-query lag values.

A healthy process may be rebuilding queries. A ready process may still encounter an external-provider outage. Monitor the business operation separately.

## Read application logs

The `Log` module writes JSON records to standard output with `time`, `level`, and `message`, plus call-site information and framework scope fields when available. Your host must collect, retain, and make those records searchable.

For a local diagnostic run:

```sh
LOG_LEVEL=debug neo run
```

The default level is `Info`; the implementation recognises debug, info, warn, error, and critical spellings in lower, title, or upper case. Changing the environment requires a new process. `neo --verbose` controls CLI verbosity; `LOG_LEVEL` controls application logging.

Framework logs identify query replay progress and failures. Progress messages include `events_replayed`, `lag_from_head`, and `duration_seconds` within the message text. Do not assume those are separately exported metrics. The cold-start test verifies the progress messages and checks failure logs for query identity and position without event payload leakage.

[Inspect the logging implementation](https://github.com/neohaskell/NeoHaskell/blob/main/core/core/Log.hs) and [the cold-start verification](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/scripts/cold-start-readiness.sh).

## Follow one operation through the system

Record the revision, approximate time, and a safe identifier for the operation or entity. Keep the cart identifier returned by your `mug-shop` request. Then ask:

1. Did the command succeed or report a rejection?
2. Does the persisted history contain the expected fact?
3. Has the relevant query caught up, and is the reader authorised to see it?
4. If an integration was expected, what result did it report?
5. Did the result become the next business fact, or is follow-up still pending?

Use the [IDE graph](/getting-started/visual-ide/) to find the responsible command, event, query, and integration. It explains relationships in the source; it does not show the live database history or replace production monitoring.

Choose safe diagnostic identifiers. Personal addresses, access tokens, uploaded documents, and full provider responses generally do not belong in routine logs. Redaction of a typed configuration field does not redact arbitrary text you later log.

## Exercise: a green process with a pending outcome

After you add an external integration to `mug-shop`, design a staging scenario where its triggering change is accepted but the external confirmation is delayed. Describe the customer-visible status and the operator-visible signal before running it.

<details>
<summary>Suggested reasoning</summary>

Health can remain green while the confirmation is pending. The relevant view should communicate that intermediate state honestly. An operator needs the pending duration and a safe correlation identifier. Repeatedly restarting a healthy process is unlikely to resolve a provider outage and may complicate diagnosis.

</details>

Use [recovery](/operate/recovery/) when diagnosis identifies interrupted work or lost infrastructure.
