---
title: Call external HTTP APIs
description: Connect external APIs without confusing a request with a completed operation.
sidebar:
  order: 2
---

Your application sends a request to another service, but the connection closes before the reply arrives. Did the service reject the work, or did the reply disappear after the work completed? An HTTP integration must handle that uncertainty as well as ordinary success and failure.

NeoHaskell provides the request machinery; your application interprets the provider’s response and decides which operations can safely be repeated. We will start with a status lookup, then use a simulated payment in the ecommerce project to practise a more consequential handoff.

Prerequisites: [integration lifecycle](/connect/) and [configuration](/build/configuration/).

## Place the provider call in your project

Continue from your `mug-shop` directory and complete the
[integration setup](/connect/#prepare-your-project). Keep the protocol helper
in `src/Shop/Integrations/ProviderStatus.hs`. The cart-side handler that selects
when to call it belongs under `src/Shop/Cart/Integrations/`, following the complete
[handler module](/connect/workflows/#add-the-handler-to-your-project).

Before adding the request, define the command that records a status result and
register it in the relevant service with `InternalTransport`. Give it successful, refused, and unresolved
outcomes with a stable operation identifier. The following builder is the
request portion of that feature; the callbacks bind the result to your
application’s command.

## Learn the request shape with a read

Start with an operation that reads provider status. In this **partial integration builder**, `statusUrl`, `recordReply`, and `recordFailure` are values supplied by your application. Both callbacks return one registered command type.

```haskell
-- Inside the event handler's Integration.batch:
Integration.outbound Http.Request
  { method = Http.GET
  , url = statusUrl
  , headers = []
  , body = Http.noBody
  , onSuccess = recordReply
  , onError = Just recordFailure
  , auth = Http.Bearer "${SHOP_PROVIDER_TOKEN}"
  , retry = Http.defaultRetry
  , timeoutSeconds = 15
  }
```

The URL and header values support environment substitution. Authentication supports `NoAuth`, `Bearer`, `Basic`, and named-header `ApiKey`. Missing environment variables raise an integration authentication error during preparation; they do not necessarily reach `onError`.

`Http.Response` carries `statusCode`, a JSON `body`, and response `headers`. **Inspect the status in your callback.** The callback named `onSuccess` is the response path; it is not a declaration that the provider approved the business operation.

For request bodies, use `Http.json`, `Http.form`, `Http.raw`, or `Http.noBody`. The current adapter supports JSON for POST, PUT, and PATCH; form and raw bodies are implemented for POST. GET and DELETE do not use the supplied body. Responses are decoded through the JSON client, so a provider returning empty or non-JSON content needs explicit compatibility testing or a custom adapter.

## Move from HTTP to payment meaning

Use a test provider to model a payment in the practice project:

1. Record an application payment attempt with a stable identifier and the order amount/currency.
2. Build the provider request from trusted application state.
3. Use the provider's documented idempotency mechanism if it supports one. This requires provider-specific work.
4. Decode and validate its response, retaining its operation identifier.
5. Record confirmed, refused, or unresolved outcomes through commands.
6. Reconcile unresolved attempts by asking the provider for their actual status.

These are design steps, not a supplied payment adapter. Choose and verify a provider's current API separately. A customer returning to a success page is not, by itself, evidence of payment confirmation.

For callbacks from the provider, validate authenticity before translating incoming data into a command. The generic inbound worker abstraction does not supply a payment-provider signature verifier or webhook route for you.

## Understand the current retry boundary

The source's `Retry` record documents `maxAttempts` as including the first attempt. The current executor compares `attempt <= maxAttempts` before retrying, which can allow one additional attempt. Its `noRetry` preset therefore must not be treated as a guarantee that a failing request is sent only once.

The executor also retries request errors separately from its status-code list. Do not infer that only the listed statuses can lead to another request. These implementation limitations matter for charges, paid AI calls, and label purchases; test actual request counts with a controlled endpoint before approving those operations.

Timeouts also do not prove that the remote side did nothing. Preserve an unresolved outcome until you have evidence.

## Run a controlled status check

Run `neo build` from `mug-shop` after adding the helper and callback command.
Exercise the status mapping in your `tests/` suite with `neo test`, including a
valid JSON error response. Start your application with `neo run`, trigger the
request through its command, and inspect the resulting status query. Use a
controlled endpoint before connecting payment credentials.

## Exercise: a lost payment reply

Describe the practice application’s state after a timeout, what its screen shows, and how you would resolve the uncertainty. Then consider which parts also apply to creating a calendar entry or submitting a document for processing.

<details>
<summary>Suggested checks</summary>

Use a test provider that accepts an operation and then drops the connection. Check that a repeated request cannot double-charge under your chosen provider contract. Check refusal, malformed JSON, a valid JSON error status, an unknown operation ID, and a later reconciliation result. Record real-provider sandbox verification separately from unit tests of response mapping.

</details>

For a smaller provider integration, continue with [email](/connect/email/).

<details>
<summary>Framework source notes</summary>

- [integrations/Integration/Http/Request.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Http/Request.hs)
- [integrations/Integration/Http/Response.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Http/Response.hs)
- [integrations/Integration/Http/Internal.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Http/Internal.hs)
- [integrations/Integration/Http/Retry.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Http/Retry.hs)
- [integrations/test/Integration/Http/InternalSpec.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/test/Integration/Http/InternalSpec.hs)
- [core/service/Integration.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration.hs)

</details>
