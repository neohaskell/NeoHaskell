---
title: Call providers and model payments
description: Connect external APIs without confusing a request with a completed business operation.
sidebar:
  order: 2
---

The shop asks a payment provider to collect money. The connection closes before the reply arrives. Did the payment fail, or did the reply disappear after the provider charged the customer?

This is why a payment needs more than an HTTP request. NeoHaskell provides a general HTTP integration; your application supplies the provider contract, payment state, reconciliation, and duplicate protection. The same reasoning applies to purchasing shipping labels.

Prerequisites: [integration lifecycle](/connect/) and [configuration](/build/configuration/).

## Learn the request shape with a read

Start with an operation that reads provider status. The following **partial integration builder** uses the real API; `statusUrl`, `recordReply`, and `recordFailure` are application values you must supply. Both callbacks return one registered command type.

```haskell
import Integration qualified
import Integration.Http qualified as Http

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

Design these boundaries before enabling a real charge:

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

## Exercise: a lost payment reply

Describe the shop state after a timeout, what the customer sees, and how an operator resolves it.

<details>
<summary>Suggested checks</summary>

Use a test provider that accepts an operation and then drops the connection. Check that a repeated request cannot double-charge under your chosen provider contract. Check refusal, malformed JSON, a valid JSON error status, an unknown operation ID, and a later reconciliation result. Record real-provider sandbox verification separately from unit tests of response mapping.

</details>

For a smaller provider integration, continue with [email](/connect/email/).

## Implementation and examples

- [integrations/Integration/Http/Request.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Http/Request.hs)
- [integrations/Integration/Http/Response.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Http/Response.hs)
- [integrations/Integration/Http/Internal.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Http/Internal.hs)
- [integrations/Integration/Http/Retry.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/Http/Retry.hs)
- [integrations/test/Integration/Http/InternalSpec.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/test/Integration/Http/InternalSpec.hs)
- [core/service/Integration.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration.hs)
