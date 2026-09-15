---
title: HTTP and the storefront
description: Connect a storefront to commands and queries without confusing a button click with a completed workflow.
sidebar:
  order: 6
---

A storefront turns a customer's choices into requests and shows the results. Its most important job is to communicate the application's actual state: a cart addition, a pending reservation, and a confirmed order mean different things.

NeoHaskell's web transport exposes commands and queries over HTTP. You can build the storefront with a frontend framework suited to your team. The current transport serves the application API and its documentation; it does not provide a general static storefront hosting API.

## Start from the real contract

With the [testbed running](/build/first-cart/), open `http://localhost:8080/docs` to inspect the generated API documentation. The same schema is available at `/openapi.json` and `/openapi.yaml`.

| Purpose | Example route | Meaning of success |
| --- | --- | --- |
| Submit a business request | `POST /commands/add-item` | The Cart command was accepted. |
| Read a view | `GET /queries/cart-summary` | A page of currently available, authorised view rows was returned. |
| Inspect the interface | `GET /openapi.json` | The application's generated API schema was returned. |

Registration drives the interface: the command declares its transport, the service registers the command, and the application registers that service and its queries. The HTTP routes use kebab-case names. Do not infer a route from a screen label such as “checkout” if no matching command exists.

## Connect one action

This **partial browser JavaScript function** sends the existing testbed request. Call it with the real IDs from [stock and checkout](/build/stock-and-checkout/). It assumes your storefront uses a same-origin proxy for `/commands`; cross-origin development needs explicit server CORS configuration.

```javascript
async function addMugs(cartId, stockId, quantity) {
  const response = await fetch('/commands/add-item', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ cartId, stockId, quantity }),
  });
  const result = await response.json();
  if (!response.ok) {
    throw new Error(result.reason ?? result.error ?? 'Could not add mugs');
  }
  return result.entityId;
}
```

An authenticated application must also supply its credential according to the authentication setup. This anonymous demo function is not a complete customer session implementation.

The UI should disable accidental duplicate submissions while the request is in flight, show a useful rejection, and refresh the relevant query after acceptance. A lost network response needs special care: the server may already have accepted the request. Decide how the application detects duplicates before automatically resending writes.

## Handle outcomes separately

The web transport maps accepted command responses to HTTP 200. Business rejections currently map to 400, with a `reason`; command failures also map to 400, with an `error`. Inspect the response body as well as the status rather than treating every 400 as invalid JSON.

Authentication and permission failures use 401 or 403. An unregistered route produces 404. Read models can temporarily lag an accepted write, so “accepted, refreshing” is a useful interface state. A bounded retry of the read is different from replaying the write.

## Put browser access in application wiring

The API has a `CorsConfig` with allowed origins, methods, headers, and an optional preflight cache age. This **partial application-wiring expression** illustrates a local storefront policy; it requires the existing `Application` and `WebTransport` imports:

```haskell
Application.withCors @() (\_ -> WebTransport.CorsConfig
  { allowedOrigins = ["http://localhost:4321"]
  , allowedMethods = ["GET", "POST", "OPTIONS"]
  , allowedHeaders = ["Content-Type", "Authorization"]
  , maxAge = Just 600
  })
```

Apply it in your application's pipeline and use your actual frontend origin. CORS governs browser access; it does not grant business permission. Protect customer information with [access control](/build/access-control/).

## Exercise: a delayed summary

The server accepted an addition, but the next summary still looks empty. Design the screen's next three actions without submitting another addition.

<details>
<summary>Suggested reasoning and checks</summary>

Show acceptance with a pending refresh, retry the read within a bounded interval, and offer a clear refresh/recovery state if it remains stale. Check a normal update, a rejected zero quantity, and a delayed projection. Simulate a network error after submission separately: “we could not confirm the outcome” is more accurate than claiming the addition failed. Verify that double-clicking does not silently add twice.

</details>

Next: [testing behaviour](/build/testing/).

Public sources: [web transport](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Transport/Web.hs), [command responses](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Response.hs), [application wiring](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Application.hs).
