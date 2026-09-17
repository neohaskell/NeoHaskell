---
title: HTTP and frontends
description: Connect an interface to commands and queries and communicate the status of each request.
sidebar:
  order: 6
---

A user interface turns choices into requests and shows their results. It needs to distinguish acceptance, work still in progress, and failure so people know what happened and what they can do next.

NeoHaskell's web transport exposes commands and queries over HTTP. You can build the interface with a frontend framework suited to your team. The current transport serves the application API and its documentation; it does not provide a general static frontend hosting API.

Our worked example is a storefront for the ecommerce practice project. A cart addition, a pending reservation, and a confirmed order give us concrete examples of the different states an interface must communicate.

The examples continue in the same `mug-shop` project. The API is the part this
lesson implements; a browser frontend is an optional client you add alongside
the Neo project.

## Start from the real contract

With [your application running](/build/first-cart/) through `neo run`, open `http://localhost:8080/docs` to inspect the generated API documentation. The same schema is available at `/openapi.json` and `/openapi.yaml`.

| Purpose | Example route | Meaning of success |
| --- | --- | --- |
| Submit a business request | `POST /commands/add-item` | The Cart command was accepted. |
| Read a view | `GET /queries/cart-summary` | A page of currently available, authorised view rows was returned. |
| Inspect the interface | `GET /openapi.json` | The application's generated API schema was returned. |

Registration drives the interface: the command declares its transport, the service registers the command, and the application registers that service and its queries. The HTTP routes use kebab-case names. Do not infer a route from a screen label such as “checkout” if no matching command exists.

## Assemble the HTTP application wiring

If your project still uses the local nonpersistent store, create or replace
`src/App.hs` with this complete wiring. It exposes the Cart and Stock commands
and their query views through the web transport. If your `App.hs` already has
configuration, authentication, or another transport policy, keep those steps
and append only the service and query registrations that are missing.

<!-- complete-file -->
```haskell title="src/App.hs"
module App (app) where

import Core
import Maybe qualified
import Path qualified
import Service.Application (Application)
import Service.Application qualified as Application
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.Transport.Web qualified as WebTransport
import Shop.Cart.Queries.CartSummary (CartSummary)
import Shop.Cart.Service qualified as Cart
import Shop.Stock.Queries.StockLevel (StockLevel)
import Shop.Stock.Service qualified as Stock

app :: Application
app = Application.new
  |> Application.withEventStore @() (\_ -> SimpleEventStore
    { basePath = Path.fromText ".neo/events" |> Maybe.getOrDie
    , persistent = False
    })
  |> Application.withTransport WebTransport.server
  |> Application.withService Cart.service
  |> Application.withQuery @CartSummary
  |> Application.withService Stock.service
  |> Application.withQuery @StockLevel
```

From the project root, run `neo build` and then `neo run`. Open `/docs` and
`/openapi.json` to confirm the registered commands and queries are in the
generated contract before connecting a browser.

## Connect one action

This **partial browser JavaScript function** sends your application’s `AddItem` request. Call it with the real IDs from [stock and checkout](/build/stock-and-checkout/). It assumes the practice frontend uses a same-origin proxy for `/commands`; cross-origin development needs explicit server CORS configuration.

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

This is an adapted partial browser function, not a complete frontend file. Put
it in the module used by your frontend (for example, create
`frontend/cart.js` if your project has no browser code yet) and call it from
the event handler for an AddItem form. The Neo project does not generate that
frontend directory or configure a proxy for it. An authenticated application
must also supply its credential according to the authentication setup. This
local practice function is not a complete customer session implementation.

The UI should disable accidental duplicate submissions while the request is in flight, show a useful rejection, and refresh the relevant query after acceptance. A lost network response needs special care: the server may already have accepted the request. Decide how the application detects duplicates before automatically resending writes.

## Handle outcomes separately

The web transport maps accepted command responses to HTTP 200. Business rejections currently map to 400, with a `reason`; command failures also map to 400, with an `error`. Inspect the response body as well as the status rather than treating every 400 as invalid JSON.

Authentication and permission failures use 401 or 403. An unregistered route produces 404. Read models can temporarily lag an accepted write, so “accepted, refreshing” is a useful interface state. A bounded retry of the read is different from replaying the write.

## Put browser access in application wiring

The API has a `CorsConfig` with allowed origins, methods, headers, and an optional preflight cache age. This **partial application-wiring expression** illustrates a local frontend policy; it requires the existing `Application` and `WebTransport` imports:

```haskell
Application.withCors @() (\_ -> WebTransport.CorsConfig
  { allowedOrigins = ["http://localhost:4321"]
  , allowedMethods = ["GET", "POST", "OPTIONS"]
  , allowedHeaders = ["Content-Type", "Authorization"]
  , maxAge = Just 600
  })
```

Apply it in your application's pipeline and use your actual frontend origin. CORS governs browser access; it does not grant business permission. Protect private information with [access control](/build/access-control/).

Create or replace `tests/scenarios/create-cart.hurl` with this complete API
check. It gives the browser contract a repeatable server-side boundary before
you add a frontend. Stop `neo run` before running `neo test`.

<!-- complete-file -->
```hurl title="tests/scenarios/create-cart.hurl"
POST http://localhost:8080/commands/create-cart
Content-Type: application/json
[]

HTTP 200
[Captures]
cart_id: jsonpath "$.entityId"

GET http://localhost:8080/queries/cart-summary
[Options]
retry: 10
retry-interval: 200

HTTP 200
[Asserts]
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].itemCount" nth 0 == 0
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].isEmpty" nth 0 == true
```

Run `neo test` from the project root. The Hurl check proves the API response
and eventual query update; it does not prove that a browser layout, proxy, or
authentication provider is configured.

## Exercise: a delayed summary

The server accepted an addition, but the next summary still looks empty. Design the screen's next three actions without submitting another addition.

<details>
<summary>Suggested reasoning and checks</summary>

Show acceptance with a pending refresh, retry the read within a bounded interval, and offer a clear refresh/recovery state if it remains stale. Check a normal update, a rejected zero quantity, and a delayed projection. Simulate a network error after submission separately: “we could not confirm the outcome” is more accurate than claiming the addition failed. Verify that double-clicking does not silently add twice.

</details>

Next: [testing behaviour](/build/testing/).

Public sources: [web transport](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Transport/Web.hs), [command responses](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Response.hs), [application wiring](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Application.hs).
