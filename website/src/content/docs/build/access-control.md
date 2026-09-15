---
title: Access control
description: Decide who may act, which records they may see, and how to verify those boundaries.
sidebar:
  order: 8
---

A customer should see their own cart. A warehouse worker may need stock information without seeing every customer's details. A merchant can change policies that ordinary customers cannot. These are business decisions before they become authentication settings.

NeoHaskell supplies identity and permission mechanisms, but your application must connect them and declare its policies. The public Cart testbed is intentionally permissive; it is a teaching example, not a ready-made access policy for a live shop.

## Separate identity from permission

**Authentication** establishes who the caller is. **Authorisation** decides what that caller may do or see.

The web transport can validate JWT credentials when the application wires `Application.withAuth`. Commands receive the resulting identity in `RequestContext.user`. A client-provided `ownerId` is not equivalent to a validated user identity.

This **partial wiring expression** enables the application's JWT authentication using an auth server URL. The example hostname is a placeholder, not a working provider:

```haskell
Application.withAuth @() (\_ -> "https://auth.example.com")
```

Use your actual identity service and test its discovery, issuer, audience, and token configuration. `withAuthOverrides` supports configuration overrides. Deployment-specific identity setup belongs in your application's operational documentation.

## Protect both the command and the record

Commands can define a top-level `canAccess` function before their `command` marker. The marker connects it to the pre-execution permission check. Without an explicit function, the command class defaults to requiring authentication.

An authenticated customer still should not edit another customer's cart. In the decision function, compare the validated subject with the cart's recorded owner before accepting a change. The existing `AddItem` ignores its request context, so it does not perform that ownership check.

There is an important deployment boundary: **without `Application.withAuth`, the current web transport creates a trusted command context and bypasses the command permission gate**. Merely declaring `canAccess` does not secure an application whose authentication is unwired. Domain checks inside `decide` remain your code's responsibility.

## Protect the view separately

Queries require two policies. `canAccess` decides whether the caller may use the query type; `canView` decides whether a particular row is visible.

This **partial replacement for the demo CartSummary policies** uses the real helper API. It assumes `CartSummary` retains its `ownerId :: Text` field and the module imports `AccessError`, `UserClaims`, and qualified `Service.AccessControl`:

```haskell
canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = AccessControl.authenticatedAccess

canView :: Maybe UserClaims -> CartSummary -> Maybe AccessError
canView = AccessControl.ownerOnly (.ownerId)
```

Place these before `deriveQuery`. `ownerOnly` compares the row's owner with the validated `sub` claim. The endpoint filters out rows that fail `canView`; it computes pagination totals after authorisation and filtering. A user who can access the query but owns no matching carts receives an empty result set, not another customer's information.

The public example instead uses `publicAccess` and `publicView`. Those can suit a product catalogue, but make a deliberate choice before applying them to customer data. Other helpers include `requirePermission`, `requireAnyPermission`, `requireAllPermissions`, and `tenantOnly`.

## Design guest carts explicitly

`CreateCart` records the authenticated subject when available; otherwise it generates an anonymous owner identifier. That generated identifier does not automatically become a secure browser session or give a later logged-in user ownership.

If the shop needs guest checkout, decide how a guest proves access to their cart and how ownership changes after login. Model and test that transition. Do not solve it by accepting an arbitrary owner identifier from the request body.

## Exercise: another customer's cart

Create a test plan using two customers and one merchant. What should each be able to read and change? Include a request with no credentials and one with an invalid token.

<details>
<summary>Suggested reasoning and checks</summary>

The owner should read their cart and perform permitted changes. The other customer should neither see its row nor successfully mutate it. A merchant's access depends on your explicit permission policy, not merely on being logged in. Missing credentials should fail an authenticated query; an invalid token should be rejected by the transport. Exercise the real authenticated web setup as well as unit tests: a unit test cannot detect that production forgot to wire authentication.

</details>

Next: [configuration](/build/configuration/) makes these deployment choices explicit.

Public sources: [access helpers](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/AccessControl.hs), [request context](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Auth.hs), [command defaults](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Command/Core.hs), [query endpoint](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Query/Endpoint.hs), [web authentication dispatch](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Transport/Web.hs).

Connecting a merchant’s external account is a separate concern from signing in to the shop. See [provider accounts and consent](/connect/provider-accounts/) for that workflow.
