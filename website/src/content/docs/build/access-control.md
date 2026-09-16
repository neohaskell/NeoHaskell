---
title: Access control
description: Decide who may act, which records they may see, and how to verify those boundaries.
sidebar:
  order: 8
---

Different people need different access to an application. Someone may be allowed to view a record but not change it, or to manage their own records without seeing anyone else's. These are application policies before they become authentication settings.

NeoHaskell supplies identity and permission mechanisms, but your application must connect them and declare its policies. We will practise with customers who should see their own carts and a merchant with wider permissions. Your current `mug-shop` project deliberately allows local anonymous practice. This chapter shows how to tighten those policies when you introduce a real identity service.

## Separate identity from permission

**Authentication** establishes who the caller is. **Authorisation** decides what that caller may do or see.

The web transport can validate JWT credentials when the application wires `Application.withAuth`. Commands receive the resulting identity in `RequestContext.user`. A client-provided `ownerId` is not equivalent to a validated user identity.

Add this **application pipeline step** in `src/App.hs` to enable the application's JWT authentication using an auth server URL. The example hostname is a placeholder, not a working provider:

```haskell
Application.withAuth @() (\_ -> "https://auth.example.com")
```

Keep this registration when later chapters extend `App.hs`. Use your actual identity service and test its discovery, issuer, audience, and token configuration. `withAuthOverrides` supports configuration overrides. Deployment-specific identity setup belongs in your application's operational documentation.

## Protect both the command and the record

Commands can define a top-level `canAccess` function before their `command` marker. The marker connects it to the pre-execution permission check. Without an explicit function, the command class defaults to requiring authentication.

Permission to use a command may still depend on the particular record it affects. In the practice project, an authenticated customer should not edit another customer's cart. In the decision function, compare the validated subject with the cart's recorded owner before accepting a change. The `AddItem` you wrote in `src/Shop/Cart/Commands/AddItem.hs` currently ignores its request context.

There is an important deployment boundary: **without `Application.withAuth`, the current web transport creates a trusted command context and bypasses the command permission gate**. Merely declaring `canAccess` does not secure an application whose authentication is unwired. Domain checks inside `decide` remain your code's responsibility.

## Check the owner before accepting a change

In `src/Shop/Cart/Commands/AddItem.hs`, replace `decide` and add `addForOwner` below it. Keep the existing `addToCart` quantity helper and the type declarations:

```haskell
decide :: AddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide request existing context = case context.user of
  Nothing -> Decider.reject "Sign in before changing a cart"
  Just user -> addForOwner request existing user

addForOwner :: AddItem -> Maybe CartEntity -> UserClaims -> Decision CartEvent
addForOwner request existing user = case existing of
  Nothing -> Decider.reject "Cart not found!"
  Just cart ->
    if cart.ownerId == user.sub
      then addToCart request cart
      else Decider.reject "This cart belongs to another user"
```

This is an **authenticated variant**, to introduce alongside your identity-service setup. It changes the earlier anonymous contract: the original anonymous HTTP tests will now fail until you supply valid test credentials and create carts under that identity. Keep a development checkpoint before the change, and add owner, other-user, and missing-user tests rather than silently weakening the new rule.

`CreateCart` already records `context.user.sub` for a signed-in caller. Carts created anonymously in earlier exercises do not automatically belong to a newly signed-in user. Use new authenticated carts when checking this variant; a guest-to-account transfer needs its own explicit design.

## Protect the view separately

Queries require two policies. `canAccess` decides whether the caller may use the query type; `canView` decides whether a particular row is visible.

This **replacement for your CartSummary policies** uses the real helper API. It assumes `CartSummary` retains its `ownerId :: Text` field; `AccessControl` supplies the ownership helper:

```haskell
canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = AccessControl.authenticatedAccess

canView :: Maybe UserClaims -> CartSummary -> Maybe AccessError
canView = AccessControl.ownerOnly (.ownerId)
```

Place these before `deriveQuery`. `ownerOnly` compares the row's owner with the validated `sub` claim. The endpoint filters out rows that fail `canView`; it computes pagination totals after authorisation and filtering. A user who can access the query but owns no matching carts receives an empty result set, not another customer's information.

Your initial CartSummary uses `publicAccess` and `publicView`. Those can suit a product catalogue, but make a deliberate choice before applying them to customer data. Other helpers include `requirePermission`, `requireAnyPermission`, `requireAllPermissions`, and `tenantOnly`.

## Design guest carts explicitly

`CreateCart` records the authenticated subject when available; otherwise it generates an anonymous owner identifier. That generated identifier does not automatically become a secure browser session or give a later logged-in user ownership.

If you add guest checkout to the practice project, decide how a guest proves access to their cart and how ownership changes after login. The same design question arises whenever anonymous work must later belong to an authenticated user. Model and test that transition. Do not solve it by accepting an arbitrary owner identifier from the request body.

## Exercise: another customer's cart

Create a test plan for the practice project using two customers and one merchant. What should each be able to read and change? Include a request with no credentials and one with an invalid token.

<details>
<summary>Suggested reasoning and checks</summary>

The owner should read their cart and perform permitted changes. The other customer should neither see its row nor successfully mutate it. A merchant's access depends on your explicit permission policy, not merely on being logged in. Missing credentials should fail an authenticated query; an invalid token should be rejected by the transport. Exercise the real authenticated web setup as well as unit tests: a unit test cannot detect that production forgot to wire authentication.

</details>

Next: [configuration](/build/configuration/) makes these deployment choices explicit.

Public sources: [access helpers](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/AccessControl.hs), [request context](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Auth.hs), [command defaults](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Command/Core.hs), [query endpoint](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Query/Endpoint.hs), [web authentication dispatch](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Transport/Web.hs).

Connecting a user's external account is a separate concern from signing in to your application. See [provider accounts and consent](/connect/provider-accounts/) for that workflow.
