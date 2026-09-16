---
title: Protect users, data, and actions
description: Turn access rules, secrets, database transport, and integration boundaries into concrete checks.
sidebar:
  order: 6
---

Different people need different access to an application. Decide which actions each role may perform, which records they may see, and what is deliberately public. These choices apply to internal tools and public services alike. The ecommerce practice project illustrates them with customer carts, staff access, and a public catalogue.

NeoHaskell provides authentication and authorisation building blocks. Your application still defines who may do what, wires the authentication provider, and tests the boundaries. The framework cannot infer your roles or ownership rules.

## Separate identity from permission

Authentication establishes who is making a request. Authorisation decides whether that identity may perform an action or see a result.

Queries have two useful decision points:

- `canAccess`: may this caller access this kind of query at all?
- `canView`: may this caller see this particular result?

The helpers in `Service.AccessControl` include `authenticatedAccess`, `requirePermission`, `ownerOnly`, `tenantOnly`, `publicAccess`, and `publicView`. Public access is an explicit choice; reserving it for catalogue information is an example domain policy, not a universal framework rule.

In the ecommerce example, these are **policy expressions for a cart query**, not a complete runnable module:

```haskell
AccessControl.authenticatedAccess
AccessControl.ownerOnly (\cart -> cart.ownerId)
```

The second expression assumes the example query has a `Text` ownership field named `ownerId` whose value matches the authenticated subject. Merely adding a field named “owner” does not enforce ownership. Wire the expressions into `src/Shop/Cart/Queries/CartSummary.hs` as shown in [access control](/build/access-control/) and test them.

Command permissions are separate: being allowed to read a record does not automatically grant permission to change it. In the cart example, read access does not grant the right to add items. Configure `Application.withAuth` at the web boundary: without authentication wiring, the current Web command path uses `trustedContext` and bypasses the command access gate. Adding a command policy alone is insufficient. Follow [access control](/build/access-control/) for the actual wiring and [testing](/build/testing/) for behavioural checks.

## Test with more than one identity

Create a compact access matrix for your application. This example policy uses the practice project’s customer, staff, and public roles:

| Caller | Own cart | Another customer's cart | Public catalogue |
| --- | --- | --- | --- |
| Anonymous | Denied | Denied | Allowed if deliberately public |
| Customer | Allowed | Denied | Allowed |
| Staff member | According to assigned permission | According to assigned permission | Allowed |

Test both collection and individual-result routes. Also check missing credentials, invalid credentials, and a legitimate caller with insufficient permission. A successful administrator request is weak evidence for isolation between ordinary users. Keep these cases in your project's `tests/` directory and run `neo test` with a disposable database after persistence is enabled.

## Keep secrets out of ordinary output

Declare sensitive configuration with `Config.secret`, as the database-password field in [persistence](/operate/persistence/) does. That provides configuration-level handling; it does not scrub arbitrary strings, request bodies, or provider responses you later log.

Persistent provider connections also require an appropriately configured secret store. The default in-memory secret store has process lifetime. Treat the chosen storage and its access controls as part of the deployment design.

For Postgres, `SslModeUnset` leaves the underlying default negotiation in place. The configuration supports explicit modes including `SslModeRequire`, `SslModeVerifyCa`, and `SslModeVerifyFull`, plus an optional root CA path. Wire the intended settings into **every** relevant database subsystem and verify connectivity in staging. A TLS setting applied only to the event store does not configure an independently created file or query store.

## Keep the development IDE local

`neo ide` binds `127.0.0.1:2323` by default. Passing `--host 0.0.0.0` makes it reachable on other interfaces. That is a deliberate exposure change for a tool that operates on your project; the command is not a production user portal.

## Exercise: challenge the agent's proposal

In the practice project, your agent proposes making all cart queries public to simplify a frontend error. Ask it to identify the failed authorisation boundary, keep the intended policy, and demonstrate a customer's successful request alongside another customer's denied request.

The useful outcome is an explained access rule with evidence. An error disappearing after broadening access is not sufficient.

Next, assess [performance](/operate/performance/) under those same business and access rules.
