---
title: Queries and useful views
description: Build read models around the questions each reader needs answered.
sidebar:
  order: 4
---

A screen or report needs information shaped around its reader's question. Displaying the application's entire internal history would make that question harder to answer. A query prepares a useful view, such as work awaiting review or the progress of a request.

NeoHaskell's read models separate presenting information from deciding whether a change is allowed. That gives you freedom to shape the view, with a tradeoff: a newly accepted change may take a short time to appear in it.

We practise with a cart summary: a customer needs their selections, while a merchant needs different stock information. This page continues the [first cart](/build/first-cart/) example. Run `neo run` from your project to serve the routes below.

## Begin with the question on the screen

The existing `CartSummary` answers: “Which cart is this, who owns it, how many entries does it have, and is it empty?” It does not report total units or prices.

Its business logic lives in `src/Shop/Cart/Queries/CartSummary.hs`:

```haskell
instance QueryOf CartEntity CartSummary where
  queryId cart = cart.cartId
  combine cart _previous = do
    let count = cart.items |> Array.length
    Update CartSummary
      { cartSummaryId = cart.cartId
      , ownerId = cart.ownerId
      , itemCount = count
      , isEmpty = count == 0
      }
```

`queryId` determines which view row this entity contributes to. `combine` receives current entity state and the existing view, if any. Here the current entity contains everything needed, so the old view is unused and replaced with `Update`.

Other outcomes are `Delete`, which removes the view row, and `NoOp`, which leaves it unchanged. More than one entity type can contribute to a query. Start with one until your screen has a reason for a combined view.

## Derive and register the view

For a new query, define its data, `canAccess`, and `canView`, then call `deriveQuery ''CartSummary [''CartEntity]`. Put the relevant `QueryOf` business instances **after** that marker: they depend on the `Query` instance it generates. Import the marker from `Service.Query.TH` and let it generate the standard instances. The application must also register the query with `Application.withQuery @CartSummary`.

The marker call in your query file is:

```haskell
deriveQuery ''CartSummary [''CartEntity]
```

The marker's internal name is `CartSummary`; the HTTP URL is `/queries/cart-summary`. Your current practice query deliberately allows public access. Before exposing private application data, define and test the [access-control policies](/build/access-control/).

## Find your cart

This runnable shell request uses NeoQL equality filtering. Replace `YOUR-CART-UUID` with the identifier returned by cart creation:

```sh
curl --get http://localhost:8080/queries/cart-summary \
  --data-urlencode 'q=.cartSummaryId == "YOUR-CART-UUID"' \
  --data-urlencode 'limit=10' \
  --data-urlencode 'offset=0'
```

Expect a page object. Its `items` array contains the matching summary once the projection catches up. `total` reflects the accessible, filtered result count; `hasMore` says whether more matching results remain; `effectiveLimit` reports the applied page cap.

The defaults are a page size of 100 and offset zero, with an absolute maximum size of 1000. A query can set a lower cap by defining `maxResults :: Int` before its marker. Clients should use the returned `effectiveLimit` when advancing through pages.

Current NeoQL supports field access and equality with string or numeric literals. It is not a general SQL language: do not invent joins, sorting, compound conditions, or boolean literal comparisons. Invalid syntax gives a parse error; expressions are limited to 500 characters.

## Avoid a misleading loading screen

After an accepted command, show a clear pending state while the view catches up. Read again with a bounded retry and a useful failure state. An empty first response is not proof that the command failed. Resubmitting an addition merely because its summary has not appeared can add it twice.

## Exercise: the cart badge

The interface says “5 items,” but the customer made one addition of five mugs. Should the badge show one or five? State the meaning, then ask your agent to identify what must change.

<details>
<summary>Suggested reasoning and checks</summary>

The existing summary reports one entry. If the badge means units, design a quantity total rather than relabelling `itemCount`. Verify one addition of five, two additions of the same product, an empty cart, and a rejected addition. Also query an unknown cart ID: the filter should produce no matching row, not another customer's cart. Tests should wait for a bounded projection update rather than assuming immediate visibility.

</details>

Next: [stock and checkout](/build/stock-and-checkout/).
