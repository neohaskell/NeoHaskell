---
title: Queries and useful views
description: Build read models around the questions each reader needs answered.
sidebar:
  order: 4
---

A screen or report needs information shaped around its reader's question. Displaying the application's entire internal history would make that question harder to answer. A query prepares a useful view, such as work awaiting review or the progress of a request.

NeoHaskell's read models separate presenting information from deciding whether a change is allowed. That gives you freedom to shape the view, with a tradeoff: a newly accepted change may take a short time to appear in it.

This page follows [Cart additions](/build/commands-and-events/). That page changed the same Cart entity so it contains `items`; the query below reads that state. Follow [your first working slice](/build/first-cart/) and the additions page in one `mug-shop` project. If you arrive here directly, use their complete checkpoints first, then create or replace `src/Shop/Cart/Queries/CartSummary.hs` with the complete file on this page.

## Begin with the question on the screen

The existing `CartSummary` answers: “Which Cart is this, who owns it, how many entries does it have, and is it empty?” It does not report total units or prices. Decide the meaning of each field before asking an agent to add one: `itemCount` currently means entries, so one addition of five mugs produces a count of one.

The query's business logic belongs in `src/Shop/Cart/Queries/CartSummary.hs`. From the `mug-shop` project root, replace that file after reviewing the focused projection below:

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

For this query, the file defines the data record, `canAccess`, and `canView`, then calls the canonical helper:

```haskell
deriveQuery ''CartSummary [''CartEntity]
```

Put the relevant `QueryOf` business instance **after** that marker: it depends on the `Query` instance the marker generates. The marker comes from the framework-facing `Core` import and generates standard query support. The complete file below preserves the required imports and declaration order.

The application registration is already present in `src/App.hs` from the first slice. If an existing application has the Cart service but no query registration, add this line beside its service registration:

```haskell
  |> Application.withQuery @CartSummary
```

The marker's internal name is `CartSummary`; the HTTP URL is `/queries/cart-summary`. The practice query deliberately allows public access. Before exposing private application data, define and test the [access-control policies](/build/access-control/).

## Complete current query file

Create the `src/Shop/Cart/Queries` directory if necessary, then replace `src/Shop/Cart/Queries/CartSummary.hs` with this assembled file from the project root:

<!-- complete-file -->
```haskell title="src/Shop/Cart/Queries/CartSummary.hs"
module Shop.Cart.Queries.CartSummary (CartSummary (..), canAccess, canView) where

import Array qualified
import Core
import Service.AccessControl (AccessError, UserClaims)
import Service.AccessControl qualified as AccessControl
import Shop.Cart.Core (CartEntity (..))

data CartSummary = CartSummary
  { cartSummaryId :: Uuid
  , ownerId :: Text
  , itemCount :: Int
  , isEmpty :: Bool
  }

canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = AccessControl.publicAccess

canView :: Maybe UserClaims -> CartSummary -> Maybe AccessError
canView = AccessControl.publicView

deriveQuery ''CartSummary [''CartEntity]

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

`Array qualified` is a real import used by the projection; keep it when assembling the file. `canAccess` and `canView` are explicit application policy functions. They are public here so the exercise can inspect a Cart without authentication; that convenience is not a recommendation for private data.

## Find your Cart

Run the application from the `mug-shop` project root:

```sh
neo build
neo run
```

Create a Cart, add an item as described in [Cart additions](/build/commands-and-events/), and replace `YOUR-CART-UUID` with the identifier returned by creation:

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

## Exercise: the Cart badge

The interface says “5 items,” but the customer made one addition of five mugs. Should the badge show one or five? State the meaning, then ask your agent to identify what must change.

<details>
<summary>Suggested reasoning and checks</summary>

The existing summary reports one entry. If the badge means units, design a quantity total rather than relabelling `itemCount`. Verify one addition of five, two additions of the same product, an empty Cart, and a rejected addition. Also query an unknown Cart ID: the filter should produce no matching row, not another customer's Cart. Tests should wait for a bounded projection update rather than assuming immediate visibility.

</details>

Next: [stock and checkout](/build/stock-and-checkout/).
