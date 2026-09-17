---
title: Entities and state
description: Understand how recorded facts become the state used for the next business decision.
sidebar:
  order: 3
---

Before accepting a new request, an application needs to know the relevant current state. Knowing how that state came about also helps explain past decisions. NeoHaskell connects those needs: an entity's current state is built by applying its events in order.

An **entity** is the business object whose rules you are protecting. It might represent a reservation, a document, or an account. In the practice project it is one Cart, identified by a UUID. Its state helps decide the next command; its event history records the changes that were accepted.

This page assumes that [your first working slice](/build/first-cart/) and [Cart additions](/build/commands-and-events/) are in the same `mug-shop` project. If you arrive here directly, open the project created by Getting Started and use those two pages' complete file checkpoints first; this page's complete entity file refers to `CartItem` and `ItemAdded` from the additions checkpoint.

## Follow one Cart through time

Suppose the event history for one Cart is:

| Recorded event | Resulting state |
| --- | --- |
| `CartCreated` | The Cart has an identifier, an owner identifier, and no entries. |
| `ItemAdded`, quantity 2 | One entry holds the selected stock identifier and quantity 2. |
| `ItemAdded`, quantity 1 | A second entry is appended, even if it refers to the same stock. |

The distinction between an entry and a total quantity is a model choice. The current example does not merge repeated additions. It also does not remove items, capture a price, or mark a Cart checked out. Those are separate business decisions that deserve their own facts and rules.

## Decide where state behaviour lives

Work from the `mug-shop` project root. `src/Shop/Cart/Event.hs` owns the Cart event vocabulary and `getEventEntityId`; `src/Shop/Cart/Item.hs` owns the small value stored in each entry; `src/Shop/Cart/Entity.hs` owns the state record, its starting value, and replay. The domain facade at `src/Shop/Cart/Core.hs` re-exports the entity and event types for commands and queries.

The additions page already told you to replace `src/Shop/Cart/Entity.hs`. This page explains why that file has its order and boundaries, then shows the complete current file. If you are applying the change now, replace the file at that path with the assembled block below. Do not move `initialState`, `update`, or `getEventEntityId` behind the marker that depends on them.

## Read the state and its wiring

The Cart state record holds the facts that later decisions need:

```haskell
data CartEntity = CartEntity
  { cartId :: Uuid
  , ownerId :: Text
  , items :: Array CartItem
  }
```

Reconstruction begins with an empty array and a nil identifier:

```haskell
initialState :: CartEntity
initialState = CartEntity {cartId = Uuid.nil, ownerId = "", items = Array.empty}
```

The `CartCreated` case establishes identity and owner. The `ItemAdded` case appends an entry already accepted by the command:

```haskell
update change cart = case change of
  CartCreated created ->
    CartEntity {cartId = created.entityId, ownerId = created.ownerId, items = Array.empty}
  ItemAdded added ->
    cart {items = cart.items |> Array.push (CartItem {stockId = added.stockId, quantity = added.quantity})}
```

The update function applies an accepted fact. Validation belongs before recording that fact; external work belongs in an integration reacting to it. The Cart update does not call a warehouse, consult today's catalogue, or reconsider whether the request should have been accepted.

After `initialState` and `update`, connect the entity to its event type with the helper exported by the framework-facing `Core` import:

```haskell
deriveEntity ''CartEntity ''CartEvent
```

The `getEventEntityId` function from `Event.hs` must be imported before this marker. These companions are the behaviour to review with your agent. The marker supplies the links between `CartEntity` and `CartEvent`, JSON conversion, the default starting value, and the framework's replay and event-routing instances. It does not require the entity's fields to support `Show`.

## Replay is a business boundary

Keeping validation out of `update` makes replay stable. If rebuilding yesterday's Cart consulted today's product price, the same history could produce a different commercial result. When a price must become part of an order agreement, design an event that records the agreed amount and currency at the appropriate moment.

Capturing that price is a **design extension to the practice project**, not a field already present in this Cart. The general lesson is to preserve the information that gives a past decision its meaning. See [language essentials](/build/language-essentials/#amounts-and-money) for the current Decimal type and its limits.

Snapshots can reduce the work needed to reconstruct an entity. They are a performance aid; the behaviour you teach and verify remains the ordered application of recorded facts. Persistence and recovery receive their own treatment in [run and evolve](/operate/).

## Complete current entity file

The following is the assembled replacement for `src/Shop/Cart/Entity.hs`, relative to the `mug-shop` project root. The `CartEvent`, `CartItem`, and `ItemAdded` definitions it imports are complete on [commands and events](/build/commands-and-events/).

<!-- complete-file -->
```haskell title="src/Shop/Cart/Entity.hs"
module Shop.Cart.Entity (CartEntity (..), initialState, update) where

import Core
import Shop.Cart.Event (CartEvent (..), getEventEntityId)
import Shop.Cart.Events.CartCreated qualified as CartCreated
import Uuid qualified
import Array qualified
import Shop.Cart.Item (CartItem (..))
import Shop.Cart.Events.ItemAdded qualified as ItemAdded

data CartEntity = CartEntity
  { cartId :: Uuid
  , ownerId :: Text
  , items :: Array CartItem
  }

initialState :: CartEntity
initialState = CartEntity {cartId = Uuid.nil, ownerId = "", items = Array.empty}

update :: CartEvent -> CartEntity -> CartEntity
update change cart = case change of
  CartCreated created ->
    CartEntity {cartId = created.entityId, ownerId = created.ownerId, items = Array.empty}
  ItemAdded added ->
    cart {items = cart.items |> Array.push (CartItem {stockId = added.stockId, quantity = added.quantity})}

deriveEntity ''CartEntity ''CartEvent
```

## Exercise: explain a reconstruction

Give your agent this history: a Cart is created, two mugs are added, then three more are added. Ask it to predict both the number of entries and the total quantity. Then have it demonstrate the reconstruction from `initialState`.

<details>
<summary>Suggested reasoning and checks</summary>

Expect two entries and five units under the current model. An empty history yields the initial state; creation alone yields a real empty Cart. A rejected zero-quantity command must not contribute an `ItemAdded` fact. Replaying the same accepted history from the same initial state should give the same state. Do not confuse this with appending the history twice: duplicate accepted additions change the result unless your application has designed duplicate handling.

</details>

Next: [queries](/build/queries/) turns this state into information useful on a screen.

Continue working in your project with `neo build`, `neo test`, and `neo ide`. The [testing lesson](/build/testing/) adds a replay check for these exact Cart modules.
