---
title: "Commands and events"
description: Add a business action while keeping requests, accepted facts, and state separate.
sidebar:
  order: 2
---

An application must distinguish what someone requested from what it accepted. That distinction gives you a place to express rules, explain refusals, and question an agent's implementation.

A **command** names an intention. An **event** names an accepted fact. In your practice project, `AddItem` requests two mugs; `ItemAdded` records an accepted addition. The [event model](/start/event-modeling/) gives those names a shared meaning.

Examples below show the relevant declarations and behaviour, with each destination named. Module headers and imports are omitted so you can focus on the idea. The [complete cart additions files](/examples/mug-shop-cart.tar.gz) include that setup and the tests; add them to the same project when you want the runnable checkpoint.

The derive helpers used here come from `Core`. Older code may call the TH
markers `event`, `command`, or `outboundIntegration` from their defining modules;
those names remain compatibility APIs. New examples use the consistent
`derive…` names.

## Choose the rule before the files

Continue in the same `mug-shop` project. We will require an existing cart and a positive quantity. Each accepted addition becomes one entry, even when the same stock is selected again.

Availability and ownership are separate policies covered in [stock](/build/stock-and-checkout/) and [access control](/build/access-control/). This action records a selection. Recognising an absent rule is part of reviewing the implementation.

Stop the development server while changing these files. The introductory nonpersistent store starts fresh afterward.

## Give the new fact its own home

Create `src/Shop/Cart/Events/ItemAdded.hs`. Its payload preserves the identifiers and quantity needed to explain the addition:

```haskell
data Event = Event
  { entityId :: Uuid
  , stockId :: Uuid
  , quantity :: Int
  }
```

Derive the payload's standard instances with its event marker:

```haskell
deriveEvent ''Event
```

Add the fact to the domain's event type in `Event.hs`:

```haskell
data CartEvent
  = CartCreated CartCreated.Event
  | ItemAdded ItemAdded.Event
```

Its existing event marker continues to derive the standard instances for the expanded event type:

```haskell
deriveEvent ''CartEvent
```

`ItemAdded.Event` is the payload; `ItemAdded` is its wrapper in the domain's list of possible facts. Keeping the payload separate makes its meaning and future changes easy to locate. `Core.hs` remains a tiny re-export; it does not grow with every new rule.

## Retain the selection in state

A cart entry needs the selected stock and quantity:

```haskell
data CartItem = CartItem {stockId :: Uuid, quantity :: Int}
```

Create `Item.hs` for that value, then replace `Entity.hs` to add an `items` array and apply `ItemAdded`. The update appends one entry. It does not validate a request or contact a supplier. Its new branch is:

```haskell
  ItemAdded added ->
    cart {items = cart.items |> Array.push (CartItem {stockId = added.stockId, quantity = added.quantity})}
```

The quantity is an `Int`. The command below admits only positive values. Any additional producer of `ItemAdded` must preserve that same invariant, because replay treats the event as an accepted fact.

## Implement the decision

The request tells us which cart to load:

```haskell
getEntityId :: AddItem -> Maybe Uuid
getEntityId request = Just request.cartId
```

The decision refuses a missing cart, then checks the quantity. Notice how the event retains the accepted input:

```haskell
  if request.quantity <= 0
    then Decider.reject "Quantity must be positive"
    else Decider.acceptExisting
      [ItemAdded (ItemAdded.Event {entityId = cart.cartId, stockId = request.stockId, quantity = request.quantity})]
```

Place this rule in `src/Shop/Cart/Commands/AddItem.hs`. The complete file in the cart-additions download separates the existence decision from the quantity rule.

The command's `cartId` becomes the event's `entityId`. `stockId` identifies the selection; it is not a product name. The command marker generates routine plumbing from the decision, entity, and transport declarations above it:

```haskell
deriveCommand ''AddItem
```

## Register the action and refresh the answer

Replace `Service.hs` to register the new command:

```haskell
  |> Service.command @AddItem
```

Then update `CartSummary` to calculate the count from the entity:

```haskell
    let count = cart.items |> Array.length
```

Keep `CreateCart.hs`, `Events/CartCreated.hs`, `Core.hs`, and `App.hs`. Your application already registers this service and query, so it needs no extra pipeline step. The [query lesson](/build/queries/) explores the projection in more depth.

## Check the new behaviour

Run `neo build`, then `neo run`. Create a new cart with the earlier request and replace `YOUR-CART-UUID` below. The fixed stock UUID is an illustrative selection until the Stock lesson creates its real record.

```sh
curl -i http://localhost:8080/commands/add-item \
  -H 'Content-Type: application/json' \
  --data '{"cartId":"YOUR-CART-UUID","stockId":"11111111-1111-1111-1111-111111111111","quantity":2}'
```

Expect acceptance, then a summary with one entry and `isEmpty: false`. One entry contains two units. Send quantity zero: expect HTTP 400 with `reason: "Quantity must be positive"`, while the accepted count remains one.

The transport declaration, service registration, and application registration together expose `/commands/add-item`. A type sitting in a file is not yet a reachable feature.

## Exercise: a per-cart limit

Choose a limit of six mugs **per cart**. Your agent rejects requests above six and says the work is complete. What case has it missed?

<details>
<summary>Suggested reasoning and evidence</summary>

Two additions of four each pass that check but total eight. Specify whether the limit covers one product or every product, then compare existing quantities plus the request. Check a normal addition, exactly six, more than six, and another addition after reaching six. A refused operation must not produce a successful `ItemAdded`. This is an extension you design, not a rule already in these files.

</details>

Next: [entities and state](/build/entities-and-state/) explains how accepted facts inform the next decision.
