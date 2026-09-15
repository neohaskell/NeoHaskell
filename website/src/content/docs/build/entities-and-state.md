---
title: Entities and state
description: Understand how recorded facts become the state used for the next business decision.
sidebar:
  order: 3
---

The shop needs to know what is in a cart now. It can also benefit from knowing how the cart reached that point. NeoHaskell connects those needs: an entity's current state is built by applying its events in order.

An **entity** is the business object whose rules you are protecting. In our example it is one cart, identified by a UUID. Its state helps decide the next command; its event history records the changes that were accepted.

## Follow one cart through time

The public Cart model behaves like this:

| Recorded event | Resulting state |
| --- | --- |
| `CartCreated` | The cart has an identifier, an owner identifier, and no entries. |
| `ItemAdded`, quantity 2 | One entry holds the selected stock identifier and quantity 2. |
| `ItemAdded`, quantity 1 | A second entry is appended, even if it refers to the same stock. |

The distinction between an entry and a total quantity is a model choice. The current example does not merge repeated additions. Nor does it remove items, capture a price, or mark a cart checked out. You can locate the update function in the [IDE graph](/getting-started/visual-ide/) and ask your agent to show you each case.

## Read the state and its wiring

Exact excerpt from the public Cart model:

```haskell
data CartEntity = CartEntity
  { cartId :: Uuid,
    ownerId :: Text,
    items :: Array CartItem
  }
  deriving (Generic)
```

The entity connects to its initial state and event application through another exact excerpt:

```haskell
instance Entity CartEntity where
  initialStateImpl = initialState
  updateImpl = update
```

The `initialState` is the starting value for reconstruction. It uses an empty array and a nil identifier. That starting value is not evidence that a real cart was created; the creation event establishes the cart's identity.

The model also declares `EventOf CartEntity = CartEvent` and `EntityOf CartEvent = CartEntity`. These tell the framework which event type belongs to the entity. An event's `getEventEntityIdImpl` identifies the stream it affects.

Entities have no equivalent of the command marker: their state and update behaviour are business logic. Your agent writes the record, serialization instances, type connections, and `Entity` instance. For new event types it can use the [event marker](/build/commands-and-events/) for the mechanical instances.

## Apply facts; make decisions elsewhere

The Cart update appends an entry when it receives `ItemAdded`. It does not call a warehouse, consult today's catalogue, or ask whether the request should have been accepted. Those decisions belong before recording the fact, or in an integration reacting to it.

This separation helps replay produce a stable answer. If rebuilding yesterday's cart consulted today's product price, the same history could produce a different commercial result. When a price must become part of an order agreement, design an event that records the agreed amount and currency at the appropriate moment.

That is a **shop design requirement**, not a field already present in this cart. See [language essentials](/build/language-essentials/#amounts-and-money) for the current Decimal type and its limits.

Snapshots can reduce the work needed to reconstruct an entity. They are a performance aid; the behaviour you teach and verify remains the ordered application of recorded facts. Persistence and recovery receive their own treatment in [run and evolve](/operate/).

## Exercise: explain a reconstruction

Give your agent this history: a cart is created, two mugs are added, then three more are added. Ask it to predict both the number of entries and the total quantity. Then have it demonstrate the reconstruction.

<details>
<summary>Suggested reasoning and checks</summary>

Expect two entries and five units under the current model. An empty history yields the initial state; creation alone yields a real empty cart. A rejected zero-quantity command must not contribute an `ItemAdded` fact. Replaying the same accepted history from the same initial state should give the same state. Do not confuse this with appending the history twice: duplicate accepted additions change the result unless your application has designed duplicate handling.

</details>

Next: [queries](/build/queries/) turns this state into information useful on a screen.

Public sources: [Cart entity and update](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Core.hs), [starter replay test](https://github.com/neohaskell/NeoHaskell/blob/main/neo/starter/tests/Property/CounterReplaySpec.hs).
