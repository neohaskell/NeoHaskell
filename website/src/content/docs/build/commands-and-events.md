---
title: Commands and events
description: Express business requests, reject invalid choices, and record accepted facts.
sidebar:
  order: 2
---

An application must distinguish what someone requested from what it accepted. Keeping those separate gives you a clear place to express rules, explain refusals, and question an agent's implementation.

A **command** names an intention. An **event** names something accepted as having happened. In our ecommerce practice project, a customer asks to add two mugs with `AddItem`; acceptance records `ItemAdded`. Start with the [event-modeling introduction](/start/event-modeling/) if you want to sketch that distinction before reading code.

## Name the decision before implementing it

The public Cart example has two rules for adding an item:

1. The cart must exist.
2. The requested quantity must be greater than zero.

It does not check stock or ownership here. Those are separate concerns we address in [stock and checkout](/build/stock-and-checkout/) and [access control](/build/access-control/). Recognising an absent rule is part of assessing the implementation.

Here is the decision function, exactly as it appears in `Testbed.Cart.Commands.AddItem`:

```haskell
decide :: AddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide cmd entity _ctx = case entity of
  Nothing ->
    Decider.reject "Cart not found!"
  Just cart ->
    if cmd.quantity <= 0
      then Decider.reject "Quantity must be positive"
      else
        Decider.acceptExisting
          [ ItemAdded
              { entityId = cart.cartId
              , stockId = cmd.stockId
              , quantity = cmd.quantity
              }
          ]
```

Read the signature as: “Given the request, possibly a cart, and the request context, decide which cart events to accept.” `Nothing` means no entity was found. `Just cart` means there is a state to inspect. `RequestContext` carries information such as authenticated identity; `_ctx` makes explicit that this example does not use it.

`acceptExisting` targets an existing stream. For creating a new cart, the example generates an identifier and uses `acceptNew`. Rejection is a business outcome, not an accepted event describing a successful addition.

## Connect the command to the application

For a new command, your agent supplies the data, the entity lookup, the decision, and the type wiring. The following is a **partial declaration pattern**, adapted from the public example; the `decide` definition above and appropriate imports belong in the same module:

```haskell
data AddItem = AddItem
  { cartId :: Uuid
  , stockId :: Uuid
  , quantity :: Int
  }

getEntityId :: AddItem -> Maybe Uuid
getEntityId cmd = Just cmd.cartId

type instance EntityOf AddItem = CartEntity
type instance TransportsOf AddItem = '[WebTransport]

command ''AddItem
```

The `command` marker comes from `Service.CommandExecutor.TH`. Put it after the declarations it connects. It generates the mechanical instances, including JSON handling and the command instance. New code should let the marker own that boilerplate; older examples sometimes spell it out.

The Cart service registers `AddItem` with `Service.command @AddItem`, and the application registers the service. A type sitting in a file is not yet an exposed feature. For HTTP commands the type name becomes a kebab-case URL: `AddItem` is `/commands/add-item`.

Events have a marker too: `event ''YourEvent` from `Service.Event.TH` generates serialization and deriving instances. You still define how domain events identify their entity and how an entity applies them. See [entities and state](/build/entities-and-state/).

## Make the agent explain its rule

Ask: “What happens for quantities minus one, zero, and one? What happens if the cart does not exist?” You should be able to answer from the decision itself before seeing test results.

The executor coordinates reading state and recording accepted events. Its concurrency machinery cannot decide whether your business rule is sensible. It also does not turn two different commands into one atomic operation; a checkout spanning Cart and Stock is one example of that boundary.

## Exercise: a purchase limit

For this exercise, choose a limit of six mugs **per cart**. Your agent adds `if cmd.quantity > 6` and says the feature is complete. What case has it missed?

<details>
<summary>Suggested reasoning and evidence</summary>

Two additions of four each satisfy the proposed check but total eight. Decide whether the limit covers one product or every product, then check existing quantities plus the request. Evidence should include a normal addition, an addition taking the total over six, exactly six, and a second addition after reaching six. Verify the rejected operation adds no successful `ItemAdded` event. This is an exercise extension, not behaviour already implemented by the testbed.

</details>

Public sources: [AddItem](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Commands/AddItem.hs), [service registration](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Service.hs), [command marker](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/CommandExecutor/TH.hs), [event marker](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Event/TH.hs).
