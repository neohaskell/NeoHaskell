---
title: "Review your application"
description: Check the project you have built and choose the next meaningful promise to implement.
sidebar:
  order: 10
---

A useful milestone is a moment when you can explain your application's behaviour and show evidence for it. You now have one project containing Cart and Stock, explicit decisions, read models, and tests. Before adding another capability, check that these parts tell the same story.

This is your `mug-shop` project from `neo new`. There is no transfer into a second workspace. The next sections continue from the files you have authored here.

## Check the shape of your project

Your domain files should now include:

```text
src/
  App.hs
  Shop/
    Config.hs
    Cart/
      Core.hs
      Entity.hs
      Event.hs
      Item.hs
      Events/CartCreated.hs
      Events/ItemAdded.hs
      Service.hs
      Commands/CreateCart.hs
      Commands/AddItem.hs
      Queries/CartSummary.hs
    Stock/
      Core.hs
      Entity.hs
      Event.hs
      Events/StockInitialized.hs
      Events/StockReserved.hs
      Service.hs
      Commands/InitializeStock.hs
      Commands/ReserveStock.hs
      Queries/StockLevel.hs
tests/
  Spec.hs
  Decider/Cart/CreateCartSpec.hs
  Decider/Cart/AddItemSpec.hs
  Decider/Cart/ReplaySpec.hs
  Decider/Stock/ReserveStockSpec.hs
  scenarios/create-cart.hurl
  scenarios/cart-flow.hurl
  scenarios/stock-flow.hurl
```

Your generated launcher and `neo.json` remain part of the project. `neo` discovers source and test modules and maintains generated build artifacts. Keep your project in version control so a later change has a clear comparison point.

## Run the evidence

Stop a running development server, then execute:

```sh
neo build
neo test
```

The decision tests check accepted payloads, missing entities, zero quantity, and the last available stock unit. The replay check protects separate cart entries. HTTP checks create their own carts and wait for the matching summary.

Now run `neo run` and repeat the [stock lesson's requests](/build/stock-and-checkout/#create-and-inspect-stock). Create three units of stock, create a cart, and add two units. The cart should show one entry. Stock should still show three available and zero reserved, because the integration between the domains is the next lesson.

In another terminal rooted in this project, run `neo ide`. Follow the [model workflow](/getting-started/visual-ide/) to inspect the relationships. The graph helps locate code; the tests establish what the code does.

## Explain the result without implementation jargon

A reasonable explanation is:

> “The application records a person's selections and rejects invalid quantities. It can also decide whether stock can be reserved. We have tested those rules. We will now connect an accepted selection to the stock decision and show what happens when that second step fails.”

That explanation makes the next task concrete. It does not depend on pretending that a complete checkout already exists.

## Choose the next slice

| Milestone | Evidence to ask for |
| --- | --- |
| [Connect Cart and Stock](/connect/workflows/) | Accepted addition, reservation result, both views, and a refused reservation. |
| Order acceptance | Explicit accepted-order fact with agreed prices, currency, quantities, and delivery context. |
| Payment | Provider identity, duplicate handling, refusals, and reconciliation after a lost reply. |
| Notification | Accepted or failed delivery work without rewriting the underlying order. |
| [AI-assisted features](/connect/ai/) | Review before publication and protection against late replies replacing newer work. |
| [Operate the application](/operate/) | Durable restart, restore evidence, authorised access, and smoke tests against the deployed revision. |

Some of these are worked integrations; others are deliberate design exercises. Choose one promise, describe acceptance and refusal, then implement and verify it before adding another.

## Exercise: define completion

Your agent says “checkout is done” because the request returned 200. Write the evidence you would need to accept that claim for your chosen checkout policy.

<details>
<summary>Suggested reasoning and checks</summary>

Name the fact that means the order was accepted. Identify which prices and customer details it fixes. Explain whether reservation and payment are prerequisites or later steps, and how each refusal is represented. Test an ordinary success, a partial failure, and a duplicate or delayed result. An HTTP acknowledgment proves only the particular command's acceptance.

</details>

Continue with [integrations](/connect/), using the same project and the same habit of making each promise visible.
