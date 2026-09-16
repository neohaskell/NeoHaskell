---
title: Language essentials
description: Read the NeoHaskell vocabulary your agent uses, one practical concept at a time.
sidebar:
  order: 11
---

You can direct an application's behaviour without memorising a language manual. Still, a small vocabulary lets you inspect an agent's proposal and ask sharper questions: Can this value be absent? Can this operation fail? Does this function make a decision or perform an external action?

Use this page as a reading companion. The examples draw on the ecommerce practice project and common data-handling tasks. They are small expressions or partial functions for a configured NeoHaskell module, not a second application to install.

## Read a pipeline from left to right

NeoHaskell commonly passes a value through named steps using `|>`:

```haskell
cart.items |> Array.length
```

That means “take the cart's entries, then count them.” You used this expression in `src/Shop/Cart/Queries/CartSummary.hs`. Qualified names such as `Array.length` tell you which module supplies the operation.

`Core` supplies the default vocabulary. These teaching excerpts omit module headers and import lists; the downloadable checkpoints contain the complete files. Neo manages the language settings, so application files do not need language pragmas. A record holds named fields; `cart.ownerId` reads one, and `cart {ownerId = newOwner}` produces an updated record value. Producing that value alone does not persist an event.

## Distinguish absence, failure, and work

| Type | Question it makes explicit | Example |
| --- | --- | --- |
| `Maybe value` | Is a value present? | A cart lookup can produce `Nothing` or `Just cart`. |
| `Result error value` | Did a calculation succeed? | JSON decoding can produce `Err message` or `Ok value`. |
| `Task error value` | What work will produce a result, and how can it fail? | Reading a file or making an HTTP request. |
| `Decision event` | Which business facts should be accepted? | Accept item addition or reject its quantity. |

`case` names the possibilities you handle. This **adapted partial helper** shows a pure input check:

```haskell
validateQuantity :: Int -> Result Text Int
validateQuantity quantity =
  if quantity > 0
    then Ok quantity
    else Err "Quantity must be positive"
```

`Task.yield` produces a successful task result; `Task.throw` produces a task error. `Task.mapError` translates an error type at a boundary, and `Task.asResult` lets you inspect an error as a value. Do not replace a meaningful failure with a default merely to make code continue: the caller may need to know the action did not complete.

`do` sequences steps, `<-` receives the result of a step, and `let` names a local value. The application's orchestration can therefore read top to bottom without putting every branch into one large function.

## Collections and identifiers

Use `Array` for ordered values and `Map` for values keyed by an identifier. `Array.map` transforms entries; `Array.takeIf` retains matching entries; `Array.reduce` combines entries into a result. `Map.get` returns `Maybe` because a key can be absent.

`Uuid.fromText` also returns `Maybe`: text from a URL is not guaranteed to be a valid identifier. An ID parsing successfully establishes its format, not that the caller may access the corresponding record.

`Text` is the usual string type. Formatting such as `[fmt|Cart #{cartId}|]` keeps interpolation readable. Use named domain types and record fields so the agent's implementation preserves the distinctions in your event model.

## Amounts and money

Numeric values need defined units, bounds, and rounding rules. Money in the practice project gives us a useful example: a price needs both an amount and a currency. The `Decimal` type supplies fixed-point storage with four decimal places. It serializes to JSON as a string, such as `"12.5000"`.

These **source-grounded expressions** illustrate construction and formatting:

```haskell
Decimal.fromCents 1250 |> Decimal.formatDecimal
-- "12.5000"

Decimal.divide (Decimal.fromCents 1250) Decimal.zero
-- Nothing
```

For a currency with two minor-unit decimal places, `fromCents` constructs from integer minor units. `toCents` truncates finer precision; it is not an implicit rounding policy. `roundTo2` is an explicit operation, and division returns `Maybe` because division by zero has no value.

The implementation stores an `Int64`, so amounts need bounds. Its text parser currently passes through a floating-point conversion; do not describe it as an arbitrary-precision financial parser. Prefer validated integer minor units where they fit your currency policy, and test rounding and maximum amounts. `Decimal` does not attach a currency or decide tax rules for you.

## What traits and markers contribute

A trait describes behaviour a type supports. `Mappable` permits transforming contained values, `Default` supplies a starting value, and serialization traits connect values with JSON. A constraint in a function signature tells you which behaviour it needs.

The helpers `deriveEvent`, `deriveCommand`, `deriveEntity`, `deriveQuery`, and `deriveOutboundIntegration` come from `Core` and generate common instances. They reduce repetitive wiring, while the decision, state update, and projection remain business logic you can inspect. See [commands and events](/build/commands-and-events/) for their declaration order and [queries](/build/queries/#derive-and-register-the-view) for the query-specific order.

## Exercise: review a “safe” helper

In the practice project, an agent parses an invalid quantity and substitutes one so the request always succeeds. Explain why that might violate the requested behaviour, then propose an observable check.

<details>
<summary>Suggested reasoning and checks</summary>

One mug is a valid quantity but may not be what the customer requested. Preserve invalid input as an error, and let the interface ask for correction. Verify a normal positive value, malformed input, zero, and the largest quantity the exercise's policy permits. Test any conversion to smaller numeric or monetary representations at their limits too. Type checking helps distinguish categories; it does not select acceptable business defaults.

</details>

Next: [integrations](/connect/) applies these ideas to work outside the core model. Return to the [build overview](/build/) for the learning sequence.

Public sources: [Result](https://github.com/neohaskell/NeoHaskell/blob/main/core/core/Result.hs), [Task](https://github.com/neohaskell/NeoHaskell/blob/main/core/core/Task.hs), [Decimal](https://github.com/neohaskell/NeoHaskell/blob/main/core/decimal/Decimal.hs), [Mappable](https://github.com/neohaskell/NeoHaskell/blob/main/core/traits/Mappable.hs).
