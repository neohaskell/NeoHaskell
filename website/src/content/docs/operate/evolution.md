---
title: Evolve an application without rewriting its past
description: Separate business changes, historical event compatibility, and query evolution.
sidebar:
  order: 5
---

Requirements change while existing data retains its meaning. New code must support both realities. For example, adding optional text to new records should not pretend that earlier users supplied it. In the ecommerce practice project, try adding an optional note to newly created carts while older carts retain an explicit absence of that information.

An event-sourced system makes history explicit. That is valuable for explaining decisions, and it creates a responsibility: future code must still understand the facts you already accepted.

## Classify the change before coding

| Change | Main question |
| --- | --- |
| New rule for future operations | Can existing state still be reconstructed before the new decision runs? |
| New event kind | Can old and new application revisions read the histories they may encounter? |
| Change to a stored event shape | How will historical JSON remain decodable and meaningful? |
| New or changed query | Can it rebuild correctly from all supported histories? |
| New external workflow | Could rollout or replay duplicate an external action? |

Use [event modeling](/start/event-modeling/) and the [visual IDE](/getting-started/visual-ide/) to identify the affected consumers. Changing one event can affect commands, queries, and integrations in different ways.

## Preserve a historical fixture

Before accepting a change, keep a small representative history in your tests. Include behaviour from before and after the change, plus a rejected operation. For the ecommerce example, use an old cart and a new cart with a note. Check that the new code:

- Decodes old and new persisted events.
- Reconstructs the intended state for each.
- Applies the new rule only where the business intends it.
- Builds the expected query results after replay.
- Does not repeat an external effect during the exercise.

A JSON type compiling successfully does not prove that historical JSON decodes successfully. Keep the historical cases beside your `Shop.Cart` tests and run `neo test` from `mug-shop` against a disposable database. Follow [testing](/build/testing/) for executable evidence.

## Use domain locking as a reminder

From a generated project's root, the CLI can lock discovered domain files:

```sh
neo lock --all
neo lock install
neo lock check
```

Before locking, inspect `git status` and clear unrelated staged work. Locking stages the selected files and `.locked-files` and creates a Git commit; already staged content can be included. `neo lock install` writes the pre-commit hook path, so preserve and integrate any existing hook deliberately.

The manifest is `.locked-files`; the installed Git hook and `neo build` check for changes to locked paths. `neo lock check` includes staged, unstaged, and untracked modifications. Locking helps make a consequential edit deliberate. It does not establish schema compatibility or provide a migration implementation.

`neo build --skip-lock-check` exists for an intentional build-time bypass. Treat the underlying historical compatibility question separately; bypassing the check does not answer it.

## Do not assume query changes migrate themselves

The Postgres query store and subscriber expose hash/checkpoint operations. Their existence does not mean every change to a query function is detected or migrated automatically, nor that standard application startup enables checkpoint resume. Plan and test a rebuild using your actual application wiring.

For an incompatible persisted query schema, make the transition explicit. Keep the authoritative event history intact and rehearse the transition on a restored copy. Include whether the previous revision can run against the resulting data; “roll back the binary” is not always sufficient after a data change.

## Exercise: add cart notes

For the ecommerce practice project, ask your agent to propose how new carts acquire an optional note and what old carts display. Before accepting code, explain the old-history behaviour yourself.

<details>
<summary>A useful acceptance boundary</summary>

An old cart should retain its original meaning, with an explicit absence of a note. A new cart with a note should retain it across restart and replay. A note that violates your size or content policy should be rejected before it becomes an accepted fact. Choose the exact policy for this exercise before checking the implementation.

</details>

Continue with [security](/operate/security/) or branch into [contributing to NeoHaskell](/operate/contributing/).
