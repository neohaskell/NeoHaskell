---
title: "Explore your application visually"
description: "Use the Neo IDE model to discuss behaviour and connect it with the code."
sidebar:
  order: 1
---

You have created a cart and read its summary in the [first cart
exercise](/build/first-cart/). Now follow that same behaviour through the Neo IDE:
the request you sent, the fact it recorded, and the information you read back.
Keep working in `mug-shop`, the project you created during setup.

Use it alongside behaviour checks. A coherent graph helps you understand a change;
it does not prove that the application executes the intended policy.

## See the model before opening the code

![The Neo IDE in mug-shop shows the blue CreateCart command, orange CartCreated event, and green CartSummary query connected in the first cart feature.](/screenshots/neo-ide-overview.png)

*Your first cart feature: creation, the recorded fact, and the summary.
Select either screenshot to enlarge it without leaving this page.*

Read the picture in three passes:

1. **Find a request.** The blue `CreateCart` card represents asking for a cart.
   Follow its downward arrow to the orange `CartCreated` fact.
2. **Find the information someone sees.** The green `CartSummary` card receives
   information from `CartCreated`. Its fields include `itemCount`
   and `isEmpty`. The IDE calls these green cards **queries**; they represent the
   read-model side of the application.
3. **Find a place to work.** The left panel lists chapters and slices. Here,
   `CreateCart` and `CartSummary` divide the Cart chapter into small
   pieces you can discuss with your agent.

Compare it with the [worked event models](/start/event-modeling/). Those drawings
follow one example over time, including successive screens and concrete values.
The IDE graph shows reusable commands, event types, and queries: one `CartSummary`
card can describe the view after many different histories. Its arrows describe
relationships in the model, rather than a live trace of requests executing.

## Continue in your cart project

If you already opened the IDE during the cart exercise, keep that window open.
Otherwise, run this from the directory containing your cart application's `src/`:

```sh
neo ide
```

Use the `mug-shop` directory you created during setup. Below, you will synchronise
its model with the cart code you added under `src/Shop/Cart/`.

Open the printed address, normally `http://127.0.0.1:2323`. The IDE binds to the
project directory from which you launched it. Confirm the workspace shown in the
connection status before editing. The IDE and the application's HTTP server are
separate processes with separate ports.

The model lives in `event-model.json` at the project root. A fresh project may not
have one yet. Create a model using the canvas controls and make a small edit so
that autosave writes it. On a fresh workspace, check that you are not looking at
an old browser-local model; **New** starts an empty model, and **Open** reads the
current workspace file.

Changes autosave when connected. Wait for the save status before closing, and use
Git to keep meaningful versions. Cmd/Ctrl-S flushes pending autosave; it does not
create a Git commit.

## Connect the picture with source

Once the workspace has a model file, run this from another terminal in the same
project:

```sh
neo inspect
neo inspect sync
neo validate
```

`neo inspect` reports the discovered domain structure. `neo inspect sync` updates
the model from the source; it writes the model file. `neo validate` checks its
schema and references without modifying it. After CLI synchronization, click
**Open** in the IDE (or reload) to read the saved model; a CLI write alone does not
broadcast a refresh through the source-file watcher. A missing model file is a validation
failure, so create and save the model first.

The IDE also watches source changes and attempts the same synchronization. A field
change on an existing node is designed to preserve layout; newly discovered
structure can trigger a broader layout update. Inspect the result after a large
refactor.

Synchronization currently runs **from code into the model**. Editing the picture
does not generate the corresponding application code. Discuss your desired model
with the coding agent, let it change the source, and compare the refreshed picture
with your intent.

## Read a small piece of behaviour

Find `CreateCart` and follow it to `CartCreated`, then to `CartSummary`. Connect
those names to the request you sent and the empty-cart summary you observed in
the previous exercise. Ask your agent to show you where the cart identifier
travels through that path.

## Return here as the cart grows

After adding `AddItem` in [commands and events](/build/commands-and-events/),
synchronise the same project again and select that command. What information
does it need, and what would it record if accepted? The closer view below shows
what you will be able to inspect then.

![Selecting the blue AddItem command highlights its arrow to the orange ItemAdded event. AddItem carries cartId, stockId, and quantity; ItemAdded carries entityId, stockId, and quantity. Other nodes are dimmed.](/screenshots/neo-ide-detail.png)

*Selecting `AddItem` highlights its connection to `ItemAdded`. Zooming in makes
the fields readable; the surrounding model stays visible for context.*

Here you can follow a specific piece of information: `quantity` enters the command
and appears on the accepted event. The requested `cartId` identifies the cart whose
event carries `entityId`. Ask your agent to show you the corresponding rule in the
source and demonstrate that zero is refused. Seeing a field named `quantity` tells
you what information travels; it does not tell you which values the rule accepts.

Use the **+** and **−** canvas controls to change zoom. Selecting a node highlights
its connections. If the picture is outside the visible area, try **Fit View**;
reopening or reloading the saved model also restores a useful starting frame.

When an application model grows, use its features and chapters to focus on one behaviour.
Follow connections across a boundary when investigating another domain or an external
provider. **Tidy by flow** adjusts the presentation; it does not fix business rules.
The Problems panel helps locate model validation issues.

**Heal with AI** is an optional model-repair action that invokes the configured
Claude CLI flow. Ordinary viewing, inspection, validation, and deterministic sync
do not require using that action. Review its changes as you would another agent's
proposal; it is not a proof of runtime behaviour.

## Know the current scope

The Model canvas is implemented. The Schema, Logs, and Emulate lenses currently
show placeholders for future work. Use the actual application logs and tests for
[operational evidence](/operate/observability/).

Keep the default loopback binding for local work. Binding with `--host 0.0.0.0`
exposes the IDE on other network interfaces; the local workspace tools should not
be treated as a production customer interface.

## Your check

For the feature you have now, predict what a second `CreateCart` request will do.
Send it and find both identifiers in the summary. The graph still has one
`CreateCart` node: it describes a kind of request, not each occurrence. Ask your
agent to locate the code that chooses a fresh identifier, then follow that field
from the command to the event and query.

After the next lesson, return and check the zero-quantity refusal too. The
`AddItem` card names the request; its decision function determines which values
are accepted.

Next: [commands and events](/build/commands-and-events/) explains the decision
behind these connections. For individual commands, consult the [CLI reference](/reference/cli/).

Implementation evidence: [IDE server](https://github.com/neohaskell/NeoHaskell/blob/main/neo/src/commands/ide.rs),
[source synchronization](https://github.com/neohaskell/NeoHaskell/blob/main/neo/src/ide/sync.rs),
and [current lenses](https://github.com/neohaskell/NeoHaskell/blob/main/neo/assets/ide/src/ui/lenses/lenses.tsx).
