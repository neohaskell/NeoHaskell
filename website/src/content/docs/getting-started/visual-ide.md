---
title: "Explore your application visually"
description: "Use the Neo IDE model to discuss behaviour and connect it with the code."
sidebar:
  order: 1
---

When your agent changes a rule, you need a way to see where it belongs in the
application. The Neo IDE provides an event-model canvas: commands, events, queries,
and integrations can be discussed as connected business concepts.

Use it alongside behaviour checks. A coherent graph helps you understand a change;
it does not prove that the application executes the intended policy.

## Open the right project

From the generated project directory, run:

```sh
neo ide
```

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

Start with the counter you ran in [setup](/getting-started/). Locate creation,
increment, the accepted events, and the view. Ask your agent to explain how those
parts correspond to the HTTP requests you sent. Then repeat the exercise with the
[cart example](/build/first-cart/).

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

Ask your agent to explain an increment of zero using the graph and the command's
rule. Can you distinguish where the request is represented from where the refusal
is decided? A model connection alone cannot answer whether zero is allowed.

Next: [build application behaviour](/build/), or consult the [CLI reference](/reference/cli/).

Implementation evidence: [IDE server](https://github.com/neohaskell/NeoHaskell/blob/main/neo/src/commands/ide.rs),
[source synchronization](https://github.com/neohaskell/NeoHaskell/blob/main/neo/src/ide/sync.rs),
and [current lenses](https://github.com/neohaskell/NeoHaskell/blob/main/neo/assets/ide/src/ui/lenses/lenses.tsx).
