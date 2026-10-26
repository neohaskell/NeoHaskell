---
name: create-verification-skill
description: "Generate a project-local verification skill for NeoHaskell framework, language and toolchain contracts. Use for /create-verification-skill, \"make a control skill for this repo\", or when the project lacks scripted proof of library, compiler, CLI or runtime-service behavior."
---

# Create a verification skill

Generate a project-local skill (`.agents/skills/verify-<target>/`) that drives real NeoHaskell framework and language entrypoints and captures proof. A consuming fixture exercises the framework contract under review. Write the output for the next agent: it will be read cold, mid-task, without this conversation.

Before creating a new skill, check the existing [test](../neohaskell-test/SKILL.md), [regression-evidence](../regression-evidence/SKILL.md), [event-store](../event-store-regression/SKILL.md), [consumer-verification](../neohaskell-consumer-verification/SKILL.md) and [benchmark](../framework-benchmark/SKILL.md) owners. Extend the relevant owner's resources for a demonstrated gap. Create a separate target skill only when it has a distinct verification contract. Validate authored metadata and links with [Validate skills](../validate-skills/SKILL.md).

## 1. Interview the repo, not the user

Answer these from the codebase and only ask the user what you cannot observe:

- **Surface:** what contract is being verified? A library or owning service API, generated declarations and instances, compiler/toolchain behavior, a CLI, command execution, storage or query delivery? Pick the primary surface and note the rest.
- **Run:** how is that target built, prepared and, where needed, started? Check current package/component declarations and implementation against documented commands. Record the toolchain, build configuration, fixture inputs and required runtime configuration. A library or compiler fixture need not have a server.
- **Drive:** reuse existing harnesses first: Cabal/Hspec/QuickCheck, consuming compiler fixtures, CLI tests, or testbed/Hurl recipes as relevant. Read what the selected fixture actually invokes and asserts. Only add a driver for a demonstrated gap; a suite name or test label alone does not establish coverage.
- **Observe:** what proves the claim? Compiler results and diagnostics, demanded pure results, executed decisions or Tasks, terminal output, response bodies, event streams, stored values and query state. Distinguish command acceptance, storage and downstream visibility.
- **Isolate:** can runs use separate fixture/build outputs, databases, ports and data directories? Verify actual runtime wiring. If isolation is unavailable, say so in the generated skill and refuse to double-drive shared state.

If the selected target doesn't build or run as-is, fix that first (or report it precisely) before generating; a skill written against a broken base teaches wrong steps. When an irrelevant missing asset blocks the selected target, the generated skill may create it, clearly marked as verification scaffolding, and remove it in cleanup. Keep intermediate drafts, run state and evidence under `.agents-state`.

## 2. Generate the skill

Write `.agents/skills/verify-<target>/SKILL.md` with YAML frontmatter (`name: verify-<target>` and a `description` naming the target, surface and trigger) and these sections, grounded in what the interview actually found, with no placeholders left:

- **Launch:** the exact commands to prepare/build the fixture and start any required process, with a usable-readiness predicate and teardown. A runtime target must work from owned empty state, including migrations and seed data. Process or database liveness alone is insufficient. For a library or compile-time target, establish the actual toolchain, consumer source and selected build/test entrypoint. Short-lived CLI drives use their own isolated sessions; do not invent a persistent server.
- **Doctor:** one read-only check that answers whether this target is worth driving: expected build/toolchain and fixture, correct configuration, and owned runtime state/process/port/auth when applicable. Run it first whenever anything looks off.
- **Drive:** the actual harness recipe with literal commands, inputs and stable entrypoints from this repo. Exercise the consumer or owning API for the claimed contract. Compilation must exercise the relevant imports, splices and instances; runtime claims must demand the result or execute the decision/Task. A complete command/storage claim must drive the real mapped path and selected backend. Use relevant deterministic examples or properties with expectations derived from the contract or independent evidence; avoid tautological assertions.
- **Evidence:** what to capture and where under `.agents-state`. Exercise the real entrypoint, capture the action and resulting state, and verify the claimed side effects alongside returned values. Do not substitute internal setters or test-only endpoints for the path being claimed. Separate compiler proof from runtime proof, and acceptance from storage and subscriber/query visibility. For indirect interactions, correlate captured identifiers and observe each required downstream outcome using bounded predicates. State unsupported delivery, durability or restart claims explicitly. Mocks belong only at an existing production boundary. When using a dry-run or test mode, observe what it actually skips through files, network or refs instead of trusting its name.
- **Cleanup:** tear down only instances and scratch state created by the run. Never kill by process name; kill what you started. Retain evidence outside the scratch subtree being removed, at the named location, and verify it survives teardown.
- **Helpers:** every shipped script is executable and its invocation appears in the skill body. A helper the reader has to reverse-engineer is not a helper.

## 3. Seed the feature map

Create `.agents/skills/verify-<target>/features/README.md` plus one file per relevant caller-visible contract (aim for the top 3-5 to start, from exports, generated APIs, commands, routes or docs). Follow [`references/feature-map-example/`](references/feature-map-example/): a README index and one file per feature. The examples cover generated APIs, a command-to-query path, and a deeper event-triggered internal command; select features relevant to the actual target rather than requiring every example for every change.

Each feature explains what the developer or caller can rely on, how to reach it, how to drive it and what observable result proves it. Keep the four H2s: `Sub-features`, `How to get to it (user POV)`, `Driving it with <harness>`, and `Gotchas`. Here, user POV means the developer/caller exercising the framework contract. The bundled examples illustrate the map shape; generated output must contain discovered literal recipes and no placeholders or invented wrappers.

If fixture state lacks a required precondition, report the journey as failed and put the reason in `Gotchas`. Never weaken the journey to make it pass. The map is the maintained verification source; one convenient entrypoint does not cover other mapped entrypoints.

## 4. Prove the generated skill before handing it over

Run its own instructions end to end once: launch, doctor, drive ONE mapped feature (one is enough; the map exists so later runs can cover the rest), capture evidence, clean up. After cleanup, confirm the evidence still exists at the named location — a cleanup that eats the proof fails this step. Fix what fails, and run the generated cleanup after every failed iteration too, so broken attempts don't strand processes and ports. A generated skill that was never executed is a draft, not a deliverable. If you proved it in a stand-in for the real target (a container in place of a fresh VM, a local stack in place of a remote one), list every setup step the stand-in did that the target will not. Those steps stay unverified until a run on the real target covers them.

## 5. Offer the maintenance loop

Point the user at `/maintain-verification-skill` for keeping the map honest as the framework and toolchain change. Suggest a cadence only if they ask.
