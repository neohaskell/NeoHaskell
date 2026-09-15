---
title: "Find your path"
description: "Follow the shop journey, evaluate NeoHaskell, or look up one specific problem."
sidebar:
  order: 0
---

You do not need to learn a framework's internals before deciding whether it helps
your business. You also should not need to reread an introduction every time you
forget how to configure a service. These docs support both moments.

## Evaluate before building

Read [why NeoHaskell](/start/why-neohaskell/), [fit and tradeoffs](/start/fit-and-tradeoffs/),
and [the shop on paper](/start/a-shop-on-paper/). Continue through the opening
sections of the build, connect, and operate pages. They explain the decisions and
consequences before introducing code. The advanced pages still begin with a
recognisable business problem.

You can finish this path able to discuss the benefits, implementation effort,
operational responsibilities, and limits with your team. You do not need to run
an example to understand those choices.

## Build with your agent

The main route is:

1. [Describe the shop](/start/event-modeling/) and [agree how to work](/start/trusting-your-agent/).
2. [Set up a project](/getting-started/) and [explore it visually](/getting-started/visual-ide/).
3. [Build the shop](/build/) using the public cart and stock examples.
4. [Connect it](/connect/) to external systems and add carefully bounded AI features.
5. [Operate and evolve it](/operate/) with persistence, deployment checks, and recovery.

The first examples supply decisions and checks. Later exercises ask you to make a
choice, explain its consequence, or correct an agent's proposal. Optional suggested
reasoning helps you assess your answer. An agent can write the implementation;
you remain responsible for deciding what success means.

## Recognise what an example promises

A **public example** identifies code or a test in this repository. A **partial
snippet** teaches one part of an implementation and needs the surrounding module
and application wiring. A **design exercise** describes behaviour for you to build.
Do not assume every scenario in the shop story is already implemented in the
starter or reference application.

The starter is a small counter. The reference application contains cart and stock
examples. We keep that distinction visible so you can run what exists, understand
it, and use the same concepts in your own shop.

For direct answers, use the [capability guide](/reference/capabilities/),
[CLI reference](/reference/cli/), [glossary](/reference/glossary/), or
[troubleshooting guide](/reference/troubleshooting/). Contribution is a
[separate branch](/operate/contributing/) you can take when it becomes useful.
