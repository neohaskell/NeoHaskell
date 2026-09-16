---
title: "Find your path"
description: "Learn NeoHaskell step by step, evaluate it for your team, or look up one specific problem."
sidebar:
  order: 0
---

You do not need to learn a framework's internals before deciding whether it helps
your application. You also should not need to reread an introduction every time you
forget how to configure a service. These docs support both moments.

## Evaluate before building

Read [why NeoHaskell](/start/why-neohaskell/), [history and change](/start/history-and-change/),
[growing by slices](/start/growing-by-slices/), and [fit and tradeoffs](/start/fit-and-tradeoffs/).
These chapters build the explanation through familiar examples and diagrams before
introducing implementation. Continue through the opening
sections of the build, connect, and operate pages. They explain the decisions and
consequences before introducing code. The advanced pages still begin with a
recognisable application problem.

You can finish this path able to discuss the benefits, implementation effort,
operational responsibilities, and limits with your team. You do not need to run
an example to understand those choices.

The diagrams highlight one relationship at a time: a request and its outcome, a
history and its summary, or a feature and the contract it shares. Their captions
explain the same idea in words. Diagrams fit the width of your screen;
select one to enlarge it in place. Press Escape or select the close control to
return to the same point in the page.

## Build with your agent

The main route is:

1. [Describe application behaviour](/start/event-modeling/) and [agree how to work](/start/trusting-your-agent/).
2. [Set up a project](/getting-started/) and [explore it visually](/getting-started/visual-ide/).
3. [Build applications](/build/): learn commands, state, queries, and tests through cart and stock examples.
4. [Connect systems](/connect/) and add carefully bounded AI features.
5. [Operate and evolve your application](/operate/) with persistence, deployment checks, and recovery.

Ecommerce is the recurring example. The practice project starts small and grows as
you learn, so you can see how concepts fit together. General topic pages also work
on their own: you can learn about queries or permissions while building an entirely
different application.

The first examples supply decisions and checks. Later exercises ask you to make a
choice, explain its consequence, or correct an agent's proposal. Optional suggested
reasoning helps you assess your answer. An agent can write the implementation;
you remain responsible for deciding what success means.

## Recognise what an example promises

A **public example** identifies code or a test in this repository. A **partial
snippet** teaches one part of an implementation and needs the surrounding module
and application wiring. A **design exercise** describes behaviour for you to build.
Do not assume every feature of the practice project is already implemented in the
starter or reference application.

The starter is a small counter. The reference application contains cart and stock
examples. We keep that distinction visible so you can run what exists, understand
it, and use the same concepts in your own application.

For direct answers, use the [capability guide](/reference/capabilities/),
[CLI reference](/reference/cli/), [glossary](/reference/glossary/), or
[troubleshooting guide](/reference/troubleshooting/). Contribution is a
[separate branch](/operate/contributing/) you can take when it becomes useful.
