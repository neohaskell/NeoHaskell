---
title: "Learn to trust your coding agent"
description: "Delegate implementation while keeping ownership of intent and acceptance."
sidebar:
  order: 5
---

You should be able to ask an agent to change your application without reviewing every line
it writes. To do that confidently, you need a clear agreement about the behaviour
and a reliable way to inspect the result.

NeoHaskell's conventions, model, compiler, and tests contribute different kinds of
evidence. Learning what each establishes helps you decide when to accept a change
and when to ask another question.

## Agree on the behaviour first

Tell the agent the rule, who it affects, and examples at its boundaries. Ask it to
explain the proposed model before implementation. If its explanation is wrong,
correction is cheap and understandable. Here is that conversation for an order
quantity limit in the ecommerce practice project:

> **Jess:** Existing customers can still see orders placed under the old limit.
> Only new requests use the new limit.
>
> **Agent:** I will change the validation for new requests and leave the historical
> events intact. I will check a new request and reconstruct an older order.

This response identifies both the change and the evidence. “I updated the limit
and everything passes” would leave important questions unanswered.

## Know what the evidence tells you

| Evidence | Useful conclusion | What still needs judgment |
| --- | --- | --- |
| Model or IDE graph | The concepts and relationships can be inspected | Whether the policy matches your business |
| Successful compilation | The program satisfies the checked type relationships | Whether it makes the correct business decision |
| Passing tests | The asserted cases behave as expected in that environment | Missing cases and differences in production |
| External-provider sandbox check | That provider interaction worked in the tested setup | Live credentials, real failures, and duplicate effects |
| A successful rollout and smoke test | The tested revision serves the expected behaviour | Long-term operation and recovery |

Do not accept a screenshot of green tests without knowing what was asserted. You
can ask for the named scenarios and their outcomes without becoming the author of
every test.

## Use a repeatable conversation

1. Explain the situation and the rule in your own words.
2. Ask the agent to identify affected parts of the model and any ambiguity.
3. Agree on success, rejection, and boundary examples.
4. Let it implement and run the relevant checks.
5. Inspect the model and observable behaviour, including one variation you choose.
6. Ask what remains untested or depends on an external service.

When a test fails, first establish whether the behaviour or the expectation is
wrong. Removing the check removes evidence; it does not resolve the disagreement.

## Grow your independence

Early in the journey, you can use the supplied questions and examples. Later,
you will choose failure cases yourself, challenge an integration proposal, and
review a deployment plan. The [testing chapter](/build/testing/) provides the
mechanics behind those conversations.

Your goal is a trustworthy working relationship: implementation can be delegated,
while you can explain what your application promises and how you checked it. Continue to
[set up your first project](/getting-started/).
