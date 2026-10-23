---
name: self-contained-skills
description: Keep authored or revised skill guidance durable with self-contained examples, owned operational dependencies, and separate run evidence. Apply to skill packages and their routing instructions.
---

# Keep skills self-contained

Write reusable instructions that remain useful when implementation code moves or disappears.

1. Teach with self-contained illustrative examples. Define the relevant data shape and behavior in the example. Do not cite implementation files, symbols, line numbers, or source links as examples. Label invented APIs as illustrative.
2. Keep operational dependencies explicit. Commands, executable configuration, maintained fixtures, and links to agent-facing skills, scripts, or resources may name their real owners. Keep a reference when the skill needs it to perform its task. Replace a reference that merely points to implementation code to explain an idea.
3. Put one-run reports, reviews, logs, transcripts, handoffs, and validation evidence under `.agents-state/<task>/`. Retain their source revision and paths when needed to interpret historical findings. Reusable skills must work without that local evidence.
4. Keep durable policy, generic scripts, fixtures, templates, and required configuration with their owning skill. Give scripts explicit inputs that work across runs. Put concrete cleanup queues and remaining work in the issue tracker. Keep observed findings, revision snapshots, and ticket state out of reusable skill resources.
5. Audit each changed instruction and resource. A reader must understand every example without source navigation. Every retained dependency must have an operational purpose. Remove snapshot claims and evidence links from reusable guidance, then run [Validate skills](../validate-skills/SKILL.md).
