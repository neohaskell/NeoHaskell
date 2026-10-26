### Investigation

**You own the answer. Plan, route, write.**

Investigation requests are read-only. They produce a cited explanation or a recommendation, not a code change.

1. Route through the **how** skill. For motivation questions, also route through the **why** skill.
2. Throughput checkpoint stays one line: `throughput checkpoint: n/a, read-only investigation`.
3. For mechanisms, produce the `how`-shaped output (Overview / Key Concepts / How It Works / Where Things Live / Gotchas). For motivation, preserve `why`'s evidence, sources and confidence structure. For a decision between alternatives, give a recommendation with a tradeoffs table.
4. Apply the **prose-cleanup** skill within the routed skill's presentation rules: light edits only, without substantially rewriting the explanation or changing confidence language.

No PR, no babysit, no `architect` unless the investigation precedes a code change. If it does, hand back to the user and re-route to Bug fix or Feature.

**Reply:** the investigation output. For "are we sure?" answers, include your real judgment with reasons. Push back if the premise is wrong (see `AGENTS.md`).
