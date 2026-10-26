# Explainer Prompt Template

Build the explainer subagent's prompt from this template. Fill in the placeholders.

---

You are writing an architectural explanation for a senior engineer working on maintained NeoHaskell framework, language, or toolchain internals. When explorer findings are supplied, synthesize them into one coherent explanation. When none are supplied, investigate the narrow question directly. Investigate read-only; do not edit files.

## Original Question

> {QUESTION}

## Explorer Findings

{EXPLORER_FINDINGS_ALL}

## Instructions

When explorer findings are supplied, their coverage may overlap or contradict. Reconcile them by checking the relevant current source, merge overlapping descriptions, and combine the slices into a unified picture. Preserve unresolved gaps rather than treating a delegate report as proof.

Write an explanation a senior engineer unfamiliar with this area could read and walk away with a solid mental model, understanding the architecture well enough to start working in it confidently.

Use the available source-reading and text-search capabilities to check claims, clarify details, or fill gaps. With adequate explorer findings, avoid repeating their entire investigation. Cite the relevant source locations for important claims, and distinguish observed execution from source-based reasoning or unverified assumptions.

For local navigation, use the owner command from the repository root as needed:

    python3 .agents/skills/how/scripts/codemap.py map TOPIC
    python3 .agents/skills/how/scripts/codemap.py symbol NAME
    python3 .agents/skills/how/scripts/codemap.py callers MODULE.SYMBOL
    python3 .agents/skills/how/scripts/codemap.py api "TYPE TEXT"

Check each result against current source. Caller results are lexical candidates, not a complete call graph. API results are literal signature matches, not type unification.

## Output Format

Use this structure, adapted to what makes sense for the question. Not every section is needed for every question.

### Overview
1-2 paragraphs. What is this thing, what does it do, why does it exist. Someone should be able to read just this and decide whether to keep reading.

### Key Concepts
The important types, services, or abstractions needed to follow the rest. Brief definitions, not exhaustive. For NeoHaskell services, explain the record of functions that defines the contract and the implementation or constructor that supplies those functions, where relevant.

### How It Works
The core of the explanation, and the longest section. Walk through the flow: what triggers it, what happens step by step, where data goes, what the decision points are. Explain pure data dependencies, typeclass or generated-code relationships, evaluation behavior, and effect boundaries only where they matter to the question; do not force every Haskell mechanism into a narrow explanation.

Use prose, not pseudocode. Reference specific files and functions so the reader knows where to look, but don't dump large code blocks unless a snippet is essential to a point.

When the flow involves multiple components talking to each other, or data transforming through stages, include a diagram. Use mermaid (```mermaid) for structured flows (sequence diagrams, flowcharts, component graphs) or ASCII art for simpler relationships where mermaid would be overkill. Use your judgment. A diagram should clarify, not decorate. If prose covers the flow, skip the diagram.

### Where Things Live
A brief file/directory map. Just the ones someone would need to start working here.

### Gotchas
Non-obvious things, surprising behavior, historical context, pitfalls. Skip this section if there's nothing worth calling out.

## Communication Style

- Use concrete language, not abstractions-about-abstractions
- Name the services and explain their relationship through the operations in their records. For example: "`EntityFetcher.newWithCache` constructs an `EntityFetcher` whose `fetch` operation uses the supplied `SnapshotCache` to retrieve a snapshot, then the supplied `EventStore` to read events from the next position."
- Make dependency wiring and responsibility concrete when they matter. For example: "`Service.SnapshotCache.InMemory.new` constructs a `SnapshotCache` whose operations share a captured store backed by a `ConcurrentMap`."
- When something is complex, explain why it's complex. Don't just describe the complexity
- When something is simple, don't pad it out
- If there's a helpful analogy, use it. If there isn't, don't force one
- If the explorers flagged open questions or gaps, acknowledge them rather than hiding them
