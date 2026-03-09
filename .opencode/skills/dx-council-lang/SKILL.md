---
name: dx-council-lang
description: "Use when user says 'ask the council' or requests expert opinions on language design, syntax choices, DX tradeoffs, or ergonomics questions for NeoHaskell. Spawns parallel agents impersonating 19 real-world language DX experts who research their person's actual positions and reason from that perspective. Produces consensus with dissent."
---

# DX Council — Language Design Expert Panel

Summon 19 language design experts as parallel agents. Each researches their person's real opinions via web search and reasons from that expert's perspective. Results are synthesized into a consensus recommendation.

## Trigger

User says "ask the council" (or variants: "consult the council", "what would the council say", "council opinion on") followed by a language design question.

## Expert Roster

Read `references/experts.md` for full profiles. Brief roster:

| # | Expert | Lens | Community |
|---|--------|------|-----------|
| 1 | Matz | Developer happiness | Ruby |
| 2 | DHH | Convention over config | Rails |
| 3 | Bob Nystrom | Function coloring | Dart/PL |
| 4 | matklad | Syntax vs semantics | Rust/Zig |
| 5 | Steve Klabnik | Strangeness budget | Rust |
| 6 | Fernando Borretti | Language pragmatics | Austral |
| 7 | Scott Wlaschin | FP for enterprise | F# |
| 8 | withoutboats | Essential complexity | Rust |
| 9 | Evan Czaplicki | Radical simplicity | Elm |
| 10 | Rich Hickey | Simple vs easy | Clojure |
| 11 | Hillel Wayne | Empirical evaluation | Cross |
| 12 | Aaron Turon | Ergonomics framework | Rust |
| 13 | Richard Feldman | Teaching-informed | Roc/Elm |
| 14 | Felienne Hermans | Cognitive science | Education |
| 15 | Don Syme | Anti-max-abstraction | F# |
| 16 | Gary Bernhardt | Consistency as trust | Ruby/JS |
| 17 | Alexis King | Type ergonomics | Haskell |
| 18 | Sandi Metz | Craft and constraints | Ruby |
| 19 | Andrey Breslav | Pragmatic interop | Kotlin |

## Orchestration Protocol

### Step 0: Read Expert Profiles

Read `references/experts.md` in full. You need each expert's philosophy, known positions, search strategy, and voice to construct accurate agent prompts.

### Step 1: Formulate the Question

Take the user's question and restate it precisely:

```
COUNCIL QUESTION: [Restate the exact design question]
CONTEXT: [Relevant NeoHaskell decisions already made - read from design/syntax.md]
OPTIONS: [If the user presented options, list them. If not, identify the option space.]
```

### Step 2: Select Relevant Experts

Not all 19 experts need to weigh in on every question. Use the Relevance Matrix at the bottom of `references/experts.md` to select 8-12 experts most relevant to the specific question type.

**Always include:** Klabnik (strangeness budget), Nystrom (function coloring), Czaplicki (simplicity) - these three lenses apply to nearly every NeoHaskell decision.

### Step 3: Spawn Expert Agents in Parallel

Spawn ALL selected experts as parallel background librarian agents. Each agent gets the following prompt template (fill in brackets from `references/experts.md`):

```
PERSONA: You are impersonating [EXPERT_NAME], [ROLE].
Your philosophy: [PHILOSOPHY from experts.md]
Your known positions: [KNOWN_POSITIONS from experts.md]
Your voice: [VOICE from experts.md]

TASK: A new programming language called NeoHaskell is being designed. It transpiles
to Haskell, targets Java/C#/JS developers, uses Elm as a baseline, adds braces
instead of indentation, uses trait/impl instead of typeclass/instance, and has
let! for monadic bind (F# style).

The design team asks: [THE COUNCIL QUESTION]

Context: [NEOHASKELL CONTEXT]

Options being considered: [OPTIONS]

INSTRUCTIONS:
1. SEARCH the web for [EXPERT_NAME]'s actual writings and opinions relevant to this
   question. Search: [SEARCH_STRATEGY from experts.md]
   Also search: "[EXPERT_NAME]" + [keywords from the question]
2. Based on their REAL positions (from search results + known positions above),
   construct their likely response.
3. DO NOT invent positions. If you cannot find relevant opinions, extrapolate ONLY
   from confirmed positions.
4. Stay in character. Match their voice and reasoning style.

RESPONSE FORMAT (use exactly this structure):

**[EXPERT_NAME]'s verdict:** [SUPPORT / OPPOSE / CONDITIONAL / ABSTAIN]

**Position:** [2-4 sentences in their voice, stating their opinion on the specific question]

**Reasoning:**
- [bullet 1 explaining WHY, referencing their known writings/philosophy]
- [bullet 2]
- [bullet 3]

**Concern:** [1-2 sentences on what they'd worry about with the proposed design]

**Evidence:** [Links or references to their actual writings that support this position]
```

Launch each agent:
```
task(
  subagent_type="librarian",
  run_in_background=true,
  load_skills=[],
  description="Council: [EXPERT_NAME] on [SHORT_QUESTION]",
  prompt=[FILLED PROMPT ABOVE]
)
```

### Step 4: Collect All Results

After spawning all agents, collect via `background_output(task_id=...)` for each.

If any agent fails or times out, note their absence. Do not retry. Proceed with available responses.

### Step 5: Synthesize Consensus

After collecting all expert responses, produce the synthesis in this exact format:

---

## Council Verdict: [QUESTION SUMMARY]

### Quick Tally

| Verdict | Experts |
|---------|---------|
| SUPPORT | [names] |
| OPPOSE | [names] |
| CONDITIONAL | [names] |
| ABSTAIN | [names] |

### Consensus Position

[2-3 paragraphs synthesizing the majority view. What do most experts agree on?
Where is the common ground? What is the recommended path?]

### Key Arguments FOR

[Bullet points - the strongest arguments from supporting experts, attributed]

### Key Arguments AGAINST

[Bullet points - the strongest arguments from opposing experts, attributed]

### Conditions and Caveats

[What the CONDITIONAL experts said - "this works IF..." statements]

### Notable Dissent

[Any expert whose position was particularly surprising or worth highlighting,
with their reasoning. Dissent is valuable signal.]

### Strangeness Budget Impact (Klabnik)

[Specifically call out how this decision spends or saves the strangeness budget]

### Function Coloring Impact (Nystrom)

[Specifically call out if this creates or avoids function coloring problems]

### Pragmatics Check (Borretti)

[Will developers ACTUALLY use this the way we intend? Or will they find shortcuts?]

### Final Recommendation

**The council recommends:** [CLEAR RECOMMENDATION]
**Confidence:** [HIGH / MEDIUM / LOW] - based on degree of expert agreement
**Dissent strength:** [STRONG / MILD / NONE] - how forcefully dissenters opposed

---

### Step 6: Present to User

Present the full synthesis. Wait for the user to accept, reject, or modify before updating any design documents.

## Expert Grouping Strategy

When spawning agents, use these groups to maximize parallelism:

**Wave 1 - Core Panel (always spawn):**
- Klabnik (strangeness budget)
- Nystrom (function coloring)
- Czaplicki (simplicity)
- matklad (syntax analysis)
- Wlaschin (FP teaching)

**Wave 2 - Depth Panel (spawn based on relevance):**
- Hickey (philosophy)
- Syme (type system)
- Feldman (teaching)
- Borretti (pragmatics)

**Wave 3 - Breadth Panel (spawn based on relevance):**
- Hermans (cognitive science)
- Wayne (empirical)
- King (type ergonomics)
- withoutboats (complexity)

**Wave 4 - Practitioner Panel (spawn based on relevance):**
- Matz (happiness)
- DHH (convention)
- Bernhardt (consistency)
- Metz (craft)
- Breslav (interop)
- Turon (framework)

Spawn all waves simultaneously (all background). Collect as they complete.

## Rapid Mode

If user says "quick council" or the question is narrow, spawn only Wave 1 (5 experts) for a fast verdict.

## Anti-Patterns

- DO NOT skip the web search. Each agent MUST search for the real expert's opinions.
- DO NOT let agents agree with each other. Each reasons independently from their persona.
- DO NOT suppress dissent. Disagreement is signal, not noise.
- DO NOT present consensus as unanimous unless it actually is.
- DO NOT update design documents before the user validates the council's recommendation.
- DO NOT impersonate experts on topics they have never discussed. Use ABSTAIN.
