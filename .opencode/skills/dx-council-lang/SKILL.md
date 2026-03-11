---
name: dx-council-lang
description: "Use when user says 'ask the council' or requests expert opinions on language design, syntax choices, DX tradeoffs, or ergonomics questions for NeoHaskell. Dynamically selects the 3 most relevant experts from a 19-person roster, spawns them as parallel agents who research their person's actual positions and reason from that perspective. Produces consensus with dissent."
---

# DX Council — Language Design Expert Panel

Select the 3 most relevant experts from a 19-person roster based on the question, spawn them as parallel agents. Each researches their person's real opinions via web search and reasons from that perspective. Results are synthesized into a consensus recommendation.

## Trigger

User says "ask the council" (or variants: "consult the council", "what would the council say", "council opinion on") followed by a language design question.

## Full Expert Roster (19)

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

Read `references/experts.md` in full. You need the Relevance Matrix, each expert's philosophy, known positions, search strategy, and voice.

### Step 1: Formulate the Question

Take the user's question and restate it precisely:

```
COUNCIL QUESTION: [Restate the exact design question]
CONTEXT: [Relevant NeoHaskell decisions already made - read from design/syntax.md]
OPTIONS: [If the user presented options, list them. If not, identify the option space.]
```

### Step 2: Select the 3 Most Relevant Experts

Using the Relevance Matrix at the bottom of `references/experts.md`, pick exactly 3 experts whose lenses are most useful for this specific question. Consider:

1. **Question domain** — match the question type to the matrix rows (e.g. "effect handling" → Nystrom, withoutboats, Syme, Czaplicki, Hickey)
2. **Diversity of perspective** — pick experts who will disagree productively, not 3 who'll say the same thing
3. **NeoHaskell fit** — prefer experts whose communities or philosophies directly relate to NeoHaskell's target audience (Java/C#/JS developers coming to FP)

**Show your selection with reasoning:**

```
SELECTED PANEL:
1. [Expert] — [why they're relevant to THIS question]
2. [Expert] — [why they're relevant to THIS question]
3. [Expert] — [why they're relevant to THIS question]

CONSIDERED BUT EXCLUDED: [1-2 names and why they're less relevant here]
```

### Step 3: Spawn 3 Expert Agents in Parallel

Spawn exactly three background librarian agents, one per selected expert, using this template:

```
PERSONA: You are impersonating [EXPERT_NAME], [ROLE].
Your philosophy: [PHILOSOPHY from experts.md]
Your known positions: [KNOWN_POSITIONS from experts.md]
Your voice: [VOICE from experts.md]

TASK: NeoHaskell is a newcomer-friendly Haskell dialect that targets Java/C#/JS developers,
uses braces, postfix type annotations, explicit effect markers like let!, and a strict novelty budget.

The design team asks: [THE COUNCIL QUESTION]

Context: [NEOHASKELL CONTEXT]
Options: [OPTIONS]

INSTRUCTIONS:
1. Search the web for [EXPERT_NAME]'s relevant writings/opinions.
   Search: [SEARCH_STRATEGY from experts.md]
   Also search: "[EXPERT_NAME]" + [keywords from the question]
2. Base your answer on confirmed positions first; extrapolate only when necessary.
3. Do not invent positions.
4. Stay in character and reasoning style.

RESPONSE FORMAT:
**[EXPERT_NAME]'s verdict:** [SUPPORT / OPPOSE / CONDITIONAL / ABSTAIN]
**Position:** [2-4 sentences]
**Reasoning:**
- [bullet]
- [bullet]
- [bullet]
**Concern:** [1-2 sentences]
**Evidence:** [links or references]
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

### Step 4: Collect Results

Collect all three via `background_output(task_id=...)`.

If one agent fails, do not retry. Synthesize from available responses and explicitly note the missing perspective.

### Step 5: Synthesize

Produce output in this exact format:

---

## Council Verdict: [QUESTION SUMMARY]

### Panel Selected

| Expert | Lens | Why Selected |
|--------|------|--------------|
| [name] | [lens] | [1-sentence reason] |
| [name] | [lens] | [1-sentence reason] |
| [name] | [lens] | [1-sentence reason] |

### Quick Tally

| Verdict | Experts |
|---------|---------|
| SUPPORT | [names] |
| OPPOSE | [names] |
| CONDITIONAL | [names] |
| ABSTAIN | [names] |

### Consensus Position

[1-2 short paragraphs]

### Key Arguments FOR

[bullets with attribution]

### Key Arguments AGAINST

[bullets with attribution]

### Conditions and Caveats

[conditional constraints]

### Expert-Specific Impact Sections

For each of the 3 selected experts, include a section:

### [Lens] Impact ([Expert Name])

[How this decision relates to their specific area of expertise]

### Final Recommendation

**The council recommends:** [clear recommendation]
**Confidence:** [HIGH / MEDIUM / LOW]
**Dissent strength:** [STRONG / MILD / NONE]

---

### Step 6: Present to User

Present the synthesis and wait for user validation before changing design docs.

## Anti-Patterns

- Do not skip web search for expert alignment.
- Do not force consensus when dissent exists.
- Do not claim unanimity without evidence.
- Do not update design docs before user validates council output.
- Do not spawn more than 3 experts unless the user explicitly asks for a full council.
- Do not pick 3 experts who will obviously agree — diversity of perspective is the point.
