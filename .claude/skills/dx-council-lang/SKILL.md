---
name: dx-council-lang
description: Language design expert panel. Use when user says 'ask the council' or needs expert opinions on language syntax, DX tradeoffs, or ergonomics questions for NeoHaskell. Selects 3 relevant experts from a 19-person roster and researches their actual positions.
---

# DX Council — Language Design Expert Panel

Select the 3 most relevant experts from a 19-person roster based on the question, research their real opinions via web search, and synthesize into a consensus recommendation.

## Trigger

User says "ask the council" (or variants) followed by a language design question.

## Expert Roster (19)

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

## Expert Selection by Question Type

| Question Type | Most Relevant Experts |
|---------------|----------------------|
| Function syntax | Nystrom, matklad, Wlaschin, Feldman, Syme |
| Type annotations | matklad, Syme, King, Wlaschin, Borretti |
| Effect handling | Nystrom, withoutboats, Syme, Czaplicki, Hickey |
| Keywords/naming | Klabnik, Matz, DHH, Breslav |
| Beginner friendliness | Hermans, Feldman, Klabnik, Czaplicki, Wlaschin |
| Pattern matching | matklad, withoutboats, King, Feldman |
| Module system | Borretti, matklad, Breslav |
| Error handling | Czaplicki, Wlaschin, Bernhardt, King |
| Type system scope | Syme, Hickey, King, withoutboats |
| Overall philosophy | Hickey, Matz, Czaplicki, Borretti, Klabnik |
| Consistency/trust | Bernhardt, matklad, Wayne, Borretti |
| Teaching/adoption | Hermans, Feldman, Wayne, Klabnik, Czaplicki |

## Selection Principles

1. **Question domain** — match question type to matrix above
2. **Diversity of perspective** — pick experts who will disagree productively
3. **NeoHaskell fit** — prefer experts whose communities relate to NeoHaskell's target (Java/C#/JS devs coming to FP)

## Orchestration Protocol

### Step 1: Formulate the Question

```
COUNCIL QUESTION: [Restate the exact design question]
CONTEXT: [Relevant NeoHaskell decisions already made]
OPTIONS: [If presented, list them. Otherwise identify the option space.]
```

### Step 2: Select 3 Experts

Show selection with reasoning:

```
SELECTED PANEL:
1. [Expert] — [why relevant to THIS question]
2. [Expert] — [why relevant to THIS question]
3. [Expert] — [why relevant to THIS question]

CONSIDERED BUT EXCLUDED: [1-2 names and why less relevant here]
```

### Step 3: Research Each Expert

For each expert, search for their actual writings:
- `"[Expert Name]" language design [topic]`
- `[Expert's blog/site] [keywords]`
- `"[Expert Name]" [specific syntax or feature]`

Base answers on confirmed positions. Mark inference explicitly.

### Step 4: Synthesize

```markdown
## Council Verdict: [QUESTION SUMMARY]

### Panel Selected

| Expert | Lens | Why Selected |
|--------|------|--------------|
| [name] | [lens] | [1-sentence reason] |

### Quick Tally

| Verdict | Experts |
|---------|---------|
| SUPPORT | [names] |
| OPPOSE | [names] |
| CONDITIONAL | [names] |

### Consensus Position

[1-2 short paragraphs]

### Key Arguments FOR

- [Expert]: [argument]

### Key Arguments AGAINST

- [Expert]: [argument]

### Conditions and Caveats

[conditional constraints]

### Final Recommendation

**The council recommends:** [clear recommendation]
**Confidence:** [HIGH / MEDIUM / LOW]
**Dissent strength:** [STRONG / MILD / NONE]
```

## Key Expert Philosophies (Quick Reference)

**Klabnik (Strangeness Budget)**: Every language has a budget of unfamiliar concepts. Spend on core innovation, keep everything else familiar.

**Hickey (Simple vs Easy)**: Simple = few interleaved concepts. Easy = familiar. Both matter, don't confuse them.

**Feldman (Teaching-Informed)**: Teaching Elm to thousands informed every Roc decision. The learning curve is a design failure.

**Czaplicki (Radical Simplicity)**: Add features by subtraction. Error messages are UX.

**Syme (Anti-Max-Abstraction)**: Haskell-style typeclasses are over-abstraction for most code.

**Borretti (Language Pragmatics)**: "The code that gets written is the code that's easier to write."

**matklad (Syntax vs Semantics)**: Most "ugly syntax" complaints are semantics in disguise. True syntax problems: visual noise, refactoring friction, linear readability.

See `${CLAUDE_SKILL_DIR}/references/experts.md` for full profiles.

## Anti-Patterns

- Do not skip web search for expert alignment
- Do not force consensus when dissent exists
- Do not claim unanimity without evidence
- Do not pick 3 experts who will obviously agree — diversity is the point
