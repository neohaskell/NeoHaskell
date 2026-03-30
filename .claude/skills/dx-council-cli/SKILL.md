---
name: dx-council-cli
description: CLI Developer Experience expert panel. Use when user says 'Ask the DX council' or needs expert opinions on CLI design, developer experience, error messages, command structure, or UX decisions. Spawns parallel research for multiple CLI design experts.
---

# DX Council — CLI Developer Experience Expert Panel

Use when the user says "Ask the DX council" or needs expert opinions on CLI design, developer experience, error messages, command structure, or UX decisions.

## How It Works

1. **Parse the question** — extract the specific design decision
2. **Select 5-7 relevant experts** — pick the most relevant subset for the question
3. **Research each expert's position** — use web search to find their actual writings
4. **Synthesize** — find consensus, surface disagreements, deliver recommendation

## Council Members

### Core Five (Use for most questions)

| Expert | Focus | Key Work |
|--------|-------|----------|
| **Aanand Prasad** | Modern CLI standards, clig.dev | https://clig.dev/ |
| **Jeff Dickey** | 12 Factor CLI Apps, oclif | Heroku CLI architect |
| **Carolyn Van Slyck** | CLI grammar design, Porter | Task-oriented commands |
| **Mislav Marohnić** | Opinionated design, gh CLI | github.com/cli/cli |
| **Lilian Costa** | Error experience design | CLI error messages |

### Extended Council

| Expert | Focus | When to Include |
|--------|-------|-----------------|
| **Eva Parish** | Help text, language clarity | Documentation questions |
| **Steve Francia** | Cobra framework, CLI patterns | Command structure |
| **Eric Raymond** | Unix philosophy | Simplicity debates |
| **Pike & Kernighan** | Composition over features | Flag proliferation |
| **Roman Shamin** | Progress displays, visual design | Loading/feedback UX |
| **Johnson & Belton** | First-run experience | Onboarding questions |
| **Jacob Tiedemann** | Organizational standards | Cross-team consistency |

## Expert Selection Guide

| Question Type | Who to Ask |
|---------------|------------|
| Command structure / grammar | Van Slyck, Marohnić, Francia, Prasad |
| Error messages / output | Parish, Dickey, Shamin |
| First-run / onboarding | Dickey, Johnson & Belton, Van Slyck |
| Flags vs. simplicity | Pike & Kernighan, Raymond, Prasad |
| Progress / loading | Shamin, Dickey |
| Help text / docs | Parish, Van Slyck, Tiedemann |
| Opinionated vs. flexible | Marohnić, Pike & Kernighan, Dickey |

## Expert Philosophies (Quick Reference)

**Prasad (clig.dev)**: "What does the user expect? Do that."
- Stderr for messages, stdout for data
- Errors should suggest next action
- Colors degrade gracefully (respect NO_COLOR)

**Dickey (12 Factor)**: "You're going to rewrite this CLI twice. Design the interface first."
- Great help is not optional
- Be fancy: spinners, colors, tables
- Make the most common thing easy

**Van Slyck**: "If you didn't design your CLI grammar intentionally, you don't have one."
- Pick noun-verb or verb-noun, be consistent
- Tab completion is mandatory
- Examples in help text, not just flag lists

**Marohnić**: "We rewrote GitHub CLI from scratch. Best decision ever."
- Be opinionated and focused
- CLIs should model workflows, not API endpoints
- 80% case trivial, 20% case possible

**Raymond (Unix)**: "Complexity is the enemy."
- Do one thing well
- Fail noisily and early
- Design for composition

**Pike & Kernighan**: "Every flag doubles your testing surface."
- Power comes from composition, not features
- The correct response to "add a flag for X" is usually "no"

## Synthesis Template

After researching expert positions:

```markdown
## DX Council Verdict: [Topic]

**Consensus:** [What most/all experts agree on]

**Dissent:** [Where experts disagree and why — often the most valuable part]

**Recommendation for NeoHaskell:** [Specific advice grounded in Jess persona + NeoHaskell principles]

**Key quotes:**
- "[Expert]": "[punchy line capturing their reasoning]"
- "[Expert]": "[contrasting view if applicable]"
```

## Example Questions

- "Should `neo build` require a project file or infer it?"
- "How should we handle errors when the user's Haskell toolchain is missing?"
- "Should we use subcommands (`neo run build`) or flags (`neo --build`)?"
- "What should the first-run experience look like?"
- "How verbose should progress output be by default?"

## Research Strategy

For each expert, search:
- `"[Expert Name]" CLI design`
- `"[Expert Name]" command line`
- `[Expert's key work] + [topic keywords]`

Base answers on confirmed positions. Extrapolate only when necessary and mark as inference.
