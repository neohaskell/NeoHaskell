---
name: roles
description: Select provider-aware models for workflow roles, or update the repository's central role-to-model-and-effort mapping.
---

# Roles

Use the active provider's column when an authorized workflow delegates work. This is the single role-to-model-and-effort mapping; AGENTS.md and the invoking workflow own responsibilities, panel sizes, writer ownership and independent-review requirements. A mapping does not authorize delegation or change those requirements.

Keep this skill and its model/effort table versioned in the repository at `.agents/skills/roles/SKILL.md`. Change the repository table when shared configuration changes; runtime availability checks use the current environment.

## Model and effort mapping

These are preferred choices, subject to availability in the current agent environment. Keep model identifiers separate from reasoning effort. Values separated by `+` are candidate pools, not a command to create that many agents.

| Workflow role | OpenAI model | OpenAI effort | Anthropic model | Anthropic effort |
| --- | --- | --- | --- | --- |
| feature, refactoring | `gpt-6-luna` | `max` | `claude-sonnet-5-5` | `high` |
| bug-fix | `gpt-6-luna` | `max` | `claude-sonnet-5-5` | `high` |
| perf-issue | `gpt-6-luna` | `max` | `claude-sonnet-5-5` | `high` |
| hillclimb | `gpt-6-luna` | `max` | `claude-sonnet-5-5` | `high` |
| judgment and prose | `gpt-6.1-sol` | `high` | `claude-opus-5-5` | `high` |
| hardest tasks | `gpt-6-astra` | `medium` | `claude-fable-5-1` | `high` |
| how explorer | `gpt-6-luna` | `max` | `claude-sonnet-5-5` | `high` |
| how explainer | `gpt-6.1-sol` | `high` | `claude-opus-5-5` | `high` |
| why investigators | `gpt-6-luna` | `max` | `claude-sonnet-5-5` | `high` |
| why synthesizer | `gpt-6.1-sol` | `high` | `claude-opus-5-5` | `high` |
| reflect tooling | `gpt-6-luna` | `max` | `claude-sonnet-5-5` | `high` |
| reflect judgment, divergent, synthesizer | `gpt-6.1-sol` | `high` | `claude-opus-5-5` | `high` |
| arena runners | `gpt-6-luna` + `gpt-6.1-sol` | `max` + `high` (respectively) | `claude-sonnet-5-5` + `claude-opus-5-5` | `high` + `high` (respectively) |
| arena cross-judge pool | `gpt-6-luna` + `gpt-6.1-sol` | `max` + `high` (respectively) | `claude-sonnet-5-5` + `claude-opus-5-5` | `high` + `high` (respectively) |
| swarm workers | `gpt-6-luna` | `max` | `claude-sonnet-5-5` | `high` |
| architect runners | `gpt-6-luna` + `gpt-6.1-sol` | `max` + `high` (respectively) | `claude-sonnet-5-5` + `claude-opus-5-5` | `high` + `high` (respectively) |
| interrogate reviewers | `gpt-6-luna` + `gpt-6.1-sol` | `max` + `high` (respectively) | `claude-sonnet-5-5` + `claude-opus-5-5` | `high` + `high` (respectively) |

## Resolve a role

1. Honor the user's explicit model or effort choice and applicable environment constraints. Otherwise select the role's preferred model for the active provider. Do not switch provider implicitly.
2. Check the actual delegation interface's available models and supported controls. A public model ID is a candidate, not proof of account access or a valid subagent parameter. Resolve aliases or host-specific identifiers only through the environment's supported mapping; never construct identifiers by appending effort or speed tokens.
3. Apply the selected model's effort from the same row, pairing pool entries in order. Pass effort through a separate supported control. If an explicit effort cannot be selected, report that limitation rather than silently substituting a default or claiming the requested effort ran. Similarly named levels across models or providers are not equivalent budgets.
4. For panels, use the invoking workflow's required count and independence rules. Prefer distinct available models from the pool where useful. Different models do not by themselves establish independence. A same-model panel may still use separate agents when the workflow permits it; disclose reduced diversity. A required cross-model comparison needs actual model diversity.
5. If the preferred choice is unavailable, report that limitation and use a confirmed available alternative or inherit the parent only where the workflow permits it. If a mandatory capability, independent reviewer or comparison cannot be supplied, leave that requirement unresolved. Do not present an author's own check as independent verification.

## Change the mapping

Edit this table when the user requests different role choices or available models change. Preserve unrelated role choices. Show the proposed mapping and validate execution identifiers before claiming it is usable. Keep provider/model choices here rather than copying them into AGENTS.md, leaf workflows or global editor settings. Do not rewrite provider-global configuration as a side effect.

Model guidance checked on 2026-10-21: [OpenAI models](https://learn.chatgpt.com/docs/models), [Anthropic models](https://platform.claude.com/docs/en/models/overview), [Anthropic effort](https://platform.claude.com/docs/en/build-with-claude/effort). These sources describe candidates; runtime availability remains the execution check.
