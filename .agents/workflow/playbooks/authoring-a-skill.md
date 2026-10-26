### Authoring or modifying a skill

**You own the skill's voice.**

1. Use whichever skill-authoring skill or built-in is available in the current agent environment.
2. Validate the skill: frontmatter has `name` and `description`, referenced files exist, cross-skill links resolve. Keep owned scripts, references, assets, tests and dependency configuration inside the skill folder; link to another skill's owned resource when sharing is required.
Run [Validate skills](../../skills/validate-skills/SKILL.md) for repository metadata, resource ownership and links, alongside the available authoring tool's native validator.
3. Test cases if structural. Skip if subjective.
4. Run **Opening a PR**.

When in doubt, delete. Keep only prose that changes a decision. Tell it to do the thing and skip the reason. Explain only when the rule is confusing without one. Match tone to scope. Point at structural sources (types, READMEs, config) per the **encode-lessons-in-structure** principle skill. Delegate to other skills by path. Don't restate. A workflow you keep hitting but isn't captured → propose a new skill.

**Reply:** summary of the skill, key design decisions, validation notes.
