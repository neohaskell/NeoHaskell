---
name: validate-skills
description: Check authored skills for required metadata, duplicate names, missing local resources and broken cross-skill links without imposing a fixed skill inventory or agent-tool configuration.
---

# Validate skill resources

Run `python3 .agents/skills/validate-skills/scripts/check.py --repo <root>` after authoring or relocating skills. Use `--skill <folder>` for one skill or `--skills-root <folder>` for a collection outside the default layout.

The checker validates required name and description strings, folder names, duplicate names, concrete local Markdown links and concrete skill resource paths. It skips fenced examples and external URLs. For the repository collection, it also checks workflow links and resource ownership. It does not impose a fixed inventory, inspect codemap cache contents, configure an agent tool, or prove that instructions make good decisions.

Use whichever skill-authoring tool is available. Run its native validator when its instructions require one. Execute changed helpers against their actual contract and use an independent behavioral check when the skill's complexity warrants it. Keep tests and resources with their owner, and link shared callers to that one copy.
