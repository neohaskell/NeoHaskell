---
name: visualize
description: Create factual diagrams, comparisons and visual explanations using the external drawio-skill dependency. Use for /visualize, an editable diagram request, or a caller that needs a visual explanation.
---

# Visualize

Use [drawio-skill](https://github.com/Agents365-ai/drawio-skill) as an external dependency. This repository contains only this wrapper. Fetch the full upstream `skills/drawio-skill/` installation outside the repository; never copy its instructions, scripts, references or assets into this project's tracked files.

## First use

If a caller requires a read-only turn, dependency discovery is read-only too. When installation or configuration is missing, say which required dependency/setup is unavailable and defer setup to a setup-permitted invocation of this skill. Do not install software, edit configuration or silently substitute another dependency in that caller's turn. Existing setup authorization still follows the caller's narrower scope and actual environment permissions.

1. Read `setup.local.json` beside this `SKILL.md` first. Validate the recorded paths and capabilities against the current machine; missing or stale state is a reason to rediscover the dependency, not proof that it must be reinstalled. Find an existing user-level installation of `drawio-skill` when needed. Resolve its actual directory as `drawio_skill_dir`, containing `SKILL.md`, `scripts/` and `references/`; the repository root alone is not the skill directory. Reuse a valid installation without reinstalling or updating it.
2. If missing, tell the user: “Visualization requires drawio-skill. I’ll install it outside this repository and check the tools needed for this output.” Then install and configure it for this invocation using the environment's available skill installer and the upstream [installation instructions](https://github.com/Agents365-ai/drawio-skill/blob/main/docs/INSTALL_SKILL.md). Install only the chosen dependency into user-level storage, not into this repository or unrelated agent installations.
3. Without a suitable installer, use a persistent external clone and load its skill directly. For example, after checking that the destination is outside this checkout and does not conflict with existing files:

   ```sh
   drawio_source_dir="${XDG_DATA_HOME:-$HOME/.local/share}/agent-skill-deps/drawio-skill"
   mkdir -p "${drawio_source_dir%/*}"
   git clone https://github.com/Agents365-ai/drawio-skill.git "$drawio_source_dir"
   drawio_skill_dir="$drawio_source_dir/skills/drawio-skill"
   ```

   Reuse an existing matching installation rather than cloning into it. Do not overwrite an unrelated directory. No host registration or MCP server is needed to read the installed skill and invoke its scripts by absolute path.
4. Read the installed `SKILL.md` and ensure Python 3 is available, then run `python3 "$drawio_skill_dir/scripts/diagramctl.py" doctor` with the resolved directory. The default check does not launch a GUI. Core workflows require Python; install/configure draw.io for native exports or Graphviz for automatic layout only when the requested output needs them. Follow the environment's package-management and permission rules, preserving existing configuration. Use the installed dependency's documentation for platform details.
5. Verify the requested capability with an actual small output and its structural/rendering check; a successful doctor report alone is not rendering proof. Save the resolved installation, tool paths, revision/version when available, capability checks and verification result in the single `setup.local.json` file beside this `SKILL.md`. Keep the installed dependency external. Later invocations read and validate that record before reuse; updates are explicit work.

## Local setup record

`setup.local.json` is the only persistent machine-specific setup record. Resolve it relative to this skill's directory, not the current working directory. The adjacent `.gitignore` excludes it from version control. If it is missing, create it during setup; a read-only invocation may read it but must not create or update it.

Store the external skill/source directory, resolved tool executables, dependency revision/version, available capabilities and verification status there. Use null or an explicit unchecked state for facts not yet established; do not invent installation paths. Treat the file as JSON data, never shell instructions. Repair stale entries only in a setup-permitted invocation, and replace the record atomically without keeping additional configuration copies. Keep credentials and secrets out of it.

Ship only `SKILL.md` and `.gitignore`; never copy or commit `setup.local.json` when installing or sharing this wrapper. Diagram inputs, previews, diagnostics and evidence still belong under `.agents-state/visualize/`; they are not separate setup/configuration records.

## Make the requested visual

Follow the installed dependency and load only the references needed for the request. Keep the caller's audience, factual evidence, output format and stopping rules. Use the local CLI; external publishing and host-wide integration are separate actions, not automatic setup.

For NeoHaskell diagrams, establish nodes and relationships from maintained source, including service-record ownership and indirect interactions where relevant. The inspected upstream importers do not include Haskell: use verified graph/diagram input instead of claiming an automatic Haskell extraction. Preserve the distinction between observed behavior and inferred relationships.

Use only supplied or verified numbers and labels for quantitative comparisons. If the requested chart or export is unsupported, report the specific missing capability instead of presenting a decorative approximation as proof. Never fabricate screenshots of an interface that was not captured.

Keep intermediate inputs, previews and diagnostics under `.agents-state/visualize/`. Deliver editable source and the requested verified preview/export at the caller's chosen destination; otherwise keep them under `.agents-state/visualize/`. Inspect the actual output before reporting success. If setup or rendering fails, report what succeeded, what is missing and the concrete next step; do not claim a completed visual.
