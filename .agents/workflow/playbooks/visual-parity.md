### Visual parity

**You own pixel-exact equivalence. The baseline is the spec. You do not touch it.** Equivalence is verified by image diff, not by eye.

1. Establish the baseline first, before any migration: a visual regression harness that screenshots the current component across its states, plus the target when matching two implementations. Use **control-ui** for browser/IDE visuals. For a TUI, use **control-cli** with a real PTY to drive the interaction and capture the terminal renderer's images; include focus, cursor, keyboard navigation and resize states where relevant. Establish reproducible captures before freezing: same browser or terminal renderer/version, viewport or terminal rows/columns, fonts/theme/color configuration and display scaling, locale/TERM, fixtures, readiness, animation/cursor phase and tested build. Confirm unchanged captures reproduce and freeze the exact image comparator with the harness. Keep PTY transcripts as interaction receipts and rendered images as pixel-parity evidence. No baseline, no parity claim. A blocking prerequisite, not a follow-up.
2. Anti-shortcut clauses, stated and held: no harness modifications, no baseline tampering, no component restructuring to make a diff pass. If the baseline looks wrong, stop and ask, don't edit it.
3. Migrate one component at a time. Parallelize across worktrees, one owner per component (the **separate-before-serializing-shared-state** principle skill). Shared primitives migrate first as a blocking phase.
4. Verify each component against its baseline via image diff on the matching surface via the control skill. A nonzero diff is a fail. Investigate the pixel delta. `/loop` per component until the diff is zero.
5. Run **Opening a PR** per component or per safe batch.

Keep temporary capture manifests, baseline copies, candidate images and image-diff receipts under repository-root `.agents-state/visual-parity/<run>/`, configuring destinations before freezing the harness. Canonical approved baselines remain with their owning harness.

**Reply:** components migrated, the diff result for each, the baseline harness location, what's left.
