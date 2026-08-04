# Agent Instructions for NeoCLI

You are an autonomous AI coding agent assigned to implement features for the NeoCLI project. 

Follow this strict workflow for every session:

1. **Read Current State**: Begin by reading `STATE.md` to understand the current progress, what has already been scaffolded, and the overall context of the project.
2. **Execute Next Step**: Read `NEXT_STEP.md` to find your current objective and action items. This is your primary goal.
3. **Consult Implementation Plan**: As you implement the next step, continuously reference `IMPLEMENTATION_PLAN.md` to understand the architectural rules, expected file structures, TUI guidelines, and error handling strategies. This document is your technical source of truth for *how* the feature should be implemented.
4. **Verify Your Work**: 
    - First, run `cargo check` to ensure your implementation compiles.
    - Second, **test the CLI manually by hand in a shell**. Run `cargo run -- <command>` with various flags and inputs to ensure it behaves correctly and looks right (TUI).
    - Third, run `cargo test` to ensure no regressions were introduced.
    - Fourth, run the **Neo-on-Neo smoke test** (if using `ralph.sh`, this is automated, but you can run it manually by creating a new project with your build and running `build` and `test` inside it).
    - If you find bugs during manual testing, fix them immediately.
    - **IMPORTANT**: If automated tests (cargo check/test or the smoke test) fail, you MUST document the specific errors in `NEXT_STEP.md` before attempting a fix in the next iteration.
5. **Human Assistance**: If you encounter a bug that requires human design decisions, or if you are stuck and cannot resolve a problem after several attempts, explicitly state 'HUMAN_ASSISTANCE_REQUIRED' and stop.
6. **Update State Log**: Once you have successfully completed the tasks in `NEXT_STEP.md` and verified them manually, append a brief log entry to the bottom of the `STATE.md` file.
7. **Determine Next Step**: Update `NEXT_STEP.md` with the subsequent logical task from `IMPLEMENTATION_PLAN.md`.

**Key Reminders**:
- Adhere to the `miette` error handling conventions defined in `src/errors.rs`.
- Respect the `OutputMode` abstraction (Interactive vs. CI) detailed in the plan.
- The project strictly uses `tokio` for async and `ratatui` for interactive terminal UI.
