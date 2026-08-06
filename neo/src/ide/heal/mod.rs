//! Deterministic event-model repair.
//!
//! The heal flow's outer shell still spawns `claude -p` for the fuzzy cases,
//! but most repairs against a NeoHaskell codebase are mechanical: the
//! `inspect_project` walk already knows every command, event, query and
//! integration plus their wiring. Comparing that table to the JSON model
//! deterministically produces a list of missing edges, missing nodes,
//! integration-kind drifts and misplaced positions.
//!
//! Doing this in Rust matters because:
//!   * it's instant (single-digit ms vs minutes of LLM reasoning),
//!   * it's deterministic (same input → same output, no flake),
//!   * it eliminates the largest source of LLM thrash (sonnet was burning
//!     thousands of thinking tokens on set-difference computations that
//!     belong in code).
//!
//! When the deterministic pass alone makes the file valid we never spawn
//! the subprocess. When residual issues remain we spawn claude with a
//! much smaller prompt that names ONLY those residuals.

pub mod apply;
pub mod diff;
