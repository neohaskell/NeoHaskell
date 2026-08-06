//! Built-in interpreter patterns, one module per failure mode.
//!
//! Adding a new pattern:
//!   1. Create `<id_snake>.rs` in this directory exporting
//!      `pub const ENTRY: crate::interpret::Interpreter = …`.
//!   2. Add a `pub mod <id_snake>;` line to this file.
//!   3. Add `&<id_snake>::ENTRY,` to `ALL` at the correct precedence
//!      position (cabal → nix → git → hurl; within a kind, more-specific
//!      patterns first).
//!   4. Bump the `entry_count_matches_documented` assertion in
//!      `crate::interpret::tests` to the new count.
//!
//! See `.claude/skills/adding-error-interpreter-pattern/SKILL.md` for the
//! end-to-end workflow (which JSONL record motivated the entry, how to
//! pick the regex, where to write the regression test).

use crate::interpret::Interpreter;

pub mod cabal_unknown_package;
pub mod cabal_could_not_resolve;
pub mod nix_attribute_missing_single_quotes;
pub mod nix_attribute_missing_backticks;
pub mod nix_deadbeef_hash;
pub mod nix_builder_failed;
pub mod nix_tarball_download;
pub mod git_missing_ref;
pub mod git_unknown_revision;
pub mod git_repo_not_found;
pub mod hurl_connection_refused;

/// Every interpreter, in precedence order. First match wins. Group by tool
/// (cabal → nix → git → hurl) and within a tool put more-specific patterns
/// before more-general ones.
pub const ALL: &[&Interpreter] = &[
    &cabal_unknown_package::ENTRY,
    &cabal_could_not_resolve::ENTRY,
    &nix_attribute_missing_single_quotes::ENTRY,
    &nix_attribute_missing_backticks::ENTRY,
    &nix_deadbeef_hash::ENTRY,
    &nix_builder_failed::ENTRY,
    &nix_tarball_download::ENTRY,
    &git_missing_ref::ENTRY,
    &git_unknown_revision::ENTRY,
    &git_repo_not_found::ENTRY,
    &hurl_connection_refused::ENTRY,
];
