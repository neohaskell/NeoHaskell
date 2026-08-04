//! Interpreter for nix `hash '<bad>' ... has wrong length` errors.
//!
//! Fires when the NeoHaskell SHA pin in `flake.nix` is the placeholder
//! `deadbeef` literal — invariably because `NEO_SKIP_NETWORK=1` was set
//! when `neo build` last regenerated the flake.

use crate::interpret::{Interpreter, Kind};

pub const ENTRY: Interpreter = Interpreter {
    id: "nix-deadbeef-hash",
    kind: Kind::Nix,
    // `(?s)` so `.*?` can cross newlines — nix sometimes interleaves the
    // hash quote and the "wrong length" message across separate lines.
    pattern: r"(?s)hash '(?<hash>[^']*)'.*?has wrong length",
    cause: "the NeoHaskell pin in `flake.nix` resolved to `{hash}`, which is not a valid git hash — this almost always means `NEO_SKIP_NETWORK=1` was set when `neo build` last ran, so `flake.nix` got the placeholder SHA `deadbeef` baked in",
    fix:   "Unset `NEO_SKIP_NETWORK` and regenerate the flake: `unset NEO_SKIP_NETWORK && rm flake.nix flake.lock cabal.project *.cabal && neo build`. (Use `NEO_SKIP_NETWORK=1` only for offline scaffolding, never for builds.)",
};

#[cfg(test)]
mod tests {
    use crate::interpret::{match_kind, Kind};

    #[test]
    fn matches_inline() {
        let captured = "error: hash 'deadbeef' has wrong length, expected 40";
        let i = match_kind(Kind::Nix, captured).expect("should match");
        assert!(i.cause.contains("resolved to `deadbeef`"), "cause: {}", i.cause);
        assert!(i.fix.contains("unset NEO_SKIP_NETWORK"), "fix: {}", i.fix);
    }

    #[test]
    fn matches_across_lines() {
        let captured = "error: hash 'abc'\n  at /nix/store/...\n  has wrong length, expected 40";
        let i = match_kind(Kind::Nix, captured).expect("should match");
        assert!(i.cause.contains("`abc`"), "cause: {}", i.cause);
    }
}
