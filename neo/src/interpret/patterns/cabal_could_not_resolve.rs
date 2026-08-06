//! Interpreter for cabal `Could not resolve dependencies` errors.
//!
//! Emitted when cabal's solver cannot find a set of versions that satisfy
//! every constraint in `neo.json` simultaneously.

use crate::interpret::{Interpreter, Kind};

pub const ENTRY: Interpreter = Interpreter {
    id: "cabal-could-not-resolve",
    kind: Kind::Cabal,
    pattern: r"Could not resolve dependencies",
    cause: "version constraints in `neo.json` cannot be satisfied simultaneously",
    fix:   "Loosen the `^`/`~` ranges in `neo.json` so they overlap (e.g. change `\"text\": \"^2.1\"` to `\"text\": \"^2.0\"`), or pin compatible versions for the packages cabal listed as conflicting above. Then re-run `neo build`.",
};

#[cfg(test)]
mod tests {
    use crate::interpret::{match_kind, Kind};

    #[test]
    fn matches() {
        let captured = "cabal: Could not resolve dependencies:\n[__0] trying: foo-1.0\n";
        let i = match_kind(Kind::Cabal, captured).unwrap();
        assert!(i.cause.contains("version constraints in `neo.json` cannot be satisfied"));
        assert!(i.fix.contains("Loosen the `^`/`~` ranges"));
    }
}
