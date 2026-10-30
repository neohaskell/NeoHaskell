//! Interpreter for Nix writes that fail because the disk is full.

use crate::interpret::{Interpreter, Kind};

pub const ENTRY: Interpreter = Interpreter {
    id: "nix-disk-full",
    kind: Kind::Nix,
    pattern: r"No space left on device|ENOSPC",
    cause: "nix ran out of disk space",
    fix: "Free disk space, then retry the build.",
};

#[cfg(test)]
mod tests {
    use crate::interpret::{match_kind, Kind};

    #[test]
    fn matches_enospc() {
        let interpreted = match_kind(Kind::Nix, "error: write failed: ENOSPC").unwrap();
        assert!(interpreted.fix.contains("Free disk space"));
    }
}
