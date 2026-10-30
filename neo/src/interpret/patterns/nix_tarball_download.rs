//! Interpreter for nix tarball / flake-input download failures.
//!
//! Covers the three distinct error messages nix uses for the same
//! underlying class of failure: network couldn't reach a flake input.

use crate::interpret::{Interpreter, Kind};

pub const ENTRY: Interpreter = Interpreter {
    id: "nix-tarball-download",
    kind: Kind::Nix,
    pattern: r"Could not download tarball|unable to download|error: getting attributes of path",
    cause: "nix could not download or access a flake input",
    fix:   "Check your connection (try `curl -I https://github.com`). If you are offline or behind a strict proxy, set `NEO_SKIP_NETWORK=1` only for scaffolding — builds always need real network.",
};

#[cfg(test)]
mod tests {
    use crate::interpret::{match_any, match_kind, Kind};

    #[test]
    fn matches_tarball() {
        let captured = "error: Could not download tarball from https://github.com/...";
        let i = match_kind(Kind::Nix, captured).expect("should match");
        assert!(i.cause.contains("nix could not download or access"));
    }

    #[test]
    fn matches_unable_to_download() {
        let i = match_kind(Kind::Nix, "error: unable to download 'https://x'").expect("should match");
        assert!(i.fix.contains("NEO_SKIP_NETWORK"));
    }

    #[test]
    fn disk_exhaustion_outweighs_earlier_download_error() {
        let captured = "error: unable to download 'http://127.0.0.1/cache.narinfo': HTTP error 418\nerror: write of 23016 bytes: No space left on device";
        let i = match_any(captured).expect("should match");
        assert!(i.cause.contains("disk space"), "cause: {}", i.cause);
        assert!(i.fix.contains("disk space"), "fix: {}", i.fix);
    }

    #[test]
    fn matches_getting_attributes() {
        let i = match_kind(Kind::Nix, "error: getting attributes of path '/nix/store/x'").expect("should match");
        assert!(i.cause.contains("flake input"));
    }
}
