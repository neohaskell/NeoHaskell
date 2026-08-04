//! The generated Haskell `test-suite` stanza — built in by default.
//!
//! NeoHaskell ships first-class in-process testing (the `Test` testlib, decider /
//! projection / property helpers), but a stock `neo` project historically had no
//! way to build a suite that used them: the reconciled `<name>.cabal` emitted only
//! `library` + `executable`, so `neo test` → `cabal test all` compiled zero Haskell
//! tests. This module closes that gap.
//!
//! Testing is **not a toggle** — every NeoCLI project has a test-suite so `neo test`
//! is always available. On every reconcile we guarantee the `tests/Spec.hs`
//! hspec-discover driver exists (creating `tests/` if needed), then list every
//! sibling `*Spec.hs` as an other-module. The suite is wired to `hspec` +
//! `QuickCheck` + `quickcheck-instances` and the project's own library, using the
//! standard `hspec-discover` convention — matching how the NeoHaskell TDD skills
//! scaffold specs (`tests/**/<X>Spec.hs` exporting `spec`).
//!
//! Haskell specs share the `tests/` directory with `.hurl` e2e scenarios; the two
//! never collide (hspec-discover + module discovery see only `*.hs`; Hurl discovery
//! sees only `*.hurl`).

use std::fs;
use std::path::Path;

use crate::errors::NeoError;
use crate::reconcile::modules;

/// `main-is` for the generated test-suite. The file holds only the `hspec-discover`
/// preprocessor pragma; GHC turns it into the `Main` module that runs every spec.
pub const DRIVER_FILE: &str = "Spec.hs";

/// Contents of the `hspec-discover` driver. hspec-discover scans `tests/` for every
/// `*Spec.hs` (skipping this driver file) and generates a `main` that runs them all.
pub const DRIVER_CONTENTS: &str = "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}\n";

/// The test-suite `other-modules`: every module under `tests/` except the
/// `Spec` driver (which is `main-is`, not an other-module) and `Main`.
///
/// Every module the suite compiles must be listed here; otherwise the generated
/// project's `-Wall -Werror` `common_cfg` trips `-Wmissing-home-modules` and the
/// whole suite fails to build. Reusing [`modules::discover`] keeps the src/ and
/// tests/ discovery identical (nested dirs → dotted module names, `Main` excluded).
pub fn other_modules(project_dir: &Path) -> Vec<String> {
    let driver_module = Path::new(DRIVER_FILE)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("Spec");
    modules::discover(project_dir.join("tests"))
        .into_iter()
        .filter(|m| m != driver_module)
        .collect()
}

/// Guarantee the `hspec-discover` driver exists at `tests/Spec.hs`, creating the
/// `tests/` directory if it is missing.
///
/// Called on every reconcile (testing is built in by default). Idempotent: writes
/// the driver only when it is absent, so a hand-authored driver — or the specs the
/// TDD skills scaffold under `tests/` — are never clobbered. With only the driver
/// present, hspec-discover finds zero specs and the suite passes trivially; `neo
/// test` is still available.
pub fn ensure_driver(project_dir: &Path) -> miette::Result<()> {
    let test_dir = project_dir.join("tests");
    if !test_dir.exists() {
        fs::create_dir_all(&test_dir)
            .map_err(|e| NeoError::io_at("creating the `tests/` directory at", &test_dir, e))?;
    }
    let driver = test_dir.join(DRIVER_FILE);
    if driver.exists() {
        return Ok(());
    }
    fs::write(&driver, DRIVER_CONTENTS)
        .map_err(|e| NeoError::io_at("writing the hspec-discover test driver to", &driver, e))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn other_modules_excludes_driver() {
        let dir = tempdir().unwrap();
        fs::create_dir_all(dir.path().join("tests")).unwrap();
        fs::write(dir.path().join("tests/Spec.hs"), DRIVER_CONTENTS).unwrap();
        fs::write(dir.path().join("tests/ExampleSpec.hs"), "").unwrap();
        let mods = other_modules(dir.path());
        assert_eq!(mods, vec!["ExampleSpec".to_string()]);
    }

    #[test]
    fn other_modules_includes_nested_and_helpers() {
        let dir = tempdir().unwrap();
        fs::create_dir_all(dir.path().join("tests/Decider/Order")).unwrap();
        fs::write(dir.path().join("tests/Spec.hs"), DRIVER_CONTENTS).unwrap();
        fs::write(dir.path().join("tests/Decider/Order/PlaceSpec.hs"), "").unwrap();
        fs::write(dir.path().join("tests/TestHelpers.hs"), "").unwrap();
        let mods = other_modules(dir.path());
        // Sorted (modules::discover sorts); driver excluded; helper + nested spec kept.
        assert_eq!(
            mods,
            vec![
                "Decider.Order.PlaceSpec".to_string(),
                "TestHelpers".to_string(),
            ]
        );
    }

    #[test]
    fn other_modules_empty_when_only_driver() {
        let dir = tempdir().unwrap();
        fs::create_dir_all(dir.path().join("tests")).unwrap();
        fs::write(dir.path().join("tests/Spec.hs"), DRIVER_CONTENTS).unwrap();
        assert!(other_modules(dir.path()).is_empty());
    }

    #[test]
    fn ensure_driver_writes_when_missing() {
        let dir = tempdir().unwrap();
        fs::create_dir_all(dir.path().join("tests")).unwrap();
        fs::write(dir.path().join("tests/ExampleSpec.hs"), "").unwrap();
        ensure_driver(dir.path()).unwrap();
        let driver = dir.path().join("tests/Spec.hs");
        assert!(driver.exists());
        assert_eq!(fs::read_to_string(&driver).unwrap(), DRIVER_CONTENTS);
    }

    #[test]
    fn ensure_driver_creates_test_dir_from_scratch() {
        // Testing is built in by default: even a project with no `tests/` at all gets
        // the driver (and directory), so `neo test` is always available.
        let dir = tempdir().unwrap();
        assert!(!dir.path().join("tests").exists());
        ensure_driver(dir.path()).unwrap();
        let driver = dir.path().join("tests/Spec.hs");
        assert!(driver.exists(), "ensure_driver must create tests/ and the driver");
        assert_eq!(fs::read_to_string(&driver).unwrap(), DRIVER_CONTENTS);
        // With only the driver, discovery yields no specs (suite still builds/passes).
        assert!(other_modules(dir.path()).is_empty());
    }

    #[test]
    fn ensure_driver_is_idempotent_and_preserves_custom_driver() {
        let dir = tempdir().unwrap();
        fs::create_dir_all(dir.path().join("tests")).unwrap();
        let driver = dir.path().join("tests/Spec.hs");
        let custom = "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}\n-- hand-edited\n";
        fs::write(&driver, custom).unwrap();
        ensure_driver(dir.path()).unwrap();
        // A pre-existing driver is never clobbered.
        assert_eq!(fs::read_to_string(&driver).unwrap(), custom);
    }
}
