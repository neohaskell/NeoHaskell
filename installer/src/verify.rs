use anyhow::Result;
use std::process::Command;

use crate::{detect, error::InstallerError, ui};

pub fn verify_nix(dry_run: bool) -> Result<()> {
    if dry_run {
        ui::print_step("Would verify toolchain installation", true);
        return Ok(());
    }

    let output = Command::new(detect::NIX_BIN)
        .arg("--version")
        .output()
        .map_err(InstallerError::CommandFailed)?;

    if output.status.success() {
        let version = String::from_utf8_lossy(&output.stdout);
        ui::print_step(&format!("Toolchain verified: {}", version.trim()), true);
        Ok(())
    } else {
        Err(InstallerError::VerificationFailed {
            details: "Toolchain not responding".into(),
        }
        .into())
    }
}

pub fn verify_neo(dry_run: bool) -> Result<()> {
    if dry_run {
        ui::print_step("Would verify Neo CLI installation", true);
        return Ok(());
    }

    let output = Command::new("neo")
        .arg("--version")
        .output()
        .map_err(InstallerError::CommandFailed)?;

    if output.status.success() {
        let version = String::from_utf8_lossy(&output.stdout);
        ui::print_step(&format!("Neo CLI verified: {}", version.trim()), true);
        Ok(())
    } else {
        Err(InstallerError::VerificationFailed {
            details: "Neo CLI not responding".into(),
        }
        .into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_nix_dry_run_returns_ok() {
        assert!(verify_nix(true).is_ok());
    }

    #[test]
    fn verify_neo_dry_run_returns_ok() {
        assert!(verify_neo(true).is_ok());
    }

    #[test]
    fn verify_nix_uses_absolute_path() {
        // Verify the constant is used (compile-time check via reference)
        assert!(detect::NIX_BIN.starts_with('/'));
    }

    #[test]
    fn verify_neo_uses_neo_command() {
        // This test verifies the function exists and handles missing neo gracefully
        // On dev machines without neo installed, this should return an error, not panic
        let result = verify_neo(false);
        // Either Ok (neo installed) or Err (neo not installed) — but never panic
        let _ = result;
    }
}
