use std::process::{Command, Stdio};

use anyhow::Result;

use crate::{detect, error::InstallerError, ui};

pub const INSTALL_CMD: &str = "curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install --no-confirm";

pub const NEO_FLAKE_REF: &str = "github:neohaskell/neo#neo-cli";

pub fn install_nix(verbose: bool, dry_run: bool) -> Result<()> {
    if dry_run {
        ui::print_step("Would install toolchain via Determinate installer", true);
        return Ok(());
    }

    let pb = ui::create_spinner(ui::MSG_TOOLCHAIN);

    let mut cmd = Command::new("sh");
    cmd.arg("-c").arg(INSTALL_CMD);

    if verbose {
        cmd.stdout(Stdio::inherit()).stderr(Stdio::inherit());
    } else {
        cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
    }

    let output = cmd.output().map_err(InstallerError::CommandFailed)?;

    if output.status.success() {
        ui::finish_success(&pb, "Toolchain installed");
        Ok(())
    } else {
        ui::finish_error(&pb, "Toolchain installation failed");
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(InstallerError::NixInstallFailed {
            details: stderr.to_string(),
        }
        .into())
    }
}

pub fn install_neo(verbose: bool, dry_run: bool) -> Result<()> {
    if dry_run {
        ui::print_step(&format!("Would install Neo CLI from {NEO_FLAKE_REF}"), true);
        return Ok(());
    }

    let pb = ui::create_spinner(ui::MSG_NEO_CLI);

    let mut cmd = Command::new(detect::NIX_BIN);
    cmd.args(["profile", "install", NEO_FLAKE_REF]);

    if verbose {
        cmd.stdout(Stdio::inherit()).stderr(Stdio::inherit());
    } else {
        cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
    }

    let output = cmd.output().map_err(InstallerError::CommandFailed)?;

    if output.status.success() {
        ui::finish_success(&pb, "Neo CLI installed");
        Ok(())
    } else {
        ui::finish_error(&pb, "Neo CLI installation failed");
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(InstallerError::NeoInstallFailed {
            details: stderr.to_string(),
        }
        .into())
    }
}

pub fn setup_shell(dry_run: bool) -> Result<()> {
    let shell = detect::get_shell();
    let profile_path = shell_profile_path(&shell)?;
    let path_line = nix_path_export(&shell);

    if profile_already_configured(&profile_path, &path_line)? {
        return Ok(());
    }

    if dry_run {
        ui::print_step(
            &format!("Would add PATH entry to {}", profile_path.display()),
            true,
        );
        return Ok(());
    }

    append_to_file(&profile_path, &path_line)?;
    Ok(())
}

fn shell_profile_path(shell: &detect::Shell) -> Result<std::path::PathBuf> {
    let home = std::env::var("HOME").map_err(|_| InstallerError::NixInstallFailed {
        details: "HOME not set".into(),
    })?;
    let path = match shell {
        detect::Shell::Zsh => format!("{home}/.zshrc"),
        detect::Shell::Bash => {
            if std::env::consts::OS == "macos" {
                format!("{home}/.bash_profile")
            } else {
                format!("{home}/.bashrc")
            }
        }
        detect::Shell::Fish => format!("{home}/.config/fish/config.fish"),
        detect::Shell::Unknown(_) => format!("{home}/.profile"),
    };
    Ok(std::path::PathBuf::from(path))
}

fn nix_path_export(shell: &detect::Shell) -> String {
    match shell {
        detect::Shell::Fish => {
            "fish_add_path /nix/var/nix/profiles/default/bin $HOME/.nix-profile/bin".to_string()
        }
        _ => r#"export PATH="/nix/var/nix/profiles/default/bin:$HOME/.nix-profile/bin:$PATH""#
            .to_string(),
    }
}

fn profile_already_configured(path: &std::path::Path, line: &str) -> Result<bool> {
    match std::fs::read_to_string(path) {
        Ok(contents) => Ok(contents.contains(line)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(false),
        Err(e) => Err(InstallerError::CommandFailed(e).into()),
    }
}

fn append_to_file(path: &std::path::Path, line: &str) -> Result<()> {
    use std::io::Write;
    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)?;
    writeln!(file, "\n# Added by NeoHaskell installer")?;
    writeln!(file, "{line}")?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn install_nix_dry_run_returns_ok() {
        assert!(install_nix(false, true).is_ok());
    }

    #[test]
    fn install_neo_dry_run_returns_ok() {
        assert!(install_neo(false, true).is_ok());
    }

    #[test]
    fn setup_shell_dry_run_returns_ok() {
        assert!(setup_shell(true).is_ok());
    }

    #[test]
    fn install_cmd_contains_determinate_url() {
        assert!(INSTALL_CMD.contains("install.determinate.systems/nix"));
    }

    #[test]
    fn install_cmd_contains_no_confirm() {
        assert!(INSTALL_CMD.contains("--no-confirm"));
    }

    #[test]
    fn neo_flake_ref_is_correct() {
        assert_eq!(NEO_FLAKE_REF, "github:neohaskell/neo#neo-cli");
    }

    #[test]
    fn shell_profile_path_zsh() {
        let path = shell_profile_path(&detect::Shell::Zsh).unwrap();
        assert!(path.to_string_lossy().ends_with(".zshrc"));
    }

    #[test]
    fn shell_profile_path_fish() {
        let path = shell_profile_path(&detect::Shell::Fish).unwrap();
        assert!(path.to_string_lossy().ends_with("config.fish"));
    }

    #[test]
    fn nix_path_export_bash_contains_export() {
        let line = nix_path_export(&detect::Shell::Bash);
        assert!(line.starts_with("export PATH="));
    }

    #[test]
    fn nix_path_export_fish_uses_fish_add_path() {
        let line = nix_path_export(&detect::Shell::Fish);
        assert!(line.starts_with("fish_add_path"));
    }

    #[test]
    fn profile_already_configured_returns_false_for_missing_file() {
        let result = profile_already_configured(
            std::path::Path::new("/tmp/nonexistent_neo_test_file"),
            "test line",
        );
        assert_eq!(result.unwrap(), false);
    }

    #[test]
    fn setup_shell_idempotent() {
        let dir = tempfile::tempdir().unwrap();
        let profile = dir.path().join(".zshrc");
        let line = nix_path_export(&detect::Shell::Zsh);
        std::fs::write(&profile, &line).unwrap();
        assert!(profile_already_configured(&profile, &line).unwrap());
    }
}
