pub mod detect;
pub mod error;
pub mod install;
pub mod ui;
pub mod verify;

use clap::Parser;

#[derive(Parser)]
#[command(
    name = "neo-install",
    version,
    about = "Install NeoHaskell on your system",
    after_help = "Source: https://github.com/neohaskell/NeoHaskell"
)]
pub struct Cli {
    #[arg(long, help = "Show what would be done without doing it")]
    pub dry_run: bool,

    #[arg(
        long,
        short,
        help = "Show detailed output including toolchain installer output"
    )]
    pub verbose: bool,

    #[arg(long, help = "Reinstall even if already installed")]
    pub force: bool,
}

pub fn run(cli: &Cli) -> anyhow::Result<()> {
    // Step 1: Platform checks
    let os = detect::detect_os();
    let arch = detect::detect_arch();

    if let detect::Os::Unsupported(ref name) = os {
        return Err(error::InstallerError::UnsupportedPlatform {
            os: name.clone(),
            arch: format!("{arch:?}"),
        }
        .into());
    }
    if let detect::Arch::Unsupported(ref name) = arch {
        return Err(error::InstallerError::UnsupportedPlatform {
            os: format!("{os:?}"),
            arch: name.clone(),
        }
        .into());
    }

    if detect::is_rosetta() {
        return Err(error::InstallerError::UnsupportedPlatform {
            os: "macOS".into(),
            arch:
                "x86_64 (Rosetta emulation detected \u{2014} please use the Apple Silicon binary)"
                    .into(),
        }
        .into());
    }

    // Step 2: Check Nix status
    let nix_status = detect::detect_nix_status();

    if cli.verbose || cli.dry_run {
        ui::print_step(&format!("Detected toolchain status: {nix_status:?}"), true);
    }

    match nix_status {
        detect::NixStatus::NotInstalled => {
            install::install_nix(cli.verbose, cli.dry_run)?;
        }
        detect::NixStatus::DeterminateInstalled { .. } => {
            if cli.force {
                install::install_nix(cli.verbose, cli.dry_run)?;
            } else if cli.verbose || cli.dry_run {
                ui::print_step("Toolchain already installed, skipping", true);
            }
        }
        detect::NixStatus::NixOs => {
            if cli.verbose || cli.dry_run {
                ui::print_step("Running on NixOS, toolchain already present", true);
            }
        }
        detect::NixStatus::UpstreamInstalled => {
            return Err(error::InstallerError::NixAlreadyExists.into());
        }
        detect::NixStatus::NixDarwinPresent => {
            return Err(error::InstallerError::NixDarwinDetected.into());
        }
        detect::NixStatus::BrokenInstall => {
            return Err(error::InstallerError::BrokenNixInstall.into());
        }
        detect::NixStatus::DeterminateConflict => {
            return Err(error::InstallerError::NixAlreadyExists.into());
        }
    }

    // Step 3: Install Neo
    if !detect::neo_installed() || cli.force {
        install::install_neo(cli.verbose, cli.dry_run)?;
    } else if cli.verbose || cli.dry_run {
        ui::print_step("Neo CLI already installed, skipping", true);
    }

    // Step 4: Shell setup
    install::setup_shell(cli.dry_run)?;

    // Step 5: Verify
    verify::verify_nix(cli.dry_run)?;
    verify::verify_neo(cli.dry_run)?;

    ui::print_step(ui::MSG_DONE, true);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_dry_run_returns_ok() {
        let cli = Cli {
            dry_run: true,
            verbose: false,
            force: false,
        };
        // dry_run should always succeed (no side effects)
        assert!(run(&cli).is_ok());
    }

    #[test]
    fn run_dry_run_verbose_returns_ok() {
        let cli = Cli {
            dry_run: true,
            verbose: true,
            force: false,
        };
        assert!(run(&cli).is_ok());
    }

    #[test]
    fn run_dry_run_force_returns_ok() {
        let cli = Cli {
            dry_run: true,
            verbose: false,
            force: true,
        };
        assert!(run(&cli).is_ok());
    }
}
