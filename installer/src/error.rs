use thiserror::Error;

#[derive(Error, Debug)]
pub enum InstallerError {
    #[error("Failed to install toolchain: {details}\nReport issues: https://github.com/neohaskell/NeoHaskell/issues")]
    NixInstallFailed { details: String },

    #[error("Failed to install Neo CLI: {details}\nReport issues: https://github.com/neohaskell/NeoHaskell/issues")]
    NeoInstallFailed { details: String },

    #[error("Installation verification failed: {details}\nReport issues: https://github.com/neohaskell/NeoHaskell/issues")]
    VerificationFailed { details: String },

    #[error("Unsupported platform: {os} {arch}")]
    UnsupportedPlatform { os: String, arch: String },

    #[error("Command execution failed: {0}")]
    CommandFailed(#[from] std::io::Error),

    #[error("Existing non-Determinate Nix installation detected. Please uninstall it first.\nSee: https://github.com/neohaskell/NeoHaskell/issues")]
    NixAlreadyExists,

    #[error(
        "nix-darwin detected. Please uninstall nix-darwin before proceeding.\nSee: https://github.com/neohaskell/NeoHaskell/issues"
    )]
    NixDarwinDetected,

    #[error("Broken Nix installation detected at /nix. Please remove it and try again.\nSee: https://github.com/neohaskell/NeoHaskell/issues")]
    BrokenNixInstall,
}

impl InstallerError {
    pub fn exit_code(&self) -> i32 {
        match self {
            Self::VerificationFailed { .. } => 2,
            _ => 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nix_install_failed_display_contains_issues_url() {
        let err = InstallerError::NixInstallFailed {
            details: "timeout".into(),
        };
        let msg = err.to_string();
        assert!(msg.contains("timeout"));
        assert!(msg.contains("https://github.com/neohaskell/NeoHaskell/issues"));
    }

    #[test]
    fn neo_install_failed_display_contains_issues_url() {
        let err = InstallerError::NeoInstallFailed {
            details: "network error".into(),
        };
        let msg = err.to_string();
        assert!(msg.contains("network error"));
        assert!(msg.contains("https://github.com/neohaskell/NeoHaskell/issues"));
    }

    #[test]
    fn verification_failed_display_contains_issues_url() {
        let err = InstallerError::VerificationFailed {
            details: "neo not found".into(),
        };
        let msg = err.to_string();
        assert!(msg.contains("neo not found"));
        assert!(msg.contains("https://github.com/neohaskell/NeoHaskell/issues"));
    }

    #[test]
    fn unsupported_platform_display() {
        let err = InstallerError::UnsupportedPlatform {
            os: "windows".into(),
            arch: "x86_64".into(),
        };
        assert_eq!(err.to_string(), "Unsupported platform: windows x86_64");
    }

    #[test]
    fn command_failed_wraps_io_error() {
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "cmd not found");
        let err: InstallerError = io_err.into();
        assert!(err.to_string().contains("cmd not found"));
    }

    #[test]
    fn nix_already_exists_display() {
        let err = InstallerError::NixAlreadyExists;
        let msg = err.to_string();
        assert!(msg.contains("non-Determinate Nix"));
        assert!(msg.contains("https://github.com/neohaskell/NeoHaskell/issues"));
    }

    #[test]
    fn nix_darwin_detected_display() {
        let err = InstallerError::NixDarwinDetected;
        let msg = err.to_string();
        assert!(msg.contains("nix-darwin"));
        assert!(msg.contains("https://github.com/neohaskell/NeoHaskell/issues"));
    }

    #[test]
    fn broken_nix_install_display() {
        let err = InstallerError::BrokenNixInstall;
        let msg = err.to_string();
        assert!(msg.contains("Broken Nix"));
        assert!(msg.contains("https://github.com/neohaskell/NeoHaskell/issues"));
    }

    #[test]
    fn exit_code_verification_failed_returns_2() {
        let err = InstallerError::VerificationFailed {
            details: "test".into(),
        };
        assert_eq!(err.exit_code(), 2);
    }

    #[test]
    fn exit_code_install_errors_return_1() {
        let cases: Vec<InstallerError> = vec![
            InstallerError::NixInstallFailed {
                details: "x".into(),
            },
            InstallerError::NeoInstallFailed {
                details: "x".into(),
            },
            InstallerError::UnsupportedPlatform {
                os: "x".into(),
                arch: "y".into(),
            },
            InstallerError::NixAlreadyExists,
            InstallerError::NixDarwinDetected,
            InstallerError::BrokenNixInstall,
        ];
        for err in cases {
            assert_eq!(err.exit_code(), 1, "Expected exit code 1 for {err:?}");
        }
    }

    #[test]
    fn command_failed_exit_code_returns_1() {
        let io_err = std::io::Error::new(std::io::ErrorKind::Other, "fail");
        let err = InstallerError::CommandFailed(io_err);
        assert_eq!(err.exit_code(), 1);
    }
}
