use thiserror::Error;

#[derive(Error, Debug)]
pub enum InstallerError {
    #[error("Failed to install toolchain: {details}\nReport issues: https://github.com/neohaskell/NeoHaskell/issues")]
    NixInstallFailed { details: String },

    #[error("Failed to install Neo CLI: {details}\nReport issues: https://github.com/neohaskell/NeoHaskell/issues")]
    NeoInstallFailed { details: String },

    #[error(
        "Invalid NEO_VERSION '{value}': {details}.\nSet NEO_VERSION to a Neo release tag like neo-v0.1.0, or unset it to install the newest neo-v* release.\nReleases: https://github.com/neohaskell/NeoHaskell/releases"
    )]
    InvalidNeoVersion { value: String, details: String },

    #[error(
        "Invalid NEO_BIN_DIR '{value}': {details}.\nSet NEO_BIN_DIR to an absolute directory path (e.g. /home/you/.local/bin), or unset it to use the default $HOME/.local/bin."
    )]
    InvalidBinDir { value: String, details: String },

    #[error(
        "Could not determine which Neo CLI release to install: {details}.\nReleases: https://github.com/neohaskell/NeoHaskell/releases"
    )]
    NeoReleaseResolutionFailed { details: String },

    #[error(
        "Failed to download {url}: {details}.\nCheck your network, then retry. If a specific version is missing, pick another with NEO_VERSION=neo-vX.Y.Z.\nReleases: https://github.com/neohaskell/NeoHaskell/releases"
    )]
    NeoDownloadFailed { url: String, details: String },

    #[error(
        "Release {tag} has no SHA256SUMS entry for '{asset}', so its integrity cannot be verified.\nThis usually means the release is incomplete for your platform. Pick another with NEO_VERSION=neo-vX.Y.Z.\nReleases: https://github.com/neohaskell/NeoHaskell/releases"
    )]
    NeoChecksumMissing { asset: String, tag: String },

    #[error(
        "Checksum mismatch for '{asset}': expected {expected}, got {actual}.\nThe download is corrupt or has been tampered with and was NOT installed. Retry the install; if it persists, report it.\nReport issues: https://github.com/neohaskell/NeoHaskell/issues"
    )]
    NeoChecksumMismatch {
        asset: String,
        expected: String,
        actual: String,
    },

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
        let io_err = std::io::Error::other("fail");
        let err = InstallerError::CommandFailed(io_err);
        assert_eq!(err.exit_code(), 1);
    }

    #[test]
    fn invalid_neo_version_message_is_actionable() {
        let err = InstallerError::InvalidNeoVersion {
            value: "v1.2.3".into(),
            details: "must begin with neo-v".into(),
        };
        let msg = err.to_string();
        assert!(msg.contains("v1.2.3"));
        assert!(msg.contains("neo-v0.1.0"));
        assert!(msg.contains("unset it to install the newest"));
    }

    #[test]
    fn checksum_mismatch_quotes_expected_and_actual_and_says_not_installed() {
        let err = InstallerError::NeoChecksumMismatch {
            asset: "neo-x86_64-unknown-linux-gnu".into(),
            expected: "aaaa".into(),
            actual: "bbbb".into(),
        };
        let msg = err.to_string();
        assert!(msg.contains("neo-x86_64-unknown-linux-gnu"));
        assert!(msg.contains("aaaa"));
        assert!(msg.contains("bbbb"));
        assert!(msg.contains("NOT installed"));
    }

    #[test]
    fn checksum_missing_names_asset_and_tag() {
        let err = InstallerError::NeoChecksumMissing {
            asset: "neo-aarch64-apple-darwin".into(),
            tag: "neo-v0.1.0".into(),
        };
        let msg = err.to_string();
        assert!(msg.contains("neo-aarch64-apple-darwin"));
        assert!(msg.contains("neo-v0.1.0"));
    }

    #[test]
    fn download_failed_names_url_and_offers_pin() {
        let err = InstallerError::NeoDownloadFailed {
            url: "https://example/neo".into(),
            details: "404".into(),
        };
        let msg = err.to_string();
        assert!(msg.contains("https://example/neo"));
        assert!(msg.contains("NEO_VERSION=neo-vX.Y.Z"));
    }

    #[test]
    fn release_resolution_failed_points_at_releases() {
        let err = InstallerError::NeoReleaseResolutionFailed {
            details: "no neo-v* release".into(),
        };
        assert!(err.to_string().contains("releases"));
    }

    #[test]
    fn invalid_bin_dir_message_is_actionable() {
        let err = InstallerError::InvalidBinDir {
            value: "relative/bin".into(),
            details: "it must be an absolute path".into(),
        };
        let msg = err.to_string();
        assert!(msg.contains("relative/bin"));
        assert!(msg.contains("absolute directory path"));
        assert!(msg.contains("$HOME/.local/bin"));
    }

    #[test]
    fn new_variants_all_exit_1() {
        let cases: Vec<InstallerError> = vec![
            InstallerError::InvalidNeoVersion {
                value: "x".into(),
                details: "y".into(),
            },
            InstallerError::InvalidBinDir {
                value: "x".into(),
                details: "y".into(),
            },
            InstallerError::NeoReleaseResolutionFailed {
                details: "x".into(),
            },
            InstallerError::NeoDownloadFailed {
                url: "u".into(),
                details: "d".into(),
            },
            InstallerError::NeoChecksumMissing {
                asset: "a".into(),
                tag: "t".into(),
            },
            InstallerError::NeoChecksumMismatch {
                asset: "a".into(),
                expected: "e".into(),
                actual: "c".into(),
            },
        ];
        for err in cases {
            assert_eq!(err.exit_code(), 1, "expected exit 1 for {err:?}");
        }
    }
}
