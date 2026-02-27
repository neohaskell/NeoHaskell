#[derive(Debug, Clone, PartialEq)]
pub enum Os {
    MacOS,
    Linux,
    Unsupported(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arch {
    Aarch64,
    X86_64,
    Unsupported(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Shell {
    Zsh,
    Bash,
    Fish,
    Unknown(String),
}

#[derive(Debug, Clone)]
pub enum NixStatus {
    NotInstalled,
    DeterminateInstalled { version: String },
    DeterminateConflict,
    UpstreamInstalled,
    NixOs,
    NixDarwinPresent,
    BrokenInstall,
}

pub const NIX_BIN: &str = "/nix/var/nix/profiles/default/bin/nix";
pub const NIX_RECEIPT: &str = "/nix/receipt.json";
pub const NIXOS_MARKER: &str = "/etc/NIXOS";

pub fn detect_os() -> Os {
    match std::env::consts::OS {
        "macos" => Os::MacOS,
        "linux" => Os::Linux,
        other => Os::Unsupported(other.to_string()),
    }
}

pub fn detect_arch() -> Arch {
    match std::env::consts::ARCH {
        "aarch64" => Arch::Aarch64,
        "x86_64" => Arch::X86_64,
        other => Arch::Unsupported(other.to_string()),
    }
}

pub fn get_shell() -> Shell {
    match std::env::var("SHELL") {
        Ok(shell) => parse_shell(&shell),
        Err(_) => Shell::Unknown(String::new()),
    }
}

fn parse_shell(shell: &str) -> Shell {
    if shell.ends_with("/zsh") || shell == "zsh" {
        Shell::Zsh
    } else if shell.ends_with("/bash") || shell == "bash" {
        Shell::Bash
    } else if shell.ends_with("/fish") || shell == "fish" {
        Shell::Fish
    } else {
        Shell::Unknown(shell.to_string())
    }
}

pub fn detect_nix_status() -> NixStatus {
    // 1. Check /etc/NIXOS → NixOs
    if std::path::Path::new(NIXOS_MARKER).exists() {
        return NixStatus::NixOs;
    }

    // 2. Check for nix-darwin → NixDarwinPresent
    if which::which("darwin-rebuild").is_ok() {
        return NixStatus::NixDarwinPresent;
    }

    // 3. Check /nix/receipt.json → DeterminateInstalled or DeterminateConflict
    if let Ok(contents) = std::fs::read_to_string(NIX_RECEIPT) {
        if let Ok(json) = serde_json::from_str::<serde_json::Value>(&contents) {
            let version = json
                .get("version")
                .and_then(|v| v.as_str())
                .unwrap_or("unknown")
                .to_string();
            return NixStatus::DeterminateInstalled { version };
        }
        return NixStatus::DeterminateConflict;
    }

    // 4. Check NIX_BIN exists and runs --version → UpstreamInstalled
    if std::path::Path::new(NIX_BIN).exists() {
        if let Ok(output) = std::process::Command::new(NIX_BIN)
            .arg("--version")
            .output()
        {
            if output.status.success() {
                return NixStatus::UpstreamInstalled;
            }
        }
        return NixStatus::BrokenInstall;
    }

    // 5. Check /nix directory exists → BrokenInstall
    if std::path::Path::new("/nix").exists() {
        return NixStatus::BrokenInstall;
    }

    // 6. Nothing found → NotInstalled
    NixStatus::NotInstalled
}

pub fn neo_installed() -> bool {
    which::which("neo").is_ok()
}

pub fn is_rosetta() -> bool {
    if std::env::consts::OS != "macos" {
        return false;
    }
    match std::process::Command::new("sysctl")
        .args(["-n", "sysctl.proc_translated"])
        .output()
    {
        Ok(output) => String::from_utf8_lossy(&output.stdout).trim() == "1",
        Err(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detect_os_returns_valid_os() {
        let os = detect_os();
        #[cfg(target_os = "macos")]
        assert_eq!(os, Os::MacOS);
        #[cfg(target_os = "linux")]
        assert_eq!(os, Os::Linux);
    }

    #[test]
    fn detect_arch_returns_valid_arch() {
        let arch = detect_arch();
        #[cfg(target_arch = "aarch64")]
        assert_eq!(arch, Arch::Aarch64);
        #[cfg(target_arch = "x86_64")]
        assert_eq!(arch, Arch::X86_64);
    }

    #[test]
    fn get_shell_returns_known_variant() {
        let shell = get_shell();
        match shell {
            Shell::Zsh | Shell::Bash | Shell::Fish | Shell::Unknown(_) => {}
        }
    }

    #[test]
    fn os_enum_has_three_variants() {
        let _macos = Os::MacOS;
        let _linux = Os::Linux;
        let _unsupported = Os::Unsupported("test".into());
    }

    #[test]
    fn arch_enum_has_three_variants() {
        let _aarch64 = Arch::Aarch64;
        let _x86 = Arch::X86_64;
        let _unsupported = Arch::Unsupported("test".into());
    }

    #[test]
    fn shell_enum_has_four_variants() {
        let _zsh = Shell::Zsh;
        let _bash = Shell::Bash;
        let _fish = Shell::Fish;
        let _unknown = Shell::Unknown("sh".into());
    }

    #[test]
    fn nix_status_has_seven_variants() {
        let _a = NixStatus::NotInstalled;
        let _b = NixStatus::DeterminateInstalled {
            version: "1.0".into(),
        };
        let _c = NixStatus::DeterminateConflict;
        let _d = NixStatus::UpstreamInstalled;
        let _e = NixStatus::NixOs;
        let _f = NixStatus::NixDarwinPresent;
        let _g = NixStatus::BrokenInstall;
    }

    #[test]
    fn os_clone_and_debug() {
        let os = Os::MacOS;
        let cloned = os.clone();
        assert_eq!(os, cloned);
        let _ = format!("{os:?}");
    }

    #[test]
    fn arch_clone_and_debug() {
        let arch = Arch::Aarch64;
        let cloned = arch.clone();
        assert_eq!(arch, cloned);
        let _ = format!("{arch:?}");
    }

    #[test]
    fn shell_parse_zsh() {
        assert_eq!(parse_shell("/bin/zsh"), Shell::Zsh);
    }

    #[test]
    fn shell_parse_bash() {
        assert_eq!(parse_shell("/usr/bin/bash"), Shell::Bash);
    }

    #[test]
    fn shell_parse_fish() {
        assert_eq!(parse_shell("/usr/local/bin/fish"), Shell::Fish);
    }

    #[test]
    fn shell_parse_unknown() {
        assert_eq!(parse_shell("/bin/dash"), Shell::Unknown("/bin/dash".into()));
    }

    #[test]
    fn detect_nix_status_returns_a_status() {
        let status = detect_nix_status();
        let _ = format!("{status:?}");
    }

    #[test]
    fn neo_installed_returns_bool() {
        assert!(!neo_installed());
    }

    #[test]
    fn is_rosetta_returns_false_on_native() {
        assert!(!is_rosetta());
    }

    #[test]
    fn nix_bin_constant_is_absolute_path() {
        assert!(NIX_BIN.starts_with('/'));
    }

    #[test]
    fn nix_receipt_constant_is_absolute_path() {
        assert!(NIX_RECEIPT.starts_with('/'));
    }

    #[test]
    fn nixos_marker_constant_is_absolute_path() {
        assert!(NIXOS_MARKER.starts_with('/'));
    }
}
