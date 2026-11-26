use crate::config::NeoConfig;
use crate::errors::NeoError;
use crate::reconcile::source::FullSha;
use miette::IntoDiagnostic;
use serde::Deserialize;
use std::time::Duration;

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct SharedToolchain {
    store_path: String,
    framework_revision: String,
}

pub async fn prepare() -> miette::Result<()> {
    let Some(installed) = neo_lifecycle::installed_project_toolchain() else {
        return Ok(());
    };
    if std::env::var_os("NEO_NEOHASKELL_SOURCE").is_some()
        || std::env::var_os("NEO_SKIP_NETWORK").is_some()
    {
        return Ok(());
    }
    let config = NeoConfig::load("neo.json")?;
    if crate::network::starter_neohaskell_rev().as_deref() == Some(config.neo_version.as_str()) {
        return retain(installed.display().to_string(), config.neo_version).await;
    }

    let os = match std::env::consts::OS {
        "macos" => "darwin",
        os => os,
    };
    let platform = format!("{}-{os}", std::env::consts::ARCH);
    let attr = format!(".#.neoToolchains.{platform}");
    let mut command = tokio::process::Command::new("nix");
    command
        .args(["eval", "--json", "--no-write-lock-file", &attr])
        .kill_on_drop(true);
    let output = tokio::time::timeout(Duration::from_secs(120), command.output())
        .await
        .map_err(|_| selection_error(&platform, "Nix evaluation exceeded 120 seconds"))?
        .map_err(|error| selection_error(&platform, error.to_string()))?;
    if !output.status.success() {
        return Err(selection_error(
            &platform,
            String::from_utf8_lossy(&output.stderr).trim(),
        ));
    }
    let selection: Option<SharedToolchain> = serde_json::from_slice(&output.stdout)
        .map_err(|error| selection_error(&platform, error.to_string()))?;
    let Some(selection) = selection else {
        return Ok(());
    };
    let revision = FullSha::parse(&selection.framework_revision)?;
    let pin = explicit_release(&config.neo_version)
        .map(str::to_owned)
        .unwrap_or_else(|| revision.to_string());
    retain(selection.store_path, pin).await
}

async fn retain(store_path: String, pin: String) -> miette::Result<()> {
    tokio::task::spawn_blocking(move || {
        neo_lifecycle::ensure_project_toolchain(&store_path, Some(&pin))
    })
    .await
    .into_diagnostic()?
    .map_err(|error| miette::miette!("{error}"))?;
    Ok(())
}

fn explicit_release(version: &str) -> Option<&str> {
    let number = version.strip_prefix('v').unwrap_or(version);
    let parsed = semver::Version::parse(number).ok()?;
    (parsed.pre.is_empty() && parsed.build.is_empty()).then_some(version)
}

fn selection_error(platform: &str, cause: impl Into<String>) -> miette::Report {
    NeoError::SubprocessFailed {
        operation: format!("selecting the project's NeoHaskell toolchain for {platform}"),
        cause: cause.into(),
        fix: "Check that the project flake and its locked inputs evaluate, then retry.".to_string(),
    }
    .into()
}
