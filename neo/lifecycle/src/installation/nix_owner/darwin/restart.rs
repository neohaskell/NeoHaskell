use super::publication::{LockedVendorDirectory, PublishedPolicy, RecoveredPublication};
use super::{
    DaemonIncarnation, DarwinPolicyRecovery, DarwinRestartPlan, DiskPolicy, JOB, NixBlock,
    SourceRoute, StartupContext, VendorPublicationRequest, administrator, observe,
    policy_helper_client, read_bounded_child, read_bounded_output,
};
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::io;
use std::mem::MaybeUninit;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};
use std::ptr::null_mut;
use std::time::{Duration, Instant};

const KICKSTART_TIMEOUT: Duration = Duration::from_secs(5);
const REPLACEMENT_TIMEOUT: Duration = Duration::from_secs(8);
const CACHE_PROBE_TIMEOUT: Duration = Duration::from_secs(3);
const UNRELATED_CACHE: &str = "https://neo-policy-control.invalid";

pub(super) fn inspect(request: &VendorPublicationRequest) -> DarwinPolicyRecovery {
    let result = || {
        let directory = LockedVendorDirectory::lock().ok()?;
        let observation = match directory.recover(request).ok()? {
            RecoveredPublication::Prepared => DarwinPolicyRecovery::Prepared,
            RecoveredPublication::LeafPublished => DarwinPolicyRecovery::LeafPublished,
            RecoveredPublication::Published(proof) => {
                let daemon = active_daemon().ok()?;
                if let Some(previous) = proof.restart_from {
                    if !confirms_activation(&directory, request, &proof, daemon, previous) {
                        return None;
                    }
                    DarwinPolicyRecovery::Active
                } else if snapshot_matches(&directory, request, &proof) {
                    DarwinPolicyRecovery::Ready(restart_plan(&proof, daemon))
                } else {
                    return None;
                }
            }
        };
        Some(observation)
    };
    result().unwrap_or(DarwinPolicyRecovery::Unresolved)
}

pub(super) fn apply(
    request: &VendorPublicationRequest,
    reviewed: DarwinRestartPlan,
    authorize: impl Fn() -> bool,
) -> DarwinPolicyRecovery {
    let restart = || {
        let directory = LockedVendorDirectory::lock().ok()?;
        let RecoveredPublication::Published(proof) = directory.recover(request).ok()? else {
            return None;
        };
        let daemon = active_daemon().ok()?;
        if restart_plan(&proof, daemon) != reviewed || proof.restart_from.is_some() || !authorize()
        {
            return None;
        }
        let previous = supervisor_identity(daemon);
        directory.mark_restart(request, &proof, previous).ok()?;
        let expected = PublishedPolicy {
            restart_from: Some(previous),
            ..proof
        };
        if !snapshot_matches(&directory, request, &expected)
            || active_daemon().ok()? != daemon
            || !authorize()
        {
            return None;
        }
        let pid = kickstart().ok()?;
        let replacement = wait_for_replacement(pid)?;
        confirms_activation(&directory, request, &expected, replacement, previous).then_some(())
    };
    if restart().is_some() {
        DarwinPolicyRecovery::Active
    } else {
        DarwinPolicyRecovery::Unresolved
    }
}

fn active_daemon() -> Result<DaemonIncarnation, NixBlock> {
    let value = observe(false)?;
    let observed = value.inspection.observation;
    if observed.startup != StartupContext::StandardSources
        || observed.disk.source != SourceRoute::VendorCustom
        || observed.disk.policy != DiskPolicy::Satisfies
    {
        return Err(NixBlock::ConfigurationOwnerUnknown);
    }
    Ok(value.daemon)
}

fn restart_plan(proof: &PublishedPolicy, daemon: DaemonIncarnation) -> DarwinRestartPlan {
    let mut hash = Sha256::new();
    hash.update(b"neo:determinate-darwin:restart:v1\0");
    hash.update(proof.fingerprint);
    hash.update(daemon.fingerprint);
    DarwinRestartPlan(hash.finalize().into())
}

fn supervisor_identity(daemon: DaemonIncarnation) -> [u8; 32] {
    let mut hash = Sha256::new();
    hash.update(b"neo:determinate-darwin:supervisor:v1\0");
    for field in daemon.supervisor.0 {
        hash.update(field.to_le_bytes());
    }
    hash.finalize().into()
}

fn snapshot_matches(
    directory: &LockedVendorDirectory,
    request: &VendorPublicationRequest,
    expected: &PublishedPolicy,
) -> bool {
    matches!(directory.recover(request), Ok(RecoveredPublication::Published(actual))
        if actual.fingerprint == expected.fingerprint && actual.restart_from == expected.restart_from)
}

fn confirms_activation(
    directory: &LockedVendorDirectory,
    request: &VendorPublicationRequest,
    expected: &PublishedPolicy,
    daemon: DaemonIncarnation,
    previous: [u8; 32],
) -> bool {
    supervisor_identity(daemon) != previous
        && !administrator::cancelled()
        && cache_admission()
        && active_daemon().is_ok_and(|current| current == daemon)
        && snapshot_matches(directory, request, expected)
}

fn kickstart() -> Result<u32, NixBlock> {
    let mut child = Command::new("/bin/launchctl")
        .args(["kickstart", "-kp", JOB])
        .env_clear()
        .current_dir("/")
        .stdin(Stdio::null())
        .stderr(Stdio::null())
        .stdout(Stdio::piped())
        .spawn()
        .map_err(|_| NixBlock::UnprovedService)?;
    let output = read_bounded_child(&mut child, KICKSTART_TIMEOUT);
    if output.is_err() {
        let _ = child.kill();
        let _ = child.wait();
    }
    let output = output?;
    let raw = std::str::from_utf8(&output).map_err(|_| NixBlock::UnprovedService)?;
    let pid = raw
        .trim()
        .parse::<u32>()
        .map_err(|_| NixBlock::UnprovedService)?;
    if pid < 2 || raw.trim() != pid.to_string() || i32::try_from(pid).is_err() {
        return Err(NixBlock::UnprovedService);
    }
    Ok(pid)
}

fn wait_for_replacement(pid: u32) -> Option<DaemonIncarnation> {
    let deadline = Instant::now() + REPLACEMENT_TIMEOUT;
    let mut delay = Duration::from_millis(20);
    while !administrator::cancelled() && Instant::now() < deadline {
        if let Ok(daemon) = active_daemon()
            && daemon.supervisor.pid() == pid
        {
            return Some(daemon);
        }
        std::thread::sleep(delay.min(deadline.saturating_duration_since(Instant::now())));
        delay = (delay * 2).min(Duration::from_millis(500));
    }
    None
}

fn cache_admission() -> bool {
    let check = || {
        let (urls, _) = crate::candidate::nix_cache_settings().ok()?;
        let substituters = format!("{urls} {UNRELATED_CACHE}");
        let (reader, writer) = io::pipe().ok()?;
        let stderr = writer.try_clone().ok()?;
        let (uid, gid) = unprivileged_account()?;
        let mut command = Command::new(policy_helper_client().ok()?);
        command
            .args([
                "--option",
                "substituters",
                &substituters,
                "store",
                "info",
                "--store",
                "daemon",
                "--offline",
                "--json",
            ])
            .env_clear()
            .env("HOME", "/var/empty")
            .env("NIX_CONF_DIR", "/var/empty")
            .env("PATH", "/usr/bin:/bin")
            .env("LC_ALL", "C")
            .current_dir("/")
            .stdin(Stdio::null())
            .stdout(Stdio::from(writer))
            .stderr(Stdio::from(stderr));
        unsafe {
            command.pre_exec(move || {
                if libc::setgroups(1, &gid) != 0 || libc::setgid(gid) != 0 || libc::setuid(uid) != 0
                {
                    return Err(io::Error::last_os_error());
                }
                if (
                    libc::getuid(),
                    libc::geteuid(),
                    libc::getgid(),
                    libc::getegid(),
                ) != (uid, uid, gid, gid)
                {
                    return Err(io::Error::from_raw_os_error(libc::EPERM));
                }
                Ok(())
            });
        }
        let mut child = command.spawn().ok()?;
        drop(command);
        let output = read_bounded_output(&mut child, reader, CACHE_PROBE_TIMEOUT);
        if output.is_err() {
            let _ = child.kill();
            let _ = child.wait();
        }
        let output = output.ok()?;
        Some(cache_probe_confirms(std::str::from_utf8(&output).ok()?))
    };
    check().unwrap_or(false)
}

fn unprivileged_account() -> Option<(libc::uid_t, libc::gid_t)> {
    let mut account = MaybeUninit::<libc::passwd>::uninit();
    let mut result = null_mut();
    let mut buffer = [0; 4096];
    let status = unsafe {
        libc::getpwnam_r(
            c"nobody".as_ptr(),
            account.as_mut_ptr(),
            buffer.as_mut_ptr(),
            buffer.len(),
            &mut result,
        )
    };
    if status != 0 || !std::ptr::eq(result, account.as_ptr()) {
        return None;
    }
    let account = unsafe { account.assume_init() };
    (account.pw_uid != 0 && account.pw_gid != 0).then_some((account.pw_uid, account.pw_gid))
}

#[derive(Deserialize)]
struct StoreInfo<'a> {
    trusted: bool,
    url: &'a str,
    version: &'a str,
}

fn cache_probe_confirms(output: &str) -> bool {
    let mut records = output
        .lines()
        .filter(|line| line.starts_with('{'))
        .map(serde_json::from_str::<StoreInfo<'_>>);
    let Some(Ok(StoreInfo {
        trusted: false,
        url: "daemon",
        version: "2.35.2",
    })) = records.next()
    else {
        return false;
    };
    let mut denied = output.lines().filter_map(|line| {
        line.strip_prefix("warning: ignoring untrusted substituter '")?
            .split_once('\'')
            .map(|(url, _)| url)
    });
    records.next().is_none() && denied.next() == Some(UNRELATED_CACHE) && denied.next().is_none()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cache_probe_requires_an_untrusted_client_and_unrelated_cache_refusal() {
        let record = r#"{"trusted":false,"url":"daemon","version":"2.35.2"}"#;
        let refuses = |cache| {
            format!(
                "warning: ignoring untrusted substituter '{cache}', you are not a trusted user."
            )
        };
        let good = format!("{record}\n{}", refuses(UNRELATED_CACHE));
        assert!(cache_probe_confirms(&good));
        assert!(!cache_probe_confirms(record));
        assert!(!cache_probe_confirms(&good.replace("false", "true")));
        assert!(!cache_probe_confirms(&format!(
            "{good}\n{}",
            refuses("https://cache.iog.io")
        )));
        assert!(!cache_probe_confirms(&format!("{good}\n{record}")));
    }
}
