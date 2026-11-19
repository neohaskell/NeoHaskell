use super::{InputIdentity, NixBlock, TransactionError, protected_path};
use std::fs::{File, OpenOptions};
use std::io;
use std::os::fd::{AsRawFd, FromRawFd};
use std::os::unix::fs::OpenOptionsExt;
use std::path::Path;

#[derive(Debug, PartialEq, Eq, serde::Serialize)]
pub(crate) enum KernelDaemonObservation {
    Observed(ProcessStamp),
    Inaccessible,
}

#[derive(Debug, PartialEq, Eq, serde::Serialize)]
pub(crate) struct ProcessStamp {
    pid: u32,
    start_ticks: u64,
    effective_uid: u32,
    executable: ExecutableObservation,
    launch_configuration: LaunchConfiguration,
}

#[derive(Debug, PartialEq, Eq, serde::Serialize)]
enum ExecutableObservation {
    Verified(InputIdentity),
    Inaccessible,
}

#[derive(Debug, PartialEq, Eq, serde::Serialize)]
enum LaunchConfiguration {
    Unobserved,
    ConventionalSystem { user_sources: UserSources },
    AlternateOrAmbiguous,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize)]
pub(super) enum UserSources {
    Disabled,
    Stock(StockHome),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize)]
pub(super) enum StockHome {
    Environment,
    Passwd,
}

fn launch_configuration(bytes: &[u8]) -> LaunchConfiguration {
    const KEYS: [&[u8]; 7] = [
        b"NIX_CONF_DIR",
        b"NIX_USER_CONF_FILES",
        b"NIX_CONFIG",
        b"NIX_CONFIG_HOME",
        b"XDG_CONFIG_HOME",
        b"XDG_CONFIG_DIRS",
        b"HOME",
    ];
    if bytes.len() > 64 * 1024 || (!bytes.is_empty() && bytes.last() != Some(&0)) {
        return LaunchConfiguration::AlternateOrAmbiguous;
    }
    let mut values = [None; 7];
    for entry in bytes
        .strip_suffix(&[0])
        .unwrap_or(bytes)
        .split(|byte| *byte == 0)
        .filter(|_| !bytes.is_empty())
    {
        let Some(equal) = entry.iter().position(|byte| *byte == b'=') else {
            return LaunchConfiguration::AlternateOrAmbiguous;
        };
        let key = &entry[..equal];
        if key.is_empty() {
            return LaunchConfiguration::AlternateOrAmbiguous;
        }
        if let Some(index) = KEYS.iter().position(|candidate| *candidate == key) {
            if values[index].is_some() {
                return LaunchConfiguration::AlternateOrAmbiguous;
            }
            values[index] = Some(&entry[equal + 1..]);
        }
    }
    if !matches!(values[0], None | Some(b"" | b"/etc/nix"))
        || !matches!(values[2], None | Some(b""))
    {
        return LaunchConfiguration::AlternateOrAmbiguous;
    }
    let user_sources = match values[1] {
        Some(b"") => UserSources::Disabled,
        None if values[3..6].iter().all(Option::is_none)
            && matches!(values[6], None | Some(b"/root")) =>
        {
            UserSources::Stock(if values[6].is_some() {
                StockHome::Environment
            } else {
                StockHome::Passwd
            })
        }
        _ => return LaunchConfiguration::AlternateOrAmbiguous,
    };
    LaunchConfiguration::ConventionalSystem { user_sources }
}

fn observe_launch_configuration(
    directory: &File,
    observer_uid: u32,
    executable: &ExecutableObservation,
    read: &mut impl FnMut(&mut File) -> Result<Vec<u8>, TransactionError>,
) -> Result<LaunchConfiguration, TransactionError> {
    if observer_uid != 0 {
        return Ok(LaunchConfiguration::Unobserved);
    }
    let ExecutableObservation::Verified(expected) = executable else {
        return Ok(LaunchConfiguration::Unobserved);
    };
    let mut environment = match open_at(
        directory,
        c"environ",
        libc::O_RDONLY | libc::O_NOFOLLOW | libc::O_NONBLOCK,
    ) {
        Ok(file) => file,
        Err(error) if inaccessible(&error) => return Ok(LaunchConfiguration::Unobserved),
        Err(_) => return Err(mismatch()),
    };
    let environment = read(&mut environment)?;
    let actual = open_at(directory, c"exe", libc::O_RDONLY | libc::O_NONBLOCK)
        .and_then(|file| file.metadata())
        .map_err(|_| mismatch())?;
    if !actual.is_file() || InputIdentity::of(&actual) != *expected {
        return Err(mismatch());
    }
    Ok(launch_configuration(&environment))
}

fn mismatch() -> TransactionError {
    TransactionError::NixBlocked(NixBlock::DaemonMismatch)
}

fn open_at(directory: &File, name: &std::ffi::CStr, flags: i32) -> io::Result<File> {
    let descriptor = unsafe {
        libc::openat(
            directory.as_raw_fd(),
            name.as_ptr(),
            flags | libc::O_CLOEXEC,
        )
    };
    if descriptor < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(unsafe { File::from_raw_fd(descriptor) })
    }
}

fn inaccessible(error: &io::Error) -> bool {
    matches!(
        error.kind(),
        io::ErrorKind::PermissionDenied | io::ErrorKind::NotFound
    )
}

fn stamp(stat: &[u8], status: &[u8], pid: u32) -> Result<(u64, u32), TransactionError> {
    let opening = stat.iter().position(|b| *b == b'(').ok_or_else(mismatch)?;
    let closing = stat.iter().rposition(|b| *b == b')').ok_or_else(mismatch)?;
    if opening >= closing || stat.get(opening.wrapping_sub(1)) != Some(&b' ') {
        return Err(mismatch());
    }
    let actual = std::str::from_utf8(&stat[..opening - 1])
        .ok()
        .and_then(|v| v.parse::<u32>().ok());
    let fields: Vec<_> = stat[closing + 1..]
        .split(|b| b.is_ascii_whitespace())
        .filter(|v| !v.is_empty())
        .collect();
    if actual != Some(pid)
        || !matches!(
            fields.first().copied(),
            Some(b"R" | b"S" | b"D" | b"T" | b"t" | b"I")
        )
    {
        return Err(mismatch());
    }
    let start = fields
        .get(19)
        .and_then(|v| std::str::from_utf8(v).ok())
        .and_then(|v| v.parse::<u64>().ok())
        .filter(|v| *v > 0)
        .ok_or_else(mismatch)?;
    let mut status_pid = None;
    let mut uid = None;
    for line in status.split(|b| *b == b'\n') {
        if let Some(value) = line.strip_prefix(b"Pid:") {
            if status_pid.is_some() {
                return Err(mismatch());
            }
            status_pid = Some(
                std::str::from_utf8(value)
                    .ok()
                    .and_then(|v| v.trim().parse::<u32>().ok())
                    .ok_or_else(mismatch)?,
            );
        }
        if let Some(value) = line.strip_prefix(b"Uid:") {
            if uid.is_some() {
                return Err(mismatch());
            }
            let values = std::str::from_utf8(value)
                .map_err(|_| mismatch())?
                .split_ascii_whitespace()
                .map(str::parse::<u32>)
                .collect::<Result<Vec<_>, _>>()
                .map_err(|_| mismatch())?;
            if values.len() != 4 {
                return Err(mismatch());
            }
            uid = Some(values[1]);
        }
    }
    if status_pid != Some(pid) || uid != Some(0) {
        return Err(mismatch());
    }
    Ok((start, 0))
}

fn verify_executable(
    actual: &std::fs::Metadata,
    expected: &std::fs::Metadata,
) -> Result<ExecutableObservation, TransactionError> {
    if !actual.is_file() || InputIdentity::of(actual) != InputIdentity::of(expected) {
        return Err(mismatch());
    }
    Ok(ExecutableObservation::Verified(InputIdentity::of(actual)))
}

pub(crate) fn observe(
    pid: u32,
    executable: &Path,
    mut read: impl FnMut(&mut File) -> Result<Vec<u8>, TransactionError>,
) -> Result<KernelDaemonObservation, TransactionError> {
    if pid <= 1 {
        return Err(mismatch());
    }
    let directory = match OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW | libc::O_DIRECTORY | libc::O_CLOEXEC)
        .open(format!("/proc/{pid}"))
    {
        Ok(directory) => directory,
        Err(error) if inaccessible(&error) => return Ok(KernelDaemonObservation::Inaccessible),
        Err(_) => return Err(mismatch()),
    };
    let snapshot = |read: &mut dyn FnMut(&mut File) -> Result<Vec<u8>, TransactionError>| -> Result<Option<(u64, u32)>, TransactionError> {
        let open = |name| match open_at(
            &directory,
            name,
            libc::O_RDONLY | libc::O_NOFOLLOW | libc::O_NONBLOCK,
        ) {
            Ok(file) => Ok(Some(file)),
            Err(error) if inaccessible(&error) => Ok(None),
            Err(_) => Err(mismatch()),
        };
        let Some(mut stat) = open(c"stat")? else {
            return Ok(None);
        };
        let Some(mut status) = open(c"status")? else {
            return Ok(None);
        };
        Ok(Some(stamp(&read(&mut stat)?, &read(&mut status)?, pid)?))
    };
    let Some(before) = snapshot(&mut read)? else {
        return Ok(KernelDaemonObservation::Inaccessible);
    };
    let executable = match open_at(&directory, c"exe", libc::O_RDONLY | libc::O_NONBLOCK) {
        Ok(actual) => {
            let expected_path = protected_path(executable).map_err(|_| mismatch())?;
            let expected = OpenOptions::new()
                .read(true)
                .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK)
                .open(expected_path)
                .map_err(|_| mismatch())?;
            let actual = actual.metadata().map_err(|_| mismatch())?;
            let expected = expected.metadata().map_err(|_| mismatch())?;
            verify_executable(&actual, &expected)?
        }
        Err(error) if error.kind() == io::ErrorKind::PermissionDenied => {
            ExecutableObservation::Inaccessible
        }
        Err(_) => return Err(mismatch()),
    };
    let launch_configuration = observe_launch_configuration(
        &directory,
        unsafe { libc::geteuid() },
        &executable,
        &mut read,
    )?;
    if Some(before) != snapshot(&mut read)? {
        return Err(mismatch());
    }
    Ok(KernelDaemonObservation::Observed(ProcessStamp {
        pid,
        start_ticks: before.0,
        effective_uid: before.1,
        executable,
        launch_configuration,
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn launch_selectors_preserve_user_search_uncertainty() {
        for bytes in [
            b"".as_slice(),
            b"NIX_CONF_DIR=/etc/nix\0NIX_CONFIG=\0",
            b"NIX_CONF_DIR=\0",
        ] {
            assert_eq!(
                launch_configuration(bytes),
                LaunchConfiguration::ConventionalSystem {
                    user_sources: UserSources::Stock(StockHome::Passwd),
                }
            );
        }
        assert_eq!(
            launch_configuration(b"HOME=/root\0"),
            LaunchConfiguration::ConventionalSystem {
                user_sources: UserSources::Stock(StockHome::Environment),
            }
        );
        for bytes in [
            b"NIX_USER_CONF_FILES=\0".as_slice(),
            b"NIX_USER_CONF_FILES=\0HOME=/private\0XDG_CONFIG_DIRS=/private\0",
        ] {
            assert_eq!(
                launch_configuration(bytes),
                LaunchConfiguration::ConventionalSystem {
                    user_sources: UserSources::Disabled,
                }
            );
        }
    }

    #[test]
    fn alternate_duplicate_and_malformed_selectors_never_claim_conventional_inputs() {
        for bytes in [
            b"NIX_CONF_DIR=/secret\0".as_slice(),
            b"NIX_CONFIG=secret = true\0",
            b"NIX_USER_CONF_FILES=/secret\0",
            b"NIX_CONFIG_HOME=/secret\0",
            b"XDG_CONFIG_HOME=/secret\0",
            b"XDG_CONFIG_DIRS=\0",
            b"HOME=/secret\0",
            b"HOME=\0",
            b"NIX_CONFIG=\0NIX_CONFIG=\0",
            b"NIX_CONF_DIR=/etc/nix\0NIX_CONF_DIR=/etc/nix\0",
            b"NIX_USER_CONF_FILES=\0NIX_USER_CONF_FILES=\0",
            b"HOME=/root\0HOME=/root\0",
            b"NIX_CONFIG",
            b"NIX_CONFIG\0",
            b"=secret\0",
            b"\0",
            b"NIX_CONFIG=\0\0",
            b"NIX_CONFIG=\xff\0",
        ] {
            let observation = launch_configuration(bytes);
            assert_eq!(observation, LaunchConfiguration::AlternateOrAmbiguous);
            assert_eq!(format!("{observation:?}"), "AlternateOrAmbiguous");
        }
        assert_eq!(
            launch_configuration(&vec![0; 65537]),
            LaunchConfiguration::AlternateOrAmbiguous
        );
        assert_eq!(
            launch_configuration(b"UNRELATED=secret\xff\0"),
            LaunchConfiguration::ConventionalSystem {
                user_sources: UserSources::Stock(StockHome::Passwd),
            }
        );
    }

    fn fixture_executable(directory: &std::path::Path) -> ExecutableObservation {
        let path = directory.join("executable");
        std::fs::write(&path, b"fixture").unwrap();
        ExecutableObservation::Verified(InputIdentity::of(&std::fs::metadata(path).unwrap()))
    }

    #[test]
    fn ordinary_or_unverified_observers_do_not_read_environment() {
        let temporary = tempfile::tempdir().unwrap();
        let directory = File::open(temporary.path()).unwrap();
        for (uid, executable) in [
            (1000, fixture_executable(temporary.path())),
            (0, ExecutableObservation::Inaccessible),
        ] {
            assert_eq!(
                observe_launch_configuration(&directory, uid, &executable, &mut |_| {
                    panic!("unqualified observation read environment")
                })
                .unwrap(),
                LaunchConfiguration::Unobserved
            );
        }
    }

    #[test]
    fn synthetic_environment_preserves_cancellation_and_descriptor_binding() {
        use std::io::Read;
        let temporary = tempfile::tempdir().unwrap();
        let process = temporary.path().join("process");
        std::fs::create_dir(&process).unwrap();
        let directory = File::open(&process).unwrap();
        let executable = fixture_executable(temporary.path());
        std::os::unix::fs::symlink(temporary.path().join("executable"), process.join("exe"))
            .unwrap();
        assert_eq!(
            observe_launch_configuration(&directory, 0, &executable, &mut |_| panic!(
                "missing environment read"
            ))
            .unwrap(),
            LaunchConfiguration::Unobserved
        );
        std::fs::write(process.join("environ"), b"NIX_USER_CONF_FILES=\0").unwrap();
        assert!(matches!(
            observe_launch_configuration(&directory, 0, &executable, &mut |_| Err(
                TransactionError::StopRequested
            )),
            Err(TransactionError::StopRequested)
        ));
        let retained = temporary.path().join("retained");
        std::fs::rename(&process, &retained).unwrap();
        std::fs::create_dir(&process).unwrap();
        std::fs::write(process.join("environ"), b"NIX_CONFIG=secret\0").unwrap();
        let mut read = |file: &mut File| {
            let mut bytes = Vec::new();
            file.read_to_end(&mut bytes).unwrap();
            Ok(bytes)
        };
        let baseline = observe_launch_configuration(&directory, 0, &executable, &mut read).unwrap();
        assert_eq!(
            baseline,
            LaunchConfiguration::ConventionalSystem {
                user_sources: UserSources::Disabled
            }
        );
        let baseline = ProcessStamp {
            pid: 91,
            start_ticks: 123,
            effective_uid: 0,
            executable,
            launch_configuration: baseline,
        };
        std::fs::write(retained.join("environ"), b"NIX_CONFIG=private-value\0").unwrap();
        let changed = ProcessStamp {
            pid: 91,
            start_ticks: 123,
            effective_uid: 0,
            executable: ExecutableObservation::Verified(InputIdentity::of(
                &std::fs::metadata(temporary.path().join("executable")).unwrap(),
            )),
            launch_configuration: observe_launch_configuration(
                &directory,
                0,
                &baseline.executable,
                &mut read,
            )
            .unwrap(),
        };
        assert_eq!(baseline.executable, changed.executable);
        assert_ne!(baseline.launch_configuration, changed.launch_configuration);
        assert_ne!(baseline, changed);
        assert!(!format!("{changed:?}").contains("private-value"));
    }

    #[test]
    fn changed_executable_invalidates_the_environment_read() {
        use std::io::Read;
        let temporary = tempfile::tempdir().unwrap();
        let executable = fixture_executable(temporary.path());
        let proc_exe = temporary.path().join("exe");
        std::os::unix::fs::symlink(temporary.path().join("executable"), &proc_exe).unwrap();
        let replacement = temporary.path().join("replacement");
        std::fs::write(&replacement, b"fixture").unwrap();
        std::fs::write(temporary.path().join("environ"), b"NIX_USER_CONF_FILES=\0").unwrap();
        let directory = File::open(temporary.path()).unwrap();

        let result = observe_launch_configuration(&directory, 0, &executable, &mut |file| {
            let mut bytes = Vec::new();
            file.read_to_end(&mut bytes).unwrap();
            std::fs::rename(&replacement, &proc_exe).unwrap();
            Ok(bytes)
        });

        assert!(matches!(
            result,
            Err(TransactionError::NixBlocked(NixBlock::DaemonMismatch))
        ));
    }

    fn stat(comm: &str) -> Vec<u8> {
        format!("91 ({comm}) S {} 98765 88888\n", vec!["0"; 18].join(" ")).into_bytes()
    }

    #[test]
    fn field_22_survives_odd_comm_and_does_not_use_field_23() {
        for comm in ["nix-daemon", "odd ) ( name\n"] {
            assert_eq!(
                stamp(&stat(comm), b"Pid:\t91\nUid:\t0 0 0 0\n", 91).unwrap(),
                (98765, 0)
            );
        }
    }

    #[test]
    fn truncated_foreign_dead_and_nonroot_processes_are_rejected() {
        for (stat, status, pid) in [
            (
                b"91 (nix) S 0".to_vec(),
                b"Pid: 91\nUid: 0 0 0 0".as_slice(),
                91,
            ),
            (stat("nix"), b"Pid: 92\nUid: 0 0 0 0", 91),
            (stat("nix"), b"Pid: 91\nUid: 0 1000 0 0", 91),
            (stat("nix"), b"Pid: 91\nUid: 0 0 0", 91),
            (stat("nix"), b"Pid: 91\nUid: 0 0 0 0", 92),
            (
                String::from_utf8(stat("nix"))
                    .unwrap()
                    .replace(") S", ") Z")
                    .into_bytes(),
                b"Pid: 91\nUid: 0 0 0 0",
                91,
            ),
        ] {
            assert!(stamp(&stat, status, pid).is_err());
        }
    }
    #[test]
    fn executable_identity_rejects_byte_identical_replacement() {
        let temporary = tempfile::tempdir().unwrap();
        let path = temporary.path().join("nix");
        std::fs::write(&path, b"same executable").unwrap();
        let old = File::open(&path).unwrap();
        let replacement = temporary.path().join("replacement");
        std::fs::write(&replacement, b"same executable").unwrap();
        std::fs::rename(&replacement, &path).unwrap();
        assert!(
            verify_executable(&old.metadata().unwrap(), &std::fs::metadata(&path).unwrap())
                .is_err()
        );
    }

    #[test]
    fn visibility_and_process_start_drift_are_distinct_evidence() {
        let observed = |start_ticks, executable| {
            KernelDaemonObservation::Observed(ProcessStamp {
                pid: 91,
                start_ticks,
                effective_uid: 0,
                executable,
                launch_configuration: LaunchConfiguration::Unobserved,
            })
        };
        let baseline = observed(123, ExecutableObservation::Inaccessible);
        assert_eq!(baseline, observed(123, ExecutableObservation::Inaccessible));
        assert_ne!(baseline, observed(124, ExecutableObservation::Inaccessible));
        assert_ne!(baseline, KernelDaemonObservation::Inaccessible);
        let temporary = tempfile::NamedTempFile::new().unwrap();
        let metadata = temporary.as_file().metadata().unwrap();
        let verified = verify_executable(&metadata, &metadata).unwrap();
        assert_ne!(baseline, observed(123, verified));
    }

    #[test]
    fn proc_fields_do_not_follow_links_out_of_the_opened_directory() {
        let temporary = tempfile::tempdir().unwrap();
        let directory = File::open(temporary.path()).unwrap();
        let elsewhere = temporary.path().join("elsewhere");
        std::fs::write(&elsewhere, b"unrelated").unwrap();
        std::os::unix::fs::symlink(&elsewhere, temporary.path().join("stat")).unwrap();
        assert!(open_at(&directory, c"stat", libc::O_RDONLY | libc::O_NOFOLLOW).is_err());
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn process_read_preserves_cancellation_before_executable_observation() {
        let result = observe(std::process::id(), Path::new("/unused"), |_| {
            Err(TransactionError::StopRequested)
        });
        assert!(matches!(result, Err(TransactionError::StopRequested)));
    }
}

impl KernelDaemonObservation {
    pub(super) fn conventional_user_sources(&self) -> Option<UserSources> {
        match self {
            Self::Observed(ProcessStamp {
                executable: ExecutableObservation::Verified(_),
                launch_configuration: LaunchConfiguration::ConventionalSystem { user_sources },
                ..
            }) => Some(*user_sources),
            _ => None,
        }
    }
}
