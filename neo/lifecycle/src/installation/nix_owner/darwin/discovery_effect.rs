use super::administrator::{ATTENDED_ARGUMENT, LAUNCH_ARGUMENT};
use super::{INSPECTION_BYTES, InspectedDarwinNix, decode_inspection};
use crate::installation::TransactionError;
use crate::installation::dashboard::terminal::ParkedTerminal;
use crate::installation::filesystem::{
    PolicyHelperLocation, helper_snapshot, protected_helper_path,
};
use std::fs::{self, File, Metadata};
use std::io::{self, Read};
use std::os::fd::OwnedFd;
use std::os::unix::net::UnixStream;
use std::path::Path;
use std::process::{Child, Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};

const LAUNCH_BUDGET: Duration = Duration::from_secs(125);
const CLEANUP_BUDGET: Duration = Duration::from_secs(25);
const REAP_BUDGET: Duration = Duration::from_secs(2);
const CHECK_INTERVAL: Duration = Duration::from_millis(20);

enum Session {
    Running {
        _control: UnixStream,
        deadline: Instant,
    },
    Stopping {
        cause: TransactionError,
        deadline: Instant,
    },
}

impl Session {
    fn stop(&mut self, cause: TransactionError) {
        if matches!(self, Self::Running { .. }) {
            *self = Self::Stopping {
                cause,
                deadline: Instant::now() + CLEANUP_BUDGET,
            };
        }
    }

    fn result(
        self,
        exit: ExitStatus,
        frame: &[u8],
    ) -> Result<InspectedDarwinNix, TransactionError> {
        if !matches!(exit.code(), Some(0 | 1)) {
            return Err(TransactionError::CleanupUnknown);
        }
        if let Self::Stopping { cause, .. } = self {
            return Err(cause);
        }
        if exit.code() == Some(1) && frame.is_empty() {
            return Err(TransactionError::AdministratorUnavailable);
        }
        if !exit.success() || frame.len() != INSPECTION_BYTES {
            return Err(TransactionError::Verification("Darwin policy helper reply"));
        }
        decode_inspection(frame).map_err(TransactionError::NixBlocked)
    }
}

pub(in crate::installation) fn observe(
    path: &Path,
    executable: &File,
    opened: &Metadata,
    terminal: Option<&ParkedTerminal>,
    checkpoint: impl Fn() -> Result<(), TransactionError>,
) -> Result<InspectedDarwinNix, TransactionError> {
    check_foreground(terminal, &checkpoint)?;
    if unsafe { libc::getuid() } == 0
        || unsafe { libc::getuid() } != unsafe { libc::geteuid() }
        || path
            .file_name()
            .and_then(|name| name.to_str())
            .and_then(|name| name.split_once('-'))
            .map(|(_, name)| name)
            != Some("neo-policy-helper")
    {
        return Err(TransactionError::Verification(
            "Darwin policy launcher identity",
        ));
    }
    let (control, launcher_control) = UnixStream::pair().map_err(|_| TransactionError::Io)?;
    let (output, launcher_output) = UnixStream::pair().map_err(|_| TransactionError::Io)?;
    output
        .set_nonblocking(true)
        .map_err(|_| TransactionError::Io)?;
    let (argument, stderr) = match terminal {
        Some(_) => (ATTENDED_ARGUMENT, Stdio::inherit()),
        None => (LAUNCH_ARGUMENT, Stdio::null()),
    };
    recheck(path, executable, opened)?;
    check_foreground(terminal, &checkpoint)?;
    let mut launcher = Command::new(path)
        .arg(argument)
        .current_dir("/")
        .env_clear()
        .env("PATH", "/usr/bin:/bin")
        .stdin(Stdio::from(OwnedFd::from(launcher_control)))
        .stdout(Stdio::from(OwnedFd::from(launcher_output)))
        .stderr(stderr)
        .spawn()
        .map_err(|_| TransactionError::AdministratorUnavailable)?;
    let observed = await_launcher(&mut launcher, control, output, || {
        check_foreground(terminal, &checkpoint)
    })?;
    recheck(path, executable, opened)?;
    check_foreground(terminal, &checkpoint)?;
    Ok(observed)
}

fn check_foreground(
    terminal: Option<&ParkedTerminal>,
    checkpoint: &impl Fn() -> Result<(), TransactionError>,
) -> Result<(), TransactionError> {
    checkpoint()?;
    if terminal.is_some_and(|terminal| !terminal.foreground()) {
        return Err(TransactionError::AdministratorUnavailable);
    }
    Ok(())
}

fn recheck(path: &Path, executable: &File, opened: &Metadata) -> Result<(), TransactionError> {
    let held = executable.metadata().map_err(|_| TransactionError::Io)?;
    let named = fs::symlink_metadata(path).map_err(|_| TransactionError::Io)?;
    let before = helper_snapshot(opened);
    if !protected_helper_path(path, PolicyHelperLocation::Store)
        || helper_snapshot(&held) != before
        || helper_snapshot(&named) != before
    {
        return Err(TransactionError::Verification(
            "Darwin policy helper changed",
        ));
    }
    Ok(())
}

fn await_launcher(
    launcher: &mut Child,
    control: UnixStream,
    mut output: UnixStream,
    checkpoint: impl Fn() -> Result<(), TransactionError>,
) -> Result<InspectedDarwinNix, TransactionError> {
    let mut session = Session::Running {
        _control: control,
        deadline: Instant::now() + LAUNCH_BUDGET,
    };
    let mut frame = [0; INSPECTION_BYTES + 1];
    let mut received = 0;
    let mut output_closed = false;
    loop {
        if let Session::Running { deadline, .. } = session {
            if let Err(error) = checkpoint() {
                session.stop(error);
            } else if Instant::now() >= deadline {
                session.stop(TransactionError::AdministratorUnavailable);
            }
        }

        if !output_closed {
            match output.read(&mut frame[received..]) {
                Ok(0) => output_closed = true,
                Ok(count) => {
                    received += count;
                    if received > INSPECTION_BYTES {
                        session.stop(TransactionError::Verification("Darwin policy helper reply"));
                        let _ = output.shutdown(std::net::Shutdown::Read);
                        output_closed = true;
                    }
                }
                Err(ref error)
                    if matches!(
                        error.kind(),
                        io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                    ) => {}
                Err(_) => {
                    session.stop(TransactionError::Io);
                    let _ = output.shutdown(std::net::Shutdown::Read);
                    output_closed = true;
                }
            }
        }
        match launcher.try_wait() {
            Ok(Some(exit)) if output_closed => return session.result(exit, &frame[..received]),
            Ok(_) => {}
            Err(error) if error.kind() == io::ErrorKind::Interrupted => {}
            Err(_) => session.stop(TransactionError::CleanupUnknown),
        }
        if let Session::Stopping { deadline, .. } = session
            && Instant::now() >= deadline
        {
            reap_ordinary_launcher(launcher);
            return Err(TransactionError::CleanupUnknown);
        }
        std::thread::sleep(CHECK_INTERVAL);
    }
}

fn reap_ordinary_launcher(launcher: &mut Child) {
    if matches!(launcher.try_wait(), Ok(None)) {
        let _ = launcher.kill();
        let deadline = Instant::now() + REAP_BUDGET;
        while Instant::now() < deadline {
            if matches!(launcher.try_wait(), Ok(Some(_))) {
                return;
            }
            std::thread::sleep(CHECK_INTERVAL);
        }
    }
}
