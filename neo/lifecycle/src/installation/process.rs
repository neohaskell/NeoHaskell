use super::{StateLock, open_owned_file, validate_directory, validate_private_file};
use fs2::FileExt;
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::{self, Read, Write};
use std::net::Shutdown;
use std::os::fd::{AsFd, AsRawFd, OwnedFd};
use std::os::unix::fs::MetadataExt;
use std::os::unix::net::UnixStream;
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};

pub(crate) const WORKER_ARGUMENT: &str = "--internal-operation-worker";
const NIX: &str = "/nix/var/nix/profiles/default/bin/nix";
const REQUEST_LIMIT: usize = 8192;
const CAPTURE_LIMIT: usize = 64 * 1024;
const REPLY_LIMIT: usize = CAPTURE_LIMIT * 8 + 1024;
const HANDSHAKE_TIMEOUT: Duration = Duration::from_secs(5);
const OPERATION_TIMEOUT: Duration = Duration::from_secs(30 * 60);
const TERM_GRACE: Duration = Duration::from_millis(250);

#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct LeaseIdentity {
    path: PathBuf,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) enum NixOperation {
    BuildProfile {
        output: PathBuf,
    },
    #[cfg(test)]
    Fixture {
        name: String,
    },
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct OperationResult {
    pub(super) exit_code: Option<i32>,
    pub(super) stdout: Vec<u8>,
    pub(super) stderr: Vec<u8>,
    pub(super) failure: Option<String>,
}

pub(super) struct OperationOwner {
    control: UnixStream,
    worker: Child,
}

impl OperationOwner {
    pub(super) fn start(lock: &StateLock) -> io::Result<Self> {
        Self::launch(lock, worker_command()?)
    }

    fn launch(lock: &StateLock, mut command: Command) -> io::Result<Self> {
        let (control, worker_control) = UnixStream::pair()?;
        command
            .env_clear()
            .stdin(Stdio::from(lock.file.try_clone()?))
            .stdout(Stdio::from(OwnedFd::from(worker_control)))
            .stderr(Stdio::inherit());
        let worker = command.spawn()?;
        let mut owner = Self { control, worker };
        owner.control.set_read_timeout(Some(HANDSHAKE_TIMEOUT))?;
        owner.control.set_write_timeout(Some(HANDSHAKE_TIMEOUT))?;
        #[cfg(test)]
        {
            let mut harness = [0; 16];
            owner.control.read_exact(&mut harness)?;
            if &harness != b"\nrunning 1 test\n" {
                return Err(invalid("test worker envelope"));
            }
        }
        send(
            &mut owner.control,
            &LeaseIdentity {
                path: lock.path.clone(),
            },
        )?;
        let acknowledgement: String = receive(&mut owner.control, REQUEST_LIMIT)?;
        if acknowledgement != "lease-owned" {
            return Err(invalid("worker did not acknowledge lease"));
        }
        Ok(owner)
    }

    pub(super) fn execute(mut self, operation: NixOperation) -> io::Result<OperationResult> {
        send(&mut self.control, &operation)?;
        self.control
            .set_read_timeout(Some(OPERATION_TIMEOUT + HANDSHAKE_TIMEOUT))?;
        let result = receive(&mut self.control, REPLY_LIMIT)?;
        let _ = self.control.shutdown(Shutdown::Both);
        self.worker.wait()?;
        Ok(result)
    }
}

impl Drop for OperationOwner {
    fn drop(&mut self) {
        let _ = self.control.shutdown(Shutdown::Both);
        let _ = self.worker.wait();
    }
}

fn worker_command() -> io::Result<Command> {
    let mut command = Command::new(std::env::current_exe()?);
    #[cfg(not(test))]
    command.arg(WORKER_ARGUMENT);
    #[cfg(test)]
    command.args([
        "--exact",
        "installation::process::tests::worker",
        "--ignored",
        "--nocapture",
        "--quiet",
    ]);
    Ok(command)
}

pub(crate) fn run_worker() -> i32 {
    match worker() {
        Ok(()) => 0,
        Err(_) => 125,
    }
}

fn worker() -> io::Result<()> {
    let lease = File::from(io::stdin().as_fd().try_clone_to_owned()?);
    validate_private_file(&lease.metadata()?).map_err(|_| invalid("invalid inherited lease"))?;
    let mut control = UnixStream::from(io::stdout().as_fd().try_clone_to_owned()?);
    control.peer_addr()?;
    control.set_read_timeout(Some(HANDSHAKE_TIMEOUT))?;
    control.set_write_timeout(Some(HANDSHAKE_TIMEOUT))?;
    let identity: LeaseIdentity = receive(&mut control, REQUEST_LIMIT)?;
    let directory = identity
        .path
        .parent()
        .ok_or_else(|| invalid("lease directory"))?;
    if identity.path.file_name().and_then(|name| name.to_str()) != Some(super::LOCK_NAME) {
        return Err(invalid("lease name"));
    }
    validate_directory(directory, true).map_err(|_| invalid("lease directory"))?;
    let independent = open_owned_file(&identity.path, true, true)
        .map_err(|_| invalid("lease path"))?
        .ok_or_else(|| invalid("missing lease"))?;
    let inherited = lease.metadata()?;
    let observed = independent.metadata()?;
    if inherited.dev() != observed.dev() || inherited.ino() != observed.ino() {
        return Err(invalid("lease identity"));
    }
    match FileExt::try_lock_exclusive(&independent) {
        Err(error) if error.kind() == io::ErrorKind::WouldBlock => {}
        _ => return Err(invalid("lease not held")),
    }
    FileExt::try_lock_exclusive(&lease)?;
    drop(independent);
    #[cfg(target_os = "linux")]
    if unsafe { libc::prctl(libc::PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0) } != 0 {
        return Err(io::Error::last_os_error());
    }
    send(&mut control, &"lease-owned")?;
    control.set_read_timeout(Some(HANDSHAKE_TIMEOUT))?;
    let operation: NixOperation = receive(&mut control, REQUEST_LIMIT)?;
    let command = operation.command(directory)?;
    control.set_nonblocking(true)?;
    if disconnected(&control)? {
        return Err(invalid("controller disconnected before admission"));
    }
    let result = supervise(command, &lease, &control);
    control.set_nonblocking(false)?;
    send(&mut control, &result)
}

impl NixOperation {
    fn command(&self, directory: &Path) -> io::Result<Command> {
        let mut command = match self {
            Self::BuildProfile { output } => {
                validate_output(output)?;
                let profile = directory.join("prepared");
                if let Ok(metadata) = std::fs::symlink_metadata(&profile) {
                    if !metadata.file_type().is_symlink()
                        || metadata.uid() != unsafe { libc::geteuid() }
                    {
                        return Err(invalid("foreign profile"));
                    }
                    let target = std::fs::read_link(&profile)?;
                    let target = target.to_str().ok_or_else(|| invalid("profile target"))?;
                    let generation = target
                        .strip_prefix("prepared-")
                        .and_then(|s| s.strip_suffix("-link"));
                    if !generation
                        .is_some_and(|s| !s.is_empty() && s.bytes().all(|c| c.is_ascii_digit()))
                    {
                        return Err(invalid("foreign profile target"));
                    }
                }
                let mut command = Command::new(NIX);
                command
                    .args([
                        "--extra-experimental-features",
                        "nix-command flakes",
                        "build",
                        "--profile",
                    ])
                    .arg(profile)
                    .arg("--no-link")
                    .arg(output);
                command
            }
            #[cfg(test)]
            Self::Fixture { name } => tests::fixture_command(name)?,
        };
        command
            .env_clear()
            .env("PATH", "/usr/bin:/bin")
            .env("HOME", directory)
            .env("LANG", "C")
            .current_dir(directory)
            .process_group(0);
        Ok(command)
    }
}

fn validate_output(output: &Path) -> io::Result<()> {
    let value = output.to_str().ok_or_else(|| invalid("output encoding"))?;
    let name = value
        .strip_prefix("/nix/store/")
        .ok_or_else(|| invalid("output store"))?;
    let (hash, suffix) = name.split_once('-').ok_or_else(|| invalid("output name"))?;
    if hash.len() != 32
        || !hash
            .bytes()
            .all(|c| b"0123456789abcdfghijklmnpqrsvwxyz".contains(&c))
        || suffix.is_empty()
        || suffix.len() > 211
        || suffix.starts_with('.')
        || suffix.ends_with(".drv")
        || !suffix
            .bytes()
            .all(|c| c.is_ascii_alphanumeric() || b"+._?=-".contains(&c))
    {
        return Err(invalid("opaque output path"));
    }
    Ok(())
}

struct ClientGroup {
    child: Child,
    group: i32,
    status: Option<ExitStatus>,
    quiescent: bool,
}

impl ClientGroup {
    fn signal(&self, signal: i32) {
        unsafe {
            libc::kill(-self.group, signal);
        }
    }

    fn reap(&mut self) -> io::Result<bool> {
        if self.status.is_none() {
            self.status = self.child.try_wait()?;
        }
        if self.status.is_none() {
            return Ok(false);
        }
        #[cfg(target_os = "linux")]
        loop {
            let result = unsafe { libc::waitpid(-self.group, std::ptr::null_mut(), libc::WNOHANG) };
            if result <= 0 {
                break;
            }
        }
        let result = unsafe { libc::kill(-self.group, 0) };
        if result == -1 && io::Error::last_os_error().raw_os_error() == Some(libc::ESRCH) {
            self.quiescent = true;
        }
        Ok(self.quiescent)
    }

    fn cancel(&mut self) {
        if self.quiescent {
            return;
        }
        self.signal(libc::SIGTERM);
        let started = Instant::now();
        let mut delay = Duration::from_millis(1);
        loop {
            if self.reap().unwrap_or(false) {
                return;
            }
            if started.elapsed() >= TERM_GRACE {
                self.signal(libc::SIGKILL);
            }
            std::thread::sleep(if started.elapsed() < TERM_GRACE {
                delay.min(TERM_GRACE.saturating_sub(started.elapsed()))
            } else {
                delay
            });
            delay = delay.saturating_mul(2);
        }
    }
}

impl Drop for ClientGroup {
    fn drop(&mut self) {
        self.cancel();
    }
}

fn supervise(mut command: Command, lease: &File, control: &UnixStream) -> OperationResult {
    let mut result = OperationResult {
        exit_code: None,
        stdout: Vec::new(),
        stderr: Vec::new(),
        failure: None,
    };
    let running = (|| -> io::Result<()> {
        command
            .stdin(Stdio::from(lease.try_clone()?))
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        let child = command.spawn()?;
        let group = child.id() as i32;
        let mut client = ClientGroup {
            child,
            group,
            status: None,
            quiescent: false,
        };
        let mut stdout = client
            .child
            .stdout
            .take()
            .ok_or_else(|| invalid("missing stdout"))?;
        let mut stderr = client
            .child
            .stderr
            .take()
            .ok_or_else(|| invalid("missing stderr"))?;
        nonblocking(stdout.as_raw_fd())?;
        nonblocking(stderr.as_raw_fd())?;
        let started = Instant::now();
        let mut stdout_open = true;
        let mut stderr_open = true;
        loop {
            if stdout_open {
                stdout_open = drain(&mut stdout, &mut result.stdout)?;
            }
            if stderr_open {
                stderr_open = drain(&mut stderr, &mut result.stderr)?;
            }
            if disconnected(control)? {
                return Err(invalid("controller disconnected"));
            }
            if client.reap()? {
                drain(&mut stdout, &mut result.stdout)?;
                drain(&mut stderr, &mut result.stderr)?;
                result.exit_code = client.status.and_then(|status| status.code());
                return Ok(());
            }
            if client.status.is_some() {
                return Err(invalid("client leader exited with live descendants"));
            }
            if started.elapsed() >= OPERATION_TIMEOUT {
                return Err(invalid("operation deadline"));
            }
            let mut descriptors = [
                libc::pollfd {
                    fd: control.as_raw_fd(),
                    events: libc::POLLIN,
                    revents: 0,
                },
                libc::pollfd {
                    fd: if stdout_open { stdout.as_raw_fd() } else { -1 },
                    events: libc::POLLIN,
                    revents: 0,
                },
                libc::pollfd {
                    fd: if stderr_open { stderr.as_raw_fd() } else { -1 },
                    events: libc::POLLIN,
                    revents: 0,
                },
            ];
            if unsafe { libc::poll(descriptors.as_mut_ptr(), descriptors.len() as _, 50) } < 0
                && io::Error::last_os_error().kind() != io::ErrorKind::Interrupted
            {
                return Err(io::Error::last_os_error());
            }
        }
    })();
    if let Err(error) = running {
        result.failure = Some(error.to_string());
    }
    result
}

fn nonblocking(fd: i32) -> io::Result<()> {
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL) };
    if flags == -1 || unsafe { libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) } == -1 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

fn disconnected(control: &UnixStream) -> io::Result<bool> {
    let mut byte = [0u8];
    let count = unsafe {
        libc::recv(
            control.as_raw_fd(),
            byte.as_mut_ptr().cast(),
            1,
            libc::MSG_PEEK | libc::MSG_DONTWAIT,
        )
    };
    if count == 0 {
        return Ok(true);
    }
    if count > 0 {
        return Err(invalid("unexpected controller message"));
    }
    let error = io::Error::last_os_error();
    if error.kind() == io::ErrorKind::WouldBlock {
        Ok(false)
    } else {
        Err(error)
    }
}

fn drain(reader: &mut impl Read, target: &mut Vec<u8>) -> io::Result<bool> {
    let mut buffer = [0; 4096];
    loop {
        match reader.read(&mut buffer) {
            Ok(0) => return Ok(false),
            Ok(count) => {
                if target.len() + count > CAPTURE_LIMIT {
                    return Err(invalid("client output limit"));
                }
                target.extend_from_slice(&buffer[..count]);
            }
            Err(error) if error.kind() == io::ErrorKind::WouldBlock => return Ok(true),
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            Err(error) => return Err(error),
        }
    }
}

fn send(stream: &mut UnixStream, value: &impl Serialize) -> io::Result<()> {
    let data = serde_json::to_vec(value).map_err(|_| invalid("message encoding"))?;
    stream.write_all(&(data.len() as u32).to_be_bytes())?;
    stream.write_all(&data)
}

fn receive<T: serde::de::DeserializeOwned>(stream: &mut UnixStream, limit: usize) -> io::Result<T> {
    let mut length = [0; 4];
    let deadline = Instant::now() + stream.read_timeout()?.unwrap_or(HANDSHAKE_TIMEOUT);
    read_until(stream, &mut length, deadline)?;
    let length = u32::from_be_bytes(length) as usize;
    if length > limit {
        return Err(invalid("message limit"));
    }
    let mut data = vec![0; length];
    read_until(stream, &mut data, deadline)?;
    serde_json::from_slice(&data).map_err(|_| invalid("invalid worker message"))
}

fn read_until(stream: &mut UnixStream, mut target: &mut [u8], deadline: Instant) -> io::Result<()> {
    while !target.is_empty() {
        let remaining = deadline
            .checked_duration_since(Instant::now())
            .filter(|duration| !duration.is_zero())
            .ok_or_else(|| io::Error::new(io::ErrorKind::TimedOut, "message deadline"))?;
        stream.set_read_timeout(Some(remaining))?;
        match stream.read(target) {
            Ok(0) => {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "truncated message",
                ));
            }
            Ok(count) => target = &mut target[count..],
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            Err(error) => return Err(error),
        }
    }
    Ok(())
}

fn invalid(message: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{self, OpenOptions};
    use std::os::unix::fs::PermissionsExt;
    use tempfile::TempDir;

    pub(super) fn fixture_command(name: &str) -> io::Result<Command> {
        let test = match name {
            "success" => "installation::process::tests::fixture_success",
            "resistant" => "installation::process::tests::fixture_resistant",
            "leader" => "installation::process::tests::fixture_leader",
            "overflow" => "installation::process::tests::fixture_overflow",
            "launch-error" => return Ok(Command::new("/no-such-neo-process-fixture")),
            _ => return Err(invalid("fixture name")),
        };
        let mut command = Command::new(std::env::current_exe()?);
        command.args(["--exact", test, "--ignored", "--nocapture", "--quiet"]);
        Ok(command)
    }

    fn setup() -> (TempDir, StateLock) {
        let temporary = tempfile::tempdir().unwrap();
        let path = temporary.path().canonicalize().unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o700)).unwrap();
        let lock = StateLock::acquire(&path.join("lock")).unwrap();
        (temporary, lock)
    }

    fn busy(path: &Path) -> bool {
        matches!(
            StateLock::acquire(path),
            Err(super::super::TransactionError::Busy)
        )
    }

    fn await_condition(mut condition: impl FnMut() -> bool) {
        let deadline = Instant::now() + Duration::from_secs(10);
        let mut delay = Duration::from_millis(1);
        while !condition() {
            assert!(Instant::now() < deadline, "condition deadline");
            std::thread::sleep(delay);
            delay *= 2;
        }
    }

    fn admitted(lock: &StateLock, name: &str) -> OperationOwner {
        let mut owner = OperationOwner::start(lock).unwrap();
        send(
            &mut owner.control,
            &NixOperation::Fixture {
                name: name.to_owned(),
            },
        )
        .unwrap();
        owner
    }

    #[test]
    #[ignore]
    fn worker() {
        std::process::exit(run_worker());
    }

    #[test]
    #[ignore]
    fn fixture_success() {
        println!("fixed fixture output");
        eprintln!("fixed fixture error");
        std::process::exit(0);
    }

    #[test]
    #[ignore]
    fn fixture_resistant() {
        unsafe {
            libc::signal(libc::SIGTERM, libc::SIG_IGN);
        }
        fs::write("client-pid", std::process::id().to_string()).unwrap();
        let mut marker = OpenOptions::new()
            .create(true)
            .append(true)
            .open("marker")
            .unwrap();
        loop {
            marker.write_all(b"mutation\n").unwrap();
            marker.sync_all().unwrap();
            std::thread::sleep(Duration::from_millis(10));
        }
    }

    #[test]
    #[ignore]
    fn fixture_leader() {
        let mut command = fixture_command("resistant").unwrap();
        command
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit());
        command.spawn().unwrap();
        await_condition(|| Path::new("marker").exists());
        std::process::exit(0);
    }

    #[test]
    #[ignore]
    fn fixture_overflow() {
        let block = [b'x'; 4096];
        loop {
            io::stdout().write_all(&block).unwrap();
            io::stderr().write_all(&block).unwrap();
        }
    }

    #[test]
    fn reply_limit_accepts_two_complete_binary_captures() {
        let result = OperationResult {
            exit_code: Some(0),
            stdout: vec![255; CAPTURE_LIMIT],
            stderr: vec![255; CAPTURE_LIMIT],
            failure: None,
        };
        assert!(serde_json::to_vec(&result).unwrap().len() <= REPLY_LIMIT);
    }

    #[test]
    fn fixed_operation_rejects_nonopaque_output_and_foreign_profile() {
        for output in [
            "/tmp/output",
            "/nix/store/a-name",
            "/nix/store/00000000000000000000000000000000-source.drv",
            "/nix/store/00000000000000000000000000000000-name/bin/neo",
            "/nix/store/00000000000000000000000000000000-name^out",
        ] {
            assert!(validate_output(Path::new(output)).is_err());
        }
        let (_temporary, lock) = setup();
        let directory = lock.path.parent().unwrap();
        let output = PathBuf::from("/nix/store/00000000000000000000000000000000-output");
        let operation = NixOperation::BuildProfile { output };
        let command = operation.command(directory).unwrap();
        assert_eq!(command.get_program(), NIX);
        assert_eq!(
            command.get_args().collect::<Vec<_>>(),
            [
                "--extra-experimental-features",
                "nix-command flakes",
                "build",
                "--profile",
                directory.join("prepared").to_str().unwrap(),
                "--no-link",
                "/nix/store/00000000000000000000000000000000-output"
            ]
        );
        fs::write(directory.join("prepared"), b"foreign").unwrap();
        assert!(operation.command(directory).is_err());
    }

    #[test]
    fn acknowledgement_owns_same_description_before_admission() {
        let (_temporary, lock) = setup();
        let path = lock.path.clone();
        let owner = OperationOwner::start(&lock).unwrap();
        drop(lock);
        assert!(busy(&path));
        assert!(!path.parent().unwrap().join("marker").exists());
        drop(owner);
        assert!(!busy(&path));
    }

    #[test]
    fn worker_before_acknowledgement_retains_lease_and_admits_nothing() {
        let (_temporary, lock) = setup();
        let path = lock.path.clone();
        let (control, child_control) = UnixStream::pair().unwrap();
        let mut command = worker_command().unwrap();
        let mut child = command
            .stdin(Stdio::from(lock.file.try_clone().unwrap()))
            .stdout(Stdio::from(OwnedFd::from(child_control)))
            .stderr(Stdio::null())
            .spawn()
            .unwrap();
        drop(command);
        drop(lock);
        assert!(busy(&path));
        assert!(!path.parent().unwrap().join("marker").exists());
        drop(control);
        assert!(!child.wait().unwrap().success());
        assert!(!busy(&path));
    }

    #[test]
    fn successful_child_uses_independent_bounded_captures() {
        let (_temporary, lock) = setup();
        let result = OperationOwner::start(&lock)
            .unwrap()
            .execute(NixOperation::Fixture {
                name: "success".into(),
            })
            .unwrap();
        assert_eq!(result.exit_code, Some(0));
        assert!(result.failure.is_none(), "{:?}", result.failure);
        assert!(String::from_utf8_lossy(&result.stdout).contains("fixed fixture output"));
        assert!(String::from_utf8_lossy(&result.stderr).contains("fixed fixture error"));
    }

    #[test]
    fn normal_drop_escalates_resistant_client_before_release() {
        let (_temporary, lock) = setup();
        let path = lock.path.clone();
        let marker = path.parent().unwrap().join("marker");
        let owner = admitted(&lock, "resistant");
        await_condition(|| marker.exists());
        drop(lock);
        assert!(busy(&path));
        drop(owner);
        assert!(!busy(&path));
        let content = fs::read(&marker).unwrap();
        std::thread::sleep(Duration::from_millis(40));
        assert_eq!(fs::read(marker).unwrap(), content);
    }

    #[test]
    fn early_leader_exit_cancels_surviving_group_before_release() {
        let (_temporary, lock) = setup();
        let path = lock.path.clone();
        let owner = OperationOwner::start(&lock).unwrap();
        drop(lock);
        let result = owner
            .execute(NixOperation::Fixture {
                name: "leader".into(),
            })
            .unwrap();
        assert_eq!(
            result.failure.as_deref(),
            Some("client leader exited with live descendants")
        );
        assert!(!busy(&path));
        let marker = path.parent().unwrap().join("marker");
        let content = fs::read(&marker).unwrap();
        std::thread::sleep(Duration::from_millis(40));
        assert_eq!(fs::read(marker).unwrap(), content);
    }

    #[test]
    fn output_overflow_cancels_and_keeps_capture_bounded() {
        let (_temporary, lock) = setup();
        let result = OperationOwner::start(&lock)
            .unwrap()
            .execute(NixOperation::Fixture {
                name: "overflow".into(),
            })
            .unwrap();
        assert_eq!(result.failure.as_deref(), Some("client output limit"));
        assert!(result.stdout.len() <= CAPTURE_LIMIT);
        assert!(result.stderr.len() <= CAPTURE_LIMIT);
    }

    #[test]
    fn launch_failures_leave_no_unowned_work() {
        let (_temporary, lock) = setup();
        assert!(OperationOwner::launch(&lock, Command::new("/no-such-neo-worker")).is_err());
        assert!(busy(&lock.path));
        let result = OperationOwner::start(&lock)
            .unwrap()
            .execute(NixOperation::Fixture {
                name: "launch-error".into(),
            })
            .unwrap();
        assert!(result.failure.is_some());
        let path = lock.path.clone();
        drop(lock);
        assert!(!busy(&path));
    }

    #[test]
    fn truncated_request_closes_without_client_admission() {
        let (_temporary, lock) = setup();
        let path = lock.path.clone();
        let mut owner = OperationOwner::start(&lock).unwrap();
        owner.control.write_all(&100u32.to_be_bytes()).unwrap();
        owner.control.write_all(b"{").unwrap();
        drop(lock);
        drop(owner);
        assert!(!busy(&path));
        assert!(!path.parent().unwrap().join("marker").exists());
    }

    #[test]
    fn unexpected_message_cancels_live_client() {
        let (_temporary, lock) = setup();
        let path = lock.path.clone();
        let marker = path.parent().unwrap().join("marker");
        let mut owner = admitted(&lock, "resistant");
        await_condition(|| marker.exists());
        owner.control.write_all(b"unexpected").unwrap();
        let result: OperationResult = receive(&mut owner.control, REPLY_LIMIT).unwrap();
        assert_eq!(
            result.failure.as_deref(),
            Some("unexpected controller message")
        );
        drop(lock);
        drop(owner);
        assert!(!busy(&path));
    }

    #[test]
    fn helper_death_keeps_child_stdin_lease_until_child_quiescence() {
        let (_temporary, lock) = setup();
        let path = lock.path.clone();
        let marker = path.parent().unwrap().join("marker");
        let mut owner = admitted(&lock, "resistant");
        await_condition(|| marker.exists());
        let pid: i32 = fs::read_to_string(path.parent().unwrap().join("client-pid"))
            .unwrap()
            .parse()
            .unwrap();
        drop(lock);
        owner.worker.kill().unwrap();
        owner.worker.wait().unwrap();
        assert!(busy(&path));
        let length = fs::metadata(&marker).unwrap().len();
        await_condition(|| fs::metadata(&marker).unwrap().len() > length);
        assert!(busy(&path));
        unsafe {
            libc::kill(pid, libc::SIGKILL);
        }
        await_condition(|| !busy(&path));
        drop(owner);
    }

    #[test]
    #[ignore]
    fn fixture_controller() {
        let path = std::env::current_dir().unwrap().join("lock");
        let lock = StateLock::acquire(&path).unwrap();
        let _owner = admitted(&lock, "resistant");
        await_condition(|| Path::new("marker").exists());
        fs::write("controller-ready", b"ready").unwrap();
        std::thread::sleep(Duration::from_secs(60));
    }

    #[test]
    #[ignore]
    fn fixture_guard_panic() {
        let lease = File::from(io::stdin().as_fd().try_clone_to_owned().unwrap());
        let mut command = fixture_command("resistant").unwrap();
        let child = command
            .stdin(Stdio::from(lease))
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .process_group(0)
            .spawn()
            .unwrap();
        let group = child.id() as i32;
        let _client = ClientGroup {
            child,
            group,
            status: None,
            quiescent: false,
        };
        await_condition(|| Path::new("marker").exists());
        panic!("owned fixture panic");
    }

    #[test]
    fn panic_unwinds_through_group_cancellation_before_lease_close() {
        let (temporary, lock) = setup();
        let path = lock.path.clone();
        let mut child = Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "installation::process::tests::fixture_guard_panic",
                "--ignored",
                "--nocapture",
                "--quiet",
            ])
            .current_dir(temporary.path())
            .stdin(Stdio::from(lock.file.try_clone().unwrap()))
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .unwrap();
        drop(lock);
        assert!(!child.wait().unwrap().success());
        assert!(!busy(&path));
        let marker = temporary.path().join("marker");
        let content = fs::read(&marker).unwrap();
        std::thread::sleep(Duration::from_millis(40));
        assert_eq!(fs::read(marker).unwrap(), content);
    }

    #[test]
    fn controller_sigkill_triggers_worker_cancellation() {
        let (temporary, lock) = setup();
        let path = lock.path.clone();
        drop(lock);
        let mut controller = Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "installation::process::tests::fixture_controller",
                "--ignored",
                "--nocapture",
                "--quiet",
            ])
            .current_dir(temporary.path())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .unwrap();
        await_condition(|| temporary.path().join("controller-ready").exists());
        assert!(busy(&path));
        controller.kill().unwrap();
        controller.wait().unwrap();
        await_condition(|| !busy(&path));
        let marker = temporary.path().join("marker");
        let content = fs::read(&marker).unwrap();
        std::thread::sleep(Duration::from_millis(40));
        assert_eq!(fs::read(marker).unwrap(), content);
    }
}
