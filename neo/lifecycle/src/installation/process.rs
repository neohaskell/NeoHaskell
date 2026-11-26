use super::{RootLease, open_owned_file, validate_directory, validate_private_file};
use crate::candidate::OutputPath;
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

pub(super) mod shell_startup;

pub(crate) const WORKER_ARGUMENT: &str = "--internal-operation-worker";
#[cfg(test)]
const NIX: &str = "/nix/var/nix/profiles/default/bin/nix";
const REQUEST_LIMIT: usize = 32 * 1024;
const CAPTURE_LIMIT: usize = 64 * 1024;
const FAILURE_LIMIT: usize = 1024;
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
pub(super) enum WorkerRequest {
    Release(super::release::FetchRequest),
    ShellStartup(shell_startup::ShellStartupRequest),
    #[cfg(test)]
    BuildProfile {
        output: PathBuf,
    },
    #[cfg(test)]
    Version,
    LocalDaemonInfo,
    CacheSettings(bool),
    FlakeFeature,
    ApplyCacheSettings(super::cache_setup::CachePlan),
    AcquireRetained {
        pair: String,
        candidate: String,
        offline: bool,
    },
    AcquireProjectToolchain {
        output: String,
    },
    RegisterExact {
        pair: String,
        root: RootName,
    },
    PathInfo {
        cache: Option<usize>,
        paths: Vec<String>,
    },
    HashConvert {
        hashes: Vec<String>,
    },
    VerifyContents {
        pair: String,
    },
    ProbeTool {
        output: String,
        tool: Tool,
        probe: Probe,
    },
    #[cfg(test)]
    FixturePathInfo {
        paths: Vec<String>,
    },
    #[cfg(test)]
    Fixture {
        name: String,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub(super) enum RootName {
    Candidate(String),
    Generation(String),
    ProjectToolchain,
}

impl WorkerRequest {
    pub(super) fn mutates(&self) -> bool {
        match self {
            Self::Release(_)
            | Self::LocalDaemonInfo
            | Self::CacheSettings(_)
            | Self::FlakeFeature
            | Self::PathInfo { .. }
            | Self::HashConvert { .. }
            | Self::VerifyContents { .. } => false,
            Self::ShellStartup(_)
            | Self::ApplyCacheSettings(_)
            | Self::AcquireRetained { .. }
            | Self::AcquireProjectToolchain { .. }
            | Self::RegisterExact { .. }
            | Self::ProbeTool { .. } => true,
            #[cfg(test)]
            Self::FixturePathInfo { .. } | Self::Version => false,
            #[cfg(test)]
            Self::BuildProfile { .. } | Self::Fixture { .. } => true,
        }
    }
}

fn candidate_root(candidate: &str) -> io::Result<String> {
    if candidate.len() != 64
        || !candidate
            .bytes()
            .all(|b| b.is_ascii_hexdigit() && !b.is_ascii_uppercase())
    {
        return Err(invalid("candidate root identity"));
    }
    Ok(format!("candidate-{candidate}"))
}

#[derive(Debug, Serialize, Deserialize)]
pub(super) enum Tool {
    Neo,
    Neoctl,
}

#[derive(Debug, Serialize, Deserialize)]
pub(super) enum Probe {
    Version,
    Help,
    ShortHelp,
    UnknownFlag,
    Doctor,
    MixedHelp,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct OperationResult {
    pub(super) exit_code: Option<i32>,
    pub(super) stdout: Vec<u8>,
    pub(super) stderr: Vec<u8>,
    pub(super) failure: Option<String>,
}

const POLL_BYTES: usize = 4096;

pub(super) enum ProcessPoll {
    Pending,
    Ready,
    Quiescent(io::Result<OperationResult>),
    ContainmentLost(io::Error),
}

struct Worker {
    child: Child,
    control: UnixStream,
    _lease: File,
}

impl Drop for Worker {
    fn drop(&mut self) {
        let _ = self.control.shutdown(Shutdown::Both);
        let _ = self.child.wait();
    }
}

struct PartialRead {
    bytes: Vec<u8>,
    length: usize,
}

impl PartialRead {
    fn new(length: usize) -> Self {
        Self {
            bytes: Vec::with_capacity(length),
            length,
        }
    }

    fn poll(&mut self, control: &mut UnixStream) -> io::Result<bool> {
        if self.bytes.len() == self.length {
            return Ok(true);
        }
        let mut buffer = [0; POLL_BYTES];
        let count = buffer.len().min(self.length - self.bytes.len());
        match control.read(&mut buffer[..count]) {
            Ok(0) => Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "truncated message",
            )),
            Ok(count) => {
                self.bytes.extend_from_slice(&buffer[..count]);
                Ok(self.bytes.len() == self.length)
            }
            Err(error)
                if matches!(
                    error.kind(),
                    io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                ) =>
            {
                Ok(false)
            }
            Err(error) => Err(error),
        }
    }

    fn frame_length(&self, limit: usize) -> io::Result<usize> {
        let bytes: [u8; 4] = self
            .bytes
            .as_slice()
            .try_into()
            .map_err(|_| invalid("frame length"))?;
        let length = u32::from_be_bytes(bytes) as usize;
        if length > limit {
            return Err(invalid("message limit"));
        }
        Ok(length)
    }
}

struct PartialWrite {
    bytes: Vec<u8>,
    written: usize,
}

impl PartialWrite {
    fn message(value: &impl Serialize) -> io::Result<Self> {
        let data = serde_json::to_vec(value).map_err(|_| invalid("message encoding"))?;
        if data.len() > REQUEST_LIMIT {
            return Err(invalid("message limit"));
        }
        let mut bytes = Vec::with_capacity(4 + data.len());
        bytes.extend_from_slice(&(data.len() as u32).to_be_bytes());
        bytes.extend_from_slice(&data);
        Ok(Self { bytes, written: 0 })
    }

    fn poll(&mut self, control: &mut UnixStream) -> io::Result<bool> {
        let end = self.bytes.len().min(self.written + POLL_BYTES);
        match control.write(&self.bytes[self.written..end]) {
            Ok(0) => Err(io::Error::new(
                io::ErrorKind::WriteZero,
                "worker request closed",
            )),
            Ok(count) => {
                self.written += count;
                Ok(self.written == self.bytes.len())
            }
            Err(error)
                if matches!(
                    error.kind(),
                    io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                ) =>
            {
                Ok(false)
            }
            Err(error) => Err(error),
        }
    }
}

enum ProcessState {
    Launching,
    #[cfg(test)]
    Envelope(PartialRead),
    Identity(PartialWrite),
    AckLength(PartialRead),
    Ack(PartialRead),
    Ready,
    Request(PartialWrite),
    ReplyLength(PartialRead),
    ReplyHeader(PartialRead),
    Stdout(ReplyHeader, PartialRead),
    Stderr(ReplyHeader, Vec<u8>, PartialRead),
    End(OperationResult),
    Reap {
        result: io::Result<OperationResult>,
        reply_closed: bool,
    },
    Finished,
}

pub(super) struct OperationOwner {
    control: UnixStream,
    launch: Option<std::thread::JoinHandle<io::Result<Worker>>>,
    worker: Option<Worker>,
    state: ProcessState,
    identity: Option<PartialWrite>,
    deadline: Instant,
    stdout_limit: usize,
    cancellation: Option<io::Error>,
}

impl OperationOwner {
    pub(super) fn begin(lock: &RootLease) -> io::Result<Self> {
        Self::launch(lock, None)
    }

    fn launch(lock: &RootLease, command: Option<Command>) -> io::Result<Self> {
        let identity = PartialWrite::message(&LeaseIdentity {
            path: lock.path.clone(),
        })?;
        let (control, worker_control) = UnixStream::pair()?;
        control.set_nonblocking(true)?;
        let launch_control = control.try_clone()?;
        let lease = lock.file.try_clone()?;
        let deadline = Instant::now() + HANDSHAKE_TIMEOUT;
        let launch = std::thread::Builder::new()
            .name("neo-operation-launch".into())
            .spawn(move || {
                let mut command = match command {
                    Some(command) => command,
                    None => worker_command()?,
                };
                command
                    .env_clear()
                    .stdin(Stdio::from(lease.try_clone()?))
                    .stdout(Stdio::from(OwnedFd::from(worker_control)))
                    .stderr(Stdio::inherit());
                let child = command.spawn()?;
                Ok(Worker {
                    child,
                    control: launch_control,
                    _lease: lease,
                })
            })?;
        Ok(Self {
            control,
            launch: Some(launch),
            worker: None,
            state: ProcessState::Launching,
            identity: Some(identity),
            deadline,
            stdout_limit: CAPTURE_LIMIT,
            cancellation: None,
        })
    }

    #[cfg(test)]
    pub(super) fn start(lock: &RootLease) -> io::Result<Self> {
        let mut owner = Self::begin(lock)?;
        owner.wait_ready()?;
        Ok(owner)
    }

    #[cfg(test)]
    fn wait_ready(&mut self) -> io::Result<()> {
        loop {
            match self.poll() {
                ProcessPoll::Ready => return Ok(()),
                ProcessPoll::Pending => self.wait_for_interest(),
                ProcessPoll::Quiescent(result) => return result.map(|_| ()),
                ProcessPoll::ContainmentLost(error) => return Err(error),
            }
        }
    }

    pub(super) fn submit(&mut self, operation: WorkerRequest) -> io::Result<()> {
        if !matches!(self.state, ProcessState::Ready) || self.cancellation.is_some() {
            return Err(invalid("worker not ready"));
        }
        self.stdout_limit = operation.stdout_limit()?;
        let request = PartialWrite::message(&operation)?;
        self.deadline = Instant::now() + operation.timeout() + HANDSHAKE_TIMEOUT;
        self.state = ProcessState::Request(request);
        Ok(())
    }

    pub(super) fn request_cancel(&mut self) {
        self.stop(io::Error::new(
            io::ErrorKind::Interrupted,
            "operation cancellation requested",
        ));
    }

    fn stop(&mut self, error: io::Error) {
        if self.cancellation.is_none() {
            self.cancellation = Some(error);
            let _ = self.control.shutdown(Shutdown::Write);
        }
    }

    fn begin_reap(&mut self, result: io::Result<OperationResult>) {
        let _ = self.control.shutdown(Shutdown::Write);
        self.state = ProcessState::Reap {
            result,
            reply_closed: false,
        };
    }

    pub(super) fn poll(&mut self) -> ProcessPoll {
        if matches!(self.state, ProcessState::Finished) {
            return ProcessPoll::Pending;
        }
        if Instant::now() >= self.deadline && !matches!(self.state, ProcessState::Reap { .. }) {
            self.stop(io::Error::new(io::ErrorKind::TimedOut, "message deadline"));
        }
        if let Some(launch) = self.launch.as_ref() {
            if !launch.is_finished() {
                return ProcessPoll::Pending;
            }
            let launch = self.launch.take().unwrap();
            match launch.join() {
                Ok(Ok(worker)) => self.worker = Some(worker),
                Ok(Err(error)) => {
                    self.state = ProcessState::Finished;
                    return ProcessPoll::Quiescent(Err(error));
                }
                Err(_) => {
                    self.state = ProcessState::Finished;
                    return ProcessPoll::ContainmentLost(invalid("worker launch panicked"));
                }
            }
            #[cfg(test)]
            {
                self.state = ProcessState::Envelope(PartialRead::new(16));
            }
            #[cfg(not(test))]
            {
                self.state = ProcessState::Identity(self.identity.take().unwrap());
            }
        }
        if let Some(error) = self.cancellation.take() {
            if !matches!(self.state, ProcessState::Reap { .. }) {
                self.begin_reap(Err(error));
            }
        }
        let state = std::mem::replace(&mut self.state, ProcessState::Finished);
        if let ProcessState::Reap {
            result,
            mut reply_closed,
        } = state
        {
            if !reply_closed {
                let mut discarded = [0; POLL_BYTES];
                for _ in 0..16 {
                    match self.control.read(&mut discarded) {
                        Ok(0) => {
                            reply_closed = true;
                            break;
                        }
                        Ok(_) => {}
                        Err(error) if error.kind() == io::ErrorKind::WouldBlock => break,
                        Err(error) if error.kind() == io::ErrorKind::Interrupted => break,
                        Err(_) => {
                            reply_closed = true;
                            break;
                        }
                    }
                }
            }
            let worker = self.worker.as_mut().unwrap();
            match worker.child.try_wait() {
                Ok(Some(status)) => {
                    self.worker.take();
                    return if status.code() == Some(0)
                        || (result.is_err() && status.code() == Some(125))
                    {
                        ProcessPoll::Quiescent(result)
                    } else {
                        ProcessPoll::ContainmentLost(invalid(
                            "worker exited without confirmed cleanup",
                        ))
                    };
                }
                Ok(None) => {
                    self.state = ProcessState::Reap {
                        result,
                        reply_closed,
                    };
                }
                Err(error) => {
                    self.state = ProcessState::Reap {
                        result: Err(error),
                        reply_closed,
                    };
                }
            }
            return ProcessPoll::Pending;
        }
        match self.advance(state) {
            Ok(state) => self.state = state,
            Err(error) => {
                self.begin_reap(Err(error));
            }
        }
        if matches!(self.state, ProcessState::Ready) {
            ProcessPoll::Ready
        } else {
            ProcessPoll::Pending
        }
    }

    fn advance(&mut self, state: ProcessState) -> io::Result<ProcessState> {
        Ok(match state {
            #[cfg(test)]
            ProcessState::Envelope(mut input) => {
                if !input.poll(&mut self.control)? {
                    ProcessState::Envelope(input)
                } else if input.bytes != b"\nrunning 1 test\n" {
                    return Err(invalid("test worker envelope"));
                } else {
                    ProcessState::Identity(self.identity.take().unwrap())
                }
            }
            ProcessState::Identity(mut output) => {
                if output.poll(&mut self.control)? {
                    ProcessState::AckLength(PartialRead::new(4))
                } else {
                    ProcessState::Identity(output)
                }
            }
            ProcessState::AckLength(mut input) => {
                if input.poll(&mut self.control)? {
                    ProcessState::Ack(PartialRead::new(input.frame_length(REQUEST_LIMIT)?))
                } else {
                    ProcessState::AckLength(input)
                }
            }
            ProcessState::Ack(mut input) => {
                if !input.poll(&mut self.control)? {
                    ProcessState::Ack(input)
                } else {
                    let ack: String = serde_json::from_slice(&input.bytes)
                        .map_err(|_| invalid("invalid worker message"))?;
                    if ack != "lease-owned" {
                        return Err(invalid("worker did not acknowledge lease"));
                    }
                    ProcessState::Ready
                }
            }
            ProcessState::Ready => ProcessState::Ready,
            ProcessState::Request(mut output) => {
                if output.poll(&mut self.control)? {
                    ProcessState::ReplyLength(PartialRead::new(4))
                } else {
                    ProcessState::Request(output)
                }
            }
            ProcessState::ReplyLength(mut input) => {
                if input.poll(&mut self.control)? {
                    ProcessState::ReplyHeader(PartialRead::new(input.frame_length(FAILURE_LIMIT)?))
                } else {
                    ProcessState::ReplyLength(input)
                }
            }
            ProcessState::ReplyHeader(mut input) => {
                if !input.poll(&mut self.control)? {
                    ProcessState::ReplyHeader(input)
                } else {
                    let header: ReplyHeader = serde_json::from_slice(&input.bytes)
                        .map_err(|_| invalid("invalid worker message"))?;
                    if header.stdout > self.stdout_limit || header.stderr > CAPTURE_LIMIT {
                        return Err(invalid("capture frame limit"));
                    }
                    let body = PartialRead::new(header.stdout);
                    ProcessState::Stdout(header, body)
                }
            }
            ProcessState::Stdout(header, mut input) => {
                if input.poll(&mut self.control)? {
                    let stderr = PartialRead::new(header.stderr);
                    ProcessState::Stderr(header, input.bytes, stderr)
                } else {
                    ProcessState::Stdout(header, input)
                }
            }
            ProcessState::Stderr(header, stdout, mut input) => {
                if input.poll(&mut self.control)? {
                    ProcessState::End(OperationResult {
                        exit_code: header.exit_code,
                        stdout,
                        stderr: input.bytes,
                        failure: header.failure,
                    })
                } else {
                    ProcessState::Stderr(header, stdout, input)
                }
            }
            ProcessState::End(result) => {
                let mut excess = [0];
                match self.control.read(&mut excess) {
                    Ok(0) => ProcessState::Reap {
                        result: Ok(result),
                        reply_closed: true,
                    },
                    Ok(_) => return Err(invalid("unexpected worker bytes")),
                    Err(error)
                        if matches!(
                            error.kind(),
                            io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                        ) =>
                    {
                        ProcessState::End(result)
                    }
                    Err(error) => return Err(error),
                }
            }
            ProcessState::Launching | ProcessState::Reap { .. } | ProcessState::Finished => {
                return Err(invalid("invalid process transition"));
            }
        })
    }

    pub(super) fn interest(&self) -> super::coordinator::WaitInterest {
        let descriptor = match self.state {
            ProcessState::Launching
            | ProcessState::Reap {
                reply_closed: true, ..
            } => None,
            _ => Some((
                self.control.as_raw_fd(),
                if matches!(
                    self.state,
                    ProcessState::Identity(_) | ProcessState::Request(_)
                ) {
                    libc::POLLOUT
                } else {
                    libc::POLLIN
                },
            )),
        };
        super::coordinator::WaitInterest {
            descriptor,
            deadline: Instant::now()
                + Duration::from_millis(if descriptor.is_some() { 10 } else { 1 }),
        }
    }

    pub(super) fn wait_for_interest(&self) {
        self.interest().wait();
    }

    #[cfg(test)]
    pub(super) fn execute(mut self, operation: WorkerRequest) -> io::Result<OperationResult> {
        self.wait_ready()?;
        self.submit(operation)?;
        loop {
            match self.poll() {
                ProcessPoll::Pending | ProcessPoll::Ready => self.wait_for_interest(),
                ProcessPoll::Quiescent(result) => return result,
                ProcessPoll::ContainmentLost(error) => return Err(error),
            }
        }
    }
}

impl Drop for OperationOwner {
    fn drop(&mut self) {
        self.request_cancel();
        while self.launch.is_some() || self.worker.is_some() {
            match self.poll() {
                ProcessPoll::Pending | ProcessPoll::Ready => self.wait_for_interest(),
                ProcessPoll::Quiescent(_) | ProcessPoll::ContainmentLost(_) => break,
            }
        }
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

pub(super) fn validate_inherited_lease(lease: &File, path: &Path) -> io::Result<()> {
    validate_private_file(&lease.metadata()?).map_err(|_| invalid("invalid inherited lease"))?;
    let directory = path.parent().ok_or_else(|| invalid("lease directory"))?;
    if path.file_name().and_then(|name| name.to_str()) != Some(super::LOCK_NAME) {
        return Err(invalid("lease name"));
    }
    validate_directory(directory, true).map_err(|_| invalid("lease directory"))?;
    let independent = open_owned_file(&path, true, true)
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
    FileExt::try_lock_exclusive(lease)?;
    drop(independent);
    Ok(())
}

fn worker() -> io::Result<()> {
    let lease = File::from(io::stdin().as_fd().try_clone_to_owned()?);
    validate_private_file(&lease.metadata()?).map_err(|_| invalid("invalid inherited lease"))?;
    let mut control = UnixStream::from(io::stdout().as_fd().try_clone_to_owned()?);
    control.peer_addr()?;
    control.set_read_timeout(Some(HANDSHAKE_TIMEOUT))?;
    control.set_write_timeout(Some(HANDSHAKE_TIMEOUT))?;
    let identity: LeaseIdentity = receive(&mut control, REQUEST_LIMIT)?;
    validate_inherited_lease(&lease, &identity.path)?;
    let directory = identity
        .path
        .parent()
        .ok_or_else(|| invalid("lease directory"))?;
    #[cfg(target_os = "linux")]
    if unsafe { libc::prctl(libc::PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0) } != 0 {
        return Err(io::Error::last_os_error());
    }
    send(&mut control, &"lease-owned")?;
    control.set_read_timeout(Some(HANDSHAKE_TIMEOUT))?;
    let operation: WorkerRequest = receive(&mut control, REQUEST_LIMIT)?;
    let stdout_limit = operation.stdout_limit()?;
    let command = operation.command(directory)?;
    #[cfg(test)]
    if directory.join("fixture-cache").exists()
        && !matches!(operation, WorkerRequest::ShellStartup(_))
    {
        use std::os::unix::fs::OpenOptionsExt;
        let mut trace = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .mode(0o600)
            .open(directory.join("fixture-operations.jsonl"))?;
        serde_json::to_writer(&mut trace, &operation)
            .map_err(|_| invalid("fixture trace encoding"))?;
        trace.write_all(b"\n")?;
    }
    control.set_nonblocking(true)?;
    if disconnected(&control)? {
        return Err(invalid("controller disconnected before admission"));
    }
    #[cfg(test)]
    let stdout_limit = stdout_limit
        + if matches!(operation, WorkerRequest::Release(_)) {
            b"\nrunning 1 test\n".len()
        } else {
            0
        };
    let result = match &operation {
        WorkerRequest::ShellStartup(request) => {
            shell_startup::supervise(request, command, &control, operation.timeout())
        }
        _ => supervise(command, &lease, &control, stdout_limit, operation.timeout()),
    };
    #[cfg(test)]
    let result = if matches!(operation, WorkerRequest::Release(_)) {
        let mut result = result;
        if let Some(bytes) = result.stdout.strip_prefix(b"\nrunning 1 test\n") {
            result.stdout = bytes.to_vec();
        } else if result.failure.is_none() {
            return Err(invalid("release test envelope"));
        }
        result
    } else {
        result
    };
    control.set_nonblocking(false)?;
    send_result(&mut control, &result)
}

impl WorkerRequest {
    fn timeout(&self) -> Duration {
        match self {
            Self::ShellStartup(_) | Self::LocalDaemonInfo => Duration::from_secs(15),
            Self::Release(_) => Duration::from_secs(125),
            _ => OPERATION_TIMEOUT,
        }
    }

    pub(super) fn fits(&self, directory: &Path) -> bool {
        self.command(directory).is_ok()
            && serde_json::to_vec(self).is_ok_and(|bytes| bytes.len() <= REQUEST_LIMIT)
    }

    fn stdout_limit(&self) -> io::Result<usize> {
        match self {
            Self::CacheSettings(_) => Ok(1024 * 1024),
            Self::LocalDaemonInfo => Ok(4096),
            Self::Release(fetch) => fetch.limit().map_err(|_| invalid("release request")),
            Self::PathInfo { paths, .. } => {
                if paths.is_empty() || paths.len() > 64 {
                    return Err(invalid("path batch cardinality"));
                }
                Ok(16 * 1024 * 1024)
            }
            #[cfg(test)]
            Self::FixturePathInfo { paths } => {
                if paths.is_empty() || paths.len() > 64 {
                    return Err(invalid("fixture path batch"));
                }
                Ok(16 * 1024 * 1024)
            }
            Self::HashConvert { hashes } => {
                if hashes.is_empty() || hashes.len() > 64 {
                    return Err(invalid("hash batch cardinality"));
                }
                Ok(hashes.len() * 53)
            }
            _ => Ok(CAPTURE_LIMIT),
        }
    }

    fn command(&self, directory: &Path) -> io::Result<Command> {
        if let Self::ShellStartup(request) = self {
            return request.command();
        }
        let mut command = match self {
            Self::ShellStartup(_) => unreachable!(),
            Self::Release(fetch) => {
                super::release::client_command(fetch, &directory.join(super::LOCK_NAME))?
            }
            #[cfg(test)]
            Self::BuildProfile { output } => {
                let output = output.to_str().ok_or_else(|| invalid("output encoding"))?;
                let output =
                    OutputPath::parse(output).map_err(|_| invalid("opaque output path"))?;
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
                    .arg(output.as_str());
                command
            }
            #[cfg(test)]
            Self::Version => {
                let mut command = nix_command()?;
                command.arg("--version");
                command
            }
            Self::ApplyCacheSettings(plan) => plan.command()?,
            Self::FlakeFeature => {
                let mut command = Command::new(super::nix::system_client_path()?);
                command.args([
                    "--extra-experimental-features",
                    "nix-command",
                    "eval",
                    "--offline",
                    "--expr",
                    r#"(builtins.parseFlakeRef "github:NixOS/nix").type == "github""#,
                ]);
                command
            }
            Self::CacheSettings(enable_command) => {
                let mut command = Command::new(super::nix::system_client_path()?);
                if *enable_command {
                    command.args(["--extra-experimental-features", "nix-command"]);
                }
                command.args(["config", "show", "--json"]);
                command
            }
            Self::LocalDaemonInfo => {
                let mut command = nix_daemon_command()?;
                command.args(["--offline", "store", "info", "--json"]);
                command
            }
            Self::AcquireRetained {
                pair,
                candidate,
                offline,
            } => {
                OutputPath::parse(pair).map_err(|_| invalid("opaque output path"))?;
                let directory = directory.join("resources");
                validate_directory(&directory, true).map_err(|_| invalid("resource directory"))?;
                let mut command = exact_build(*offline)?;
                command
                    .arg("--profile")
                    .arg(directory.join("prepared"))
                    .arg("--out-link")
                    .arg(directory.join(candidate_root(candidate)?))
                    .arg(pair);
                command
            }
            Self::AcquireProjectToolchain { output } => {
                let output = OutputPath::parse(output).map_err(|_| invalid("toolchain output"))?;
                let names = super::toolchain::RootNames::new(&output)
                    .ok_or_else(|| invalid("toolchain output name"))?;
                let directory = directory.join("resources");
                validate_directory(&directory, true).map_err(|_| invalid("resource directory"))?;
                let mut command = exact_build(false)?;
                command
                    .arg("--out-link")
                    .arg(directory.join(names.staged()))
                    .arg(output.as_str());
                command
            }
            Self::RegisterExact { pair, root } => {
                let output = OutputPath::parse(pair).map_err(|_| invalid("opaque output path"))?;
                let directory = directory.join("resources");
                validate_directory(&directory, true).map_err(|_| invalid("resource directory"))?;
                let name = match root {
                    RootName::Candidate(id) => candidate_root(id)?,
                    RootName::ProjectToolchain => super::toolchain::RootNames::new(&output)
                        .ok_or_else(|| invalid("toolchain output name"))?
                        .registered(),
                    RootName::Generation(name) => {
                        if !super::resources::is_generation(name) {
                            return Err(invalid("generation root name"));
                        }
                        name.clone()
                    }
                };
                let mut command = Command::new(super::nix::legacy_store_path()?);
                command
                    .args(["--store", super::nix::system_daemon()?])
                    .args([
                        "--option",
                        "substitute",
                        "false",
                        "--option",
                        "builders",
                        "",
                        "--max-jobs",
                        "0",
                        "--realise",
                    ])
                    .arg(pair)
                    .arg("--add-root")
                    .arg(directory.join(name))
                    .arg("--indirect");
                command
            }
            Self::PathInfo { cache, paths } => {
                self.stdout_limit()?;
                let mut command = if cache.is_some() {
                    nix_command()?
                } else {
                    nix_daemon_command()?
                };
                append_path_info_args(&mut command, *cache, paths)?;
                command
            }
            Self::HashConvert { hashes } => {
                use base64::Engine;
                self.stdout_limit()?;
                let mut command = nix_command()?;
                command.args(["hash", "convert", "--hash-algo", "sha256", "--to", "nix32"]);
                for hash in hashes {
                    let encoded = hash
                        .strip_prefix("sha256-")
                        .ok_or_else(|| invalid("hash encoding"))?;
                    let decoded = base64::engine::general_purpose::STANDARD
                        .decode(encoded)
                        .map_err(|_| invalid("hash encoding"))?;
                    if decoded.len() != 32
                        || base64::engine::general_purpose::STANDARD.encode(&decoded) != encoded
                    {
                        return Err(invalid("hash encoding"));
                    }
                    command.arg(hash);
                }
                command
            }
            Self::VerifyContents { pair } => {
                OutputPath::parse(pair).map_err(|_| invalid("opaque output path"))?;
                let mut command = nix_daemon_command()?;
                command
                    .args(["store", "verify", "--recursive", "--sigs-needed", "1"])
                    .arg(pair);
                command
            }
            Self::ProbeTool {
                output,
                tool,
                probe,
            } => {
                let output =
                    OutputPath::parse(output).map_err(|_| invalid("opaque output path"))?;
                let tool = match tool {
                    Tool::Neo => "neo",
                    Tool::Neoctl => "neoctl",
                };
                let mut command = Command::new(Path::new(output.as_str()).join("bin").join(tool));
                command.args(match probe {
                    Probe::Version => &["--version"][..],
                    Probe::Help => &["--help"][..],
                    Probe::ShortHelp => &["-h"][..],
                    Probe::UnknownFlag => &["--bogus"][..],
                    Probe::Doctor => &["doctor"][..],
                    Probe::MixedHelp => &["--help", "unexpected"][..],
                });
                command
            }
            #[cfg(test)]
            Self::FixturePathInfo { paths } => {
                self.stdout_limit()?;
                let mut command = nix_command()?;
                command
                    .args([
                        "path-info",
                        "--json",
                        "--json-format",
                        "1",
                        "--sigs",
                        "--store",
                    ])
                    .arg(format!(
                        "file://{}",
                        directory.join("fixture-cache").display()
                    ));
                for path in paths {
                    OutputPath::parse(path).map_err(|_| invalid("fixture output"))?;
                    command.arg(path);
                }
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
            .env("NIX_REMOTE", super::nix::DAEMON)
            .env("NIX_USER_CONF_FILES", "/dev/null")
            .current_dir(directory)
            .process_group(0);
        validate_argv_budget(&command)?;
        Ok(command)
    }
}

fn append_path_info_args(
    command: &mut Command,
    cache: Option<usize>,
    paths: &[String],
) -> io::Result<()> {
    if let Some(index) = cache {
        let cache =
            crate::candidate::declared_cache(index).map_err(|_| invalid("cache identity"))?;
        command.arg("--store").arg(cache.url());
    }
    command.args(["path-info", "--json", "--json-format", "1", "--sigs"]);
    for path in paths {
        OutputPath::parse(path).map_err(|_| invalid("opaque output path"))?;
        command.arg(path);
    }
    Ok(())
}

fn nix_command() -> io::Result<Command> {
    let mut command = Command::new(super::nix::system_client_path()?);
    command.args(["--extra-experimental-features", "nix-command flakes"]);
    Ok(command)
}

fn nix_daemon_command() -> io::Result<Command> {
    let mut command = nix_command()?;
    command.args(["--store", super::nix::system_daemon()?]);
    Ok(command)
}

fn exact_build(offline: bool) -> io::Result<Command> {
    let mut command = nix_daemon_command()?;
    if offline {
        command.arg("--offline");
    } else {
        let urls =
            crate::candidate::nix_cache_urls().map_err(|_| invalid("declared substituters"))?;
        command
            .args(["--option", "substitute", "true", "--option", "substituters"])
            .arg(urls);
    }
    command.args(["--option", "builders", "", "--max-jobs", "0", "build"]);
    Ok(command)
}

fn validate_argv_budget(command: &Command) -> io::Result<()> {
    use std::os::unix::ffi::OsStrExt;
    let mut bytes = command.get_program().as_bytes().len() + 1;
    let mut pointers = 3usize;
    for arg in command.get_args() {
        bytes = bytes
            .checked_add(arg.as_bytes().len() + 1)
            .ok_or_else(|| invalid("argv limit"))?;
        pointers += 1;
    }
    for (name, value) in command.get_envs() {
        if let Some(value) = value {
            bytes = bytes
                .checked_add(name.as_bytes().len() + value.as_bytes().len() + 2)
                .ok_or_else(|| invalid("argv limit"))?;
            pointers += 1;
        }
    }
    let total = bytes
        .checked_add(pointers * std::mem::size_of::<usize>())
        .ok_or_else(|| invalid("argv limit"))?;
    let platform = unsafe { libc::sysconf(libc::_SC_ARG_MAX) };
    if platform <= 0 || total > 32 * 1024 || total > platform as usize {
        return Err(invalid("argv limit"));
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

fn supervise(
    mut command: Command,
    lease: &File,
    control: &UnixStream,
    stdout_limit: usize,
    timeout: Duration,
) -> OperationResult {
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
                stdout_open = drain(
                    &mut stdout,
                    &mut result.stdout,
                    stdout_limit,
                    "semantic stdout limit",
                )?;
            }
            if stderr_open {
                stderr_open = drain(
                    &mut stderr,
                    &mut result.stderr,
                    CAPTURE_LIMIT,
                    "diagnostic stderr limit",
                )?;
            }
            if disconnected(control)? {
                return Err(invalid("controller disconnected"));
            }
            if client.reap()? {
                drain(
                    &mut stdout,
                    &mut result.stdout,
                    stdout_limit,
                    "semantic stdout limit",
                )?;
                drain(
                    &mut stderr,
                    &mut result.stderr,
                    CAPTURE_LIMIT,
                    "diagnostic stderr limit",
                )?;
                result.exit_code = client.status.and_then(|status| status.code());
                return Ok(());
            }
            if client.status.is_some() {
                return Err(invalid("client leader exited with live descendants"));
            }
            if started.elapsed() >= timeout {
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

fn drain(
    reader: &mut impl Read,
    target: &mut Vec<u8>,
    limit: usize,
    message: &str,
) -> io::Result<bool> {
    let mut buffer = [0; 4096];
    loop {
        match reader.read(&mut buffer) {
            Ok(0) => return Ok(false),
            Ok(count) => {
                if target.len() + count > limit {
                    return Err(invalid(message));
                }
                target.extend_from_slice(&buffer[..count]);
            }
            Err(error) if error.kind() == io::ErrorKind::WouldBlock => return Ok(true),
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            Err(error) => return Err(error),
        }
    }
}

fn send_result(stream: &mut UnixStream, result: &OperationResult) -> io::Result<()> {
    let header = ReplyHeader {
        exit_code: result.exit_code,
        stdout: result.stdout.len(),
        stderr: result.stderr.len(),
        failure: result.failure.clone(),
    };
    send(stream, &header)?;
    stream.write_all(&result.stdout)?;
    stream.write_all(&result.stderr)
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ReplyHeader {
    exit_code: Option<i32>,
    stdout: usize,
    stderr: usize,
    failure: Option<String>,
}

#[cfg(test)]
fn receive_result(stream: &mut UnixStream, stdout_limit: usize) -> io::Result<OperationResult> {
    let header: ReplyHeader = receive(stream, FAILURE_LIMIT)?;
    if header.stdout > stdout_limit || header.stderr > CAPTURE_LIMIT {
        return Err(invalid("capture frame limit"));
    }
    let deadline = Instant::now() + stream.read_timeout()?.unwrap_or(HANDSHAKE_TIMEOUT);
    let mut stdout = vec![0; header.stdout];
    let mut stderr = vec![0; header.stderr];
    read_until(stream, &mut stdout, deadline)?;
    read_until(stream, &mut stderr, deadline)?;
    Ok(OperationResult {
        exit_code: header.exit_code,
        stdout,
        stderr,
        failure: header.failure,
    })
}

fn send(stream: &mut UnixStream, value: &impl Serialize) -> io::Result<()> {
    let data = serde_json::to_vec(value).map_err(|_| invalid("message encoding"))?;
    if data.len() > REQUEST_LIMIT {
        return Err(invalid("message limit"));
    }
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
            "dashboard-private-success" => {
                "installation::process::tests::dashboard_private_success"
            }
            "dashboard-private-resistant" => {
                "installation::process::tests::dashboard_private_resistant"
            }
            "success" => "installation::process::tests::fixture_success",
            "resistant" => "installation::process::tests::fixture_resistant",
            "leader" => "installation::process::tests::fixture_leader",
            "overflow" => "installation::process::tests::fixture_overflow",
            "resource-gated" => "installation::resource_io::tests::fixture_resource_gated",
            "resource-nonzero" => "installation::resource_io::tests::fixture_resource_nonzero",
            "launch-error" => return Ok(Command::new("/no-such-neo-process-fixture")),
            _ => return Err(invalid("fixture name")),
        };
        let mut command = Command::new(std::env::current_exe()?);
        command.args(["--exact", test, "--ignored", "--nocapture", "--quiet"]);
        Ok(command)
    }

    #[test]
    #[ignore]
    fn dashboard_private_success() {
        println!(
            "PRIVATE-DASHBOARD-STDOUT {}",
            std::env::var("HOME").unwrap()
        );
        eprintln!(
            "PRIVATE-DASHBOARD-STDERR PRIVATE-DASHBOARD-ENV {}",
            std::env::var("HOME").unwrap()
        );
    }

    #[test]
    #[ignore]
    fn dashboard_private_resistant() {
        unsafe {
            libc::signal(libc::SIGTERM, libc::SIG_IGN);
        }
        dashboard_private_success();
        fs::write("dashboard-client-pid.tmp", std::process::id().to_string()).unwrap();
        fs::rename("dashboard-client-pid.tmp", "dashboard-client-pid").unwrap();
        loop {
            std::thread::park();
        }
    }

    fn setup() -> (TempDir, RootLease) {
        let temporary = tempfile::tempdir().unwrap();
        let path = temporary.path().canonicalize().unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o700)).unwrap();
        let lock = RootLease::acquire(&path.join("lock")).unwrap();
        (temporary, lock)
    }

    fn busy(path: &Path) -> bool {
        matches!(
            RootLease::acquire(path),
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

    fn admitted(lock: &RootLease, name: &str) -> OperationOwner {
        let mut owner = OperationOwner::start(lock).unwrap();
        owner.control.set_nonblocking(false).unwrap();
        owner
            .control
            .set_read_timeout(Some(HANDSHAKE_TIMEOUT))
            .unwrap();
        send(
            &mut owner.control,
            &WorkerRequest::Fixture {
                name: name.to_owned(),
            },
        )
        .unwrap();
        owner
    }

    #[test]
    fn observation_worker_requires_private_lease_directory() {
        let temporary = tempfile::tempdir().unwrap();
        let directory = temporary.path().canonicalize().unwrap();
        fs::set_permissions(&directory, fs::Permissions::from_mode(0o755)).unwrap();
        let lock = RootLease::acquire(&directory.join("lock")).unwrap();
        assert!(OperationOwner::start(&lock).is_err());
        fs::set_permissions(&directory, fs::Permissions::from_mode(0o700)).unwrap();
        let result = OperationOwner::start(&lock)
            .unwrap()
            .execute(WorkerRequest::Fixture {
                name: "success".to_owned(),
            })
            .unwrap();
        assert_eq!(result.exit_code, Some(0));
        assert_eq!(result.stdout, b"\nrunning 1 test\nfixed fixture output\n");
        assert!(result.failure.is_none());
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
        fs::write("client-pid.tmp", std::process::id().to_string()).unwrap();
        fs::rename("client-pid.tmp", "client-pid").unwrap();
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
        let (mut writer, mut reader) = UnixStream::pair().unwrap();
        let thread = std::thread::spawn(move || send_result(&mut writer, &result).unwrap());
        let received = receive_result(&mut reader, CAPTURE_LIMIT).unwrap();
        assert_eq!(received.stdout, vec![255; CAPTURE_LIMIT]);
        assert_eq!(received.stderr, vec![255; CAPTURE_LIMIT]);
        assert_eq!(received.exit_code, Some(0));
        thread.join().unwrap();
    }

    #[test]
    fn reply_rejects_oversized_frames_before_allocating_and_rejects_truncation() {
        for (stdout, payload) in [(CAPTURE_LIMIT + 1, Vec::new()), (10, vec![1, 2])] {
            let (mut writer, mut reader) = UnixStream::pair().unwrap();
            send(
                &mut writer,
                &ReplyHeader {
                    exit_code: Some(0),
                    stdout,
                    stderr: 0,
                    failure: None,
                },
            )
            .unwrap();
            writer.write_all(&payload).unwrap();
            writer.shutdown(Shutdown::Write).unwrap();
            assert!(receive_result(&mut reader, CAPTURE_LIMIT).is_err());
        }
    }

    #[test]
    fn metadata_batches_use_bounded_argv_and_generated_cache_authority() {
        let paths = (0..64)
            .map(|n| format!("/nix/store/00000000000000000000000000000000-{n}"))
            .collect::<Vec<_>>();
        let operation = WorkerRequest::PathInfo {
            cache: Some(0),
            paths: paths.clone(),
        };
        let mut command = Command::new("nix");
        append_path_info_args(&mut command, Some(0), &paths).unwrap();
        let args = command.get_args().collect::<Vec<_>>();
        let cache = crate::candidate::declared_cache(0).unwrap();
        assert_eq!(args[0], "--store");
        assert_eq!(args[1], cache.url());
        assert_eq!(
            &args[2..7],
            &["path-info", "--json", "--json-format", "1", "--sigs"]
        );
        assert!(
            args[7..]
                .iter()
                .copied()
                .eq(paths.iter().map(std::ffi::OsStr::new))
        );
        validate_argv_budget(&command).unwrap();
        assert_eq!(operation.stdout_limit().unwrap(), 16 * 1024 * 1024);
        assert!(append_path_info_args(&mut command, Some(usize::MAX), &paths).is_err());
        assert!(
            WorkerRequest::PathInfo {
                cache: None,
                paths: vec![paths[0].clone(); 65]
            }
            .stdout_limit()
            .is_err()
        );
    }

    #[test]
    fn fixed_operation_rejects_foreign_profile() {
        let (_temporary, lock) = setup();
        let directory = lock.path.parent().unwrap();
        let output = PathBuf::from("/nix/store/00000000000000000000000000000000-output");
        let operation = WorkerRequest::BuildProfile { output };
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
    fn worker_accepts_candidate_store_name_grammar() {
        let (_temporary, lock) = setup();
        let directory = lock.path.parent().unwrap();
        let prefix = "/nix/store/00000000000000000000000000000000-";
        let maximum = format!("{prefix}{}", "n".repeat(211));
        let too_long = format!("{prefix}{}", "n".repeat(212));
        let valid = [format!("{prefix}.hidden"), maximum];
        let invalid = [
            "/tmp/output".to_owned(),
            format!("/nix/store/{}-name", "0".repeat(31)),
            format!("/nix/store/{}-name", "e".repeat(32)),
            prefix.to_owned(),
            format!("{prefix}."),
            format!("{prefix}.."),
            format!("{prefix}name.drv"),
            format!("{prefix}name/bin/neo"),
            format!("{prefix}name^out"),
            format!("{prefix}name\n"),
            format!("{prefix}namé"),
            too_long,
        ];

        for output in valid {
            let command = WorkerRequest::BuildProfile {
                output: PathBuf::from(&output),
            }
            .command(directory);
            assert!(command.is_ok(), "worker rejected valid output {output}");
            assert_eq!(
                command.unwrap().get_args().last().unwrap().to_str(),
                Some(output.as_str())
            );
        }
        for output in invalid {
            assert!(
                WorkerRequest::BuildProfile {
                    output: PathBuf::from(&output),
                }
                .command(directory)
                .is_err(),
                "worker accepted invalid output {output}"
            );
        }
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
            .execute(WorkerRequest::Fixture {
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
            .execute(WorkerRequest::Fixture {
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
            .execute(WorkerRequest::Fixture {
                name: "overflow".into(),
            })
            .unwrap();
        assert!(matches!(
            result.failure.as_deref(),
            Some("semantic stdout limit" | "diagnostic stderr limit")
        ));
        assert!(result.stdout.len() <= CAPTURE_LIMIT);
        assert!(result.stderr.len() <= CAPTURE_LIMIT);
    }

    #[test]
    fn launch_failures_leave_no_unowned_work() {
        let (_temporary, lock) = setup();
        assert!(
            OperationOwner::launch(&lock, Some(Command::new("/no-such-neo-worker")))
                .unwrap()
                .wait_ready()
                .is_err()
        );
        assert!(busy(&lock.path));
        let result = OperationOwner::start(&lock)
            .unwrap()
            .execute(WorkerRequest::Fixture {
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
        let mut owner = OperationOwner::start(&lock).unwrap();
        owner
            .submit(WorkerRequest::Fixture {
                name: "resistant".to_owned(),
            })
            .unwrap();
        await_condition(|| {
            assert!(matches!(owner.poll(), ProcessPoll::Pending));
            marker.exists()
        });
        owner.control.write_all(b"unexpected").unwrap();
        await_condition(|| {
            assert!(matches!(owner.poll(), ProcessPoll::Pending));
            matches!(owner.state, ProcessState::End(_))
        });
        let ProcessState::End(result) = &owner.state else {
            unreachable!()
        };
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
        owner.worker.as_mut().unwrap().child.kill().unwrap();
        owner.worker.as_mut().unwrap().child.wait().unwrap();
        drop(owner);
        assert!(busy(&path));
        let length = fs::metadata(&marker).unwrap().len();
        await_condition(|| fs::metadata(&marker).unwrap().len() > length);
        assert!(busy(&path));
        unsafe {
            libc::kill(pid, libc::SIGKILL);
        }
        await_condition(|| !busy(&path));
    }

    #[test]
    #[ignore]
    fn fixture_controller() {
        let path = std::env::current_dir().unwrap().join("lock");
        let lock = RootLease::acquire(&path).unwrap();
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
    fn poll_to_end(owner: &mut OperationOwner) -> ProcessPoll {
        let deadline = Instant::now() + Duration::from_secs(10);
        let mut delay = Duration::from_millis(1);
        loop {
            match owner.poll() {
                ProcessPoll::Pending | ProcessPoll::Ready => {
                    assert!(Instant::now() < deadline, "process completion deadline");
                    std::thread::sleep(delay);
                    delay *= 2;
                }
                result => return result,
            }
        }
    }

    fn peer(lock: &RootLease, mode: &str) -> OperationOwner {
        fs::write(lock.path.parent().unwrap().join("peer-mode"), mode).unwrap();
        let mut command = Command::new(std::env::current_exe().unwrap());
        command.args([
            "--exact",
            "installation::process::tests::protocol_peer",
            "--ignored",
            "--nocapture",
            "--quiet",
        ]);
        OperationOwner::launch(lock, Some(command)).unwrap()
    }

    #[test]
    #[ignore]
    fn protocol_peer() {
        let run = || -> io::Result<()> {
            let mut stream = UnixStream::from(io::stdout().as_fd().try_clone_to_owned()?);
            stream.set_read_timeout(Some(Duration::from_secs(10)))?;
            let identity: LeaseIdentity = receive(&mut stream, REQUEST_LIMIT)?;
            let directory = identity.path.parent().unwrap();
            let mode = fs::read_to_string(directory.join("peer-mode"))?;
            if mode == "oversized-ack" {
                stream.write_all(&u32::MAX.to_be_bytes())?;
                return Ok(());
            }
            let ack = PartialWrite::message(&"lease-owned")?.bytes;
            if mode == "ack-pause" {
                stream.write_all(&ack[..2])?;
                fs::write(directory.join("peer-paused"), b"ack")?;
                let mut byte = [0];
                assert_eq!(stream.read(&mut byte)?, 0);
                return Ok(());
            }
            if mode == "truncated-ack" {
                stream.write_all(&ack[..7])?;
                return Ok(());
            }
            for byte in ack {
                stream.write_all(&[byte])?;
                std::thread::sleep(Duration::from_millis(if mode == "ack-drip" {
                    20
                } else {
                    1
                }));
            }
            let _: WorkerRequest = receive(&mut stream, REQUEST_LIMIT)?;
            if mode == "oversized-header" {
                stream.write_all(&u32::MAX.to_be_bytes())?;
                return Ok(());
            }
            let stdout = match mode.as_str() {
                "cancel-large" => 8 * 1024 * 1024,
                "large" => 128 * 1024,
                _ => 12000,
            };
            let header = ReplyHeader {
                exit_code: Some(0),
                stdout,
                stderr: 3,
                failure: None,
            };
            if mode == "oversized-body" {
                send(
                    &mut stream,
                    &ReplyHeader {
                        stdout: usize::MAX,
                        ..header
                    },
                )?;
                return Ok(());
            }
            let header = PartialWrite::message(&header)?.bytes;
            if mode == "truncated-header" {
                stream.write_all(&header[..7])?;
                return Ok(());
            }
            for byte in header {
                stream.write_all(&[byte])?;
                std::thread::sleep(Duration::from_millis(1));
            }
            if mode == "body-drip" {
                for byte in b"slow body" {
                    stream.write_all(&[*byte])?;
                    std::thread::sleep(Duration::from_millis(30));
                }
                return Ok(());
            }
            if mode == "truncated-body" {
                stream.write_all(b"ab")?;
                return Ok(());
            }
            if mode == "body-pause" {
                stream.write_all(b"abc")?;
                fs::write(directory.join("peer-paused"), b"body")?;
                let mut byte = [0];
                assert_eq!(stream.read(&mut byte)?, 0);
                return Ok(());
            }
            if mode == "cancel-large" {
                stream.set_write_timeout(Some(HANDSHAKE_TIMEOUT))?;
                fs::write(directory.join("peer-paused"), b"reply")?;
                let mut byte = [0];
                assert_eq!(stream.read(&mut byte)?, 0);
            }
            for chunk in vec![0xa5; stdout].chunks(173) {
                stream.write_all(chunk)?;
            }
            stream.write_all(b"err")?;
            if mode == "cancel-large" {
                fs::write(directory.join("reply-complete"), b"complete")?;
            }
            if mode == "excess" {
                stream.write_all(b"x")?;
            }
            Ok(())
        };
        std::process::exit(if run().is_ok() { 0 } else { 125 });
    }

    #[test]
    fn incremental_frames_preserve_fragmented_binary_captures_and_path_info_budget() {
        for (mode, operation, length) in [
            ("fragmented", WorkerRequest::Version, 12000),
            (
                "large",
                WorkerRequest::PathInfo {
                    cache: None,
                    paths: vec!["unused-peer-input".into()],
                },
                128 * 1024,
            ),
        ] {
            let (_temporary, lock) = setup();
            let path = lock.path.clone();
            let owner = peer(&lock, mode);
            drop(lock);
            assert!(busy(&path));
            let result = owner.execute(operation).unwrap();
            assert_eq!(result.stdout, vec![0xa5; length]);
            assert_eq!(result.stderr, b"err");
            assert_eq!(result.exit_code, Some(0));
            assert!(!busy(&path));
        }
    }

    #[test]
    fn incremental_frames_reject_oversized_truncated_and_excess_messages() {
        for mode in [
            "oversized-ack",
            "truncated-ack",
            "oversized-header",
            "truncated-header",
            "oversized-body",
            "truncated-body",
            "excess",
            "large",
        ] {
            let (_temporary, lock) = setup();
            let path = lock.path.clone();
            let owner = peer(&lock, mode);
            drop(lock);
            assert!(owner.execute(WorkerRequest::Version).is_err(), "{mode}");
            assert!(!busy(&path), "{mode}");
        }
    }

    #[test]
    fn cancellation_before_admission_and_during_partial_request_releases_only_after_reap() {
        for partial_request in [false, true] {
            let (_temporary, lock) = setup();
            let path = lock.path.clone();
            let mut owner = OperationOwner::begin(&lock).unwrap();
            if partial_request {
                owner.wait_ready().unwrap();
                owner
                    .submit(WorkerRequest::Fixture {
                        name: "x".repeat(20000),
                    })
                    .unwrap();
                assert!(matches!(owner.poll(), ProcessPoll::Pending));
            }
            drop(lock);
            owner.request_cancel();
            assert!(busy(&path));
            match poll_to_end(&mut owner) {
                ProcessPoll::Quiescent(Err(error)) => {
                    assert_eq!(error.kind(), io::ErrorKind::Interrupted)
                }
                _ => panic!("cancellation did not confirm cleanup"),
            }
            assert!(!busy(&path));
            assert!(!path.parent().unwrap().join("marker").exists());
        }
    }

    #[test]
    fn cancellation_drains_large_reply_before_releasing_lease() {
        drain_large_reply_before_releasing_lease(false);
    }

    #[test]
    fn drop_drains_large_reply_before_releasing_lease() {
        drain_large_reply_before_releasing_lease(true);
    }

    fn drain_large_reply_before_releasing_lease(drop_owner: bool) {
        let (_temporary, lock) = setup();
        let path = lock.path.clone();
        let directory = path.parent().unwrap();
        let mut owner = peer(&lock, "cancel-large");
        owner.wait_ready().unwrap();
        let worker_pid = owner.worker.as_ref().unwrap().child.id() as libc::pid_t;
        owner
            .submit(WorkerRequest::PathInfo {
                cache: None,
                paths: vec!["unused-peer-input".into()],
            })
            .unwrap();
        let deadline = Instant::now() + Duration::from_secs(3);
        while !directory.join("peer-paused").exists() {
            assert!(Instant::now() < deadline, "peer did not reach reply");
            assert!(matches!(owner.poll(), ProcessPoll::Pending));
            owner.wait_for_interest();
        }
        drop(lock);
        assert!(busy(&path));
        let started = Instant::now();
        if drop_owner {
            drop(owner);
        } else {
            owner.request_cancel();
            loop {
                assert!(started.elapsed() < Duration::from_secs(3));
                match owner.poll() {
                    ProcessPoll::Pending => owner.wait_for_interest(),
                    ProcessPoll::Quiescent(Err(error)) => {
                        assert_eq!(error.kind(), io::ErrorKind::Interrupted);
                        break;
                    }
                    _ => panic!("large reply cancellation lost containment"),
                }
            }
        }
        assert!(started.elapsed() < Duration::from_secs(3));
        assert_eq!(
            fs::read(directory.join("reply-complete")).unwrap(),
            b"complete"
        );
        assert_eq!(unsafe { libc::kill(worker_pid, 0) }, -1);
        assert_eq!(io::Error::last_os_error().raw_os_error(), Some(libc::ESRCH));
        assert!(!busy(&path));
    }

    #[test]
    fn partial_ack_and_body_cancellation_and_absolute_deadline_do_not_release_early() {
        for (mode, cancel) in [
            ("ack-pause", true),
            ("body-pause", true),
            ("ack-pause", false),
            ("body-pause", false),
        ] {
            let (_temporary, lock) = setup();
            let path = lock.path.clone();
            let mut owner = peer(&lock, mode);
            if mode == "body-pause" {
                owner.wait_ready().unwrap();
                owner.submit(WorkerRequest::Version).unwrap();
            }
            drop(lock);
            await_condition(|| {
                assert!(matches!(
                    owner.poll(),
                    ProcessPoll::Pending | ProcessPoll::Ready
                ));
                path.parent().unwrap().join("peer-paused").exists()
            });
            if cancel {
                owner.request_cancel();
            } else {
                owner.deadline = Instant::now();
            }
            assert!(busy(&path));
            match poll_to_end(&mut owner) {
                ProcessPoll::Quiescent(Err(error)) => assert_eq!(
                    error.kind(),
                    if cancel {
                        io::ErrorKind::Interrupted
                    } else {
                        io::ErrorKind::TimedOut
                    }
                ),
                _ => panic!("partial frame cleanup not confirmed"),
            }
            assert!(!busy(&path));
        }
    }

    #[test]
    fn incremental_cancel_reaps_resistant_group_before_reporting_quiescence() {
        let (_temporary, lock) = setup();
        let path = lock.path.clone();
        let directory = path.parent().unwrap();
        let mut owner = OperationOwner::start(&lock).unwrap();
        owner
            .submit(WorkerRequest::Fixture {
                name: "resistant".into(),
            })
            .unwrap();
        await_condition(|| {
            assert!(matches!(owner.poll(), ProcessPoll::Pending));
            directory.join("client-pid").exists()
        });
        let pid: i32 = fs::read_to_string(directory.join("client-pid"))
            .unwrap()
            .parse()
            .unwrap();
        drop(lock);
        owner.request_cancel();
        assert!(busy(&path));
        assert!(matches!(
            poll_to_end(&mut owner),
            ProcessPoll::Quiescent(Err(_))
        ));
        assert_eq!(unsafe { libc::kill(-pid, 0) }, -1);
        assert_eq!(io::Error::last_os_error().raw_os_error(), Some(libc::ESRCH));
        assert!(!busy(&path));
    }

    #[test]
    fn killed_worker_is_lost_containment_and_survivor_keeps_exclusion() {
        let (_temporary, lock) = setup();
        let path = lock.path.clone();
        let directory = path.parent().unwrap();
        let mut owner = OperationOwner::start(&lock).unwrap();
        owner
            .submit(WorkerRequest::Fixture {
                name: "resistant".into(),
            })
            .unwrap();
        await_condition(|| {
            assert!(matches!(owner.poll(), ProcessPoll::Pending));
            directory.join("client-pid").exists()
        });
        let pid: i32 = fs::read_to_string(directory.join("client-pid"))
            .unwrap()
            .parse()
            .unwrap();
        drop(lock);
        owner.worker.as_mut().unwrap().child.kill().unwrap();
        assert!(matches!(
            poll_to_end(&mut owner),
            ProcessPoll::ContainmentLost(_)
        ));
        assert!(busy(&path));
        let marker = directory.join("marker");
        let length = fs::metadata(&marker).unwrap().len();
        await_condition(|| fs::metadata(&marker).unwrap().len() > length);
        unsafe {
            libc::kill(pid, libc::SIGKILL);
        }
        await_condition(|| !busy(&path));
    }

    #[test]
    fn partial_progress_does_not_restart_frame_deadlines() {
        for mode in ["ack-drip", "body-drip"] {
            let (_temporary, lock) = setup();
            let path = lock.path.clone();
            let mut owner = peer(&lock, mode);
            if mode == "body-drip" {
                owner.wait_ready().unwrap();
                owner.submit(WorkerRequest::Version).unwrap();
            }
            owner.deadline = Instant::now() + Duration::from_millis(150);
            drop(lock);
            match poll_to_end(&mut owner) {
                ProcessPoll::Quiescent(Err(error)) => {
                    assert_eq!(error.kind(), io::ErrorKind::TimedOut)
                }
                _ => panic!("fragmented traffic extended the absolute deadline"),
            }
            assert!(!busy(&path));
        }
    }
}

#[cfg(test)]
impl OperationOwner {
    pub(super) fn dashboard_kill_worker(&mut self) -> u32 {
        let worker = self.worker.as_mut().unwrap();
        let pid = worker.child.id();
        worker.child.kill().unwrap();
        pid
    }
}
