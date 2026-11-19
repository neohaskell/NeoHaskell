use super::*;
use crate::candidate::{FileIdentity, OutputPath, native_file_limit};
use crate::installation::{PolicyStateLease, coordinator::StockPolicyIntent};
use policy_commit::{PublishChallenge, PublishResponse};
use std::ffi::OsStr;
use std::io::{Read, Write};
use std::os::unix::fs::MetadataExt;
use std::process::{Command, ExitStatus, Stdio};

const LAUNCH: &str = "--neo-policy-launch";
const OBSERVE: &str = "--neo-policy-observe-peer";
const APPLY: &str = "--neo-policy-publish-peer";
const RECOVER: &str = "--neo-policy-inspect-peer";
const EXISTING: &str = "--neo-policy-existing-peer";
const EXISTING_FRAME: &[u8; 16] = b"NEO-EXISTING-V01";
const STOCK_PUBLISH: &[u8; 16] = b"NEO-STOCK-PUB-V2";
const MAX_BODY: usize = 32 + PolicyScope::BYTES;
const SUDO: &str = "/usr/bin/sudo";
const ACCEPT: &[u8; 16] = b"NEO-ACCEPT-V001\0";
const BUDGET: Duration = Duration::from_secs(20);

pub(crate) fn dispatch_native_policy(arguments: &[std::ffi::OsString]) -> Option<i32> {
    let result = match arguments {
        [argument, rest @ ..] if argument == LAUNCH => match rest {
            [] => launch(),
            _ => Err(HandoffError::Frame),
        },
        [argument, rest @ ..] if argument == OBSERVE => match rest {
            [epoch] => decode_epoch(epoch).and_then(|epoch| serve(epoch, WorkerOperation::Observe)),
            _ => Err(HandoffError::Frame),
        },
        [argument, rest @ ..] if argument == APPLY => match rest {
            [epoch] => decode_epoch(epoch).and_then(|epoch| serve(epoch, WorkerOperation::Apply)),
            _ => Err(HandoffError::Frame),
        },
        [argument, rest @ ..] if argument == RECOVER => match rest {
            [epoch] => decode_epoch(epoch).and_then(|epoch| serve(epoch, WorkerOperation::Inspect)),
            _ => Err(HandoffError::Frame),
        },
        _ => return None,
    };
    Some(match result {
        Ok(()) => 0,
        Err(error) => {
            eprintln!("Nix policy handoff failed ({error:?})");
            1
        }
    })
}

pub(crate) fn dispatch_existing_policy(arguments: &[std::ffi::OsString]) -> Option<i32> {
    let [argument, rest @ ..] = arguments else {
        return None;
    };
    if argument != EXISTING {
        return None;
    }
    let result = match rest {
        [epoch] => decode_epoch(epoch).and_then(|epoch| serve(epoch, WorkerOperation::Existing)),
        _ => Err(HandoffError::Frame),
    };
    Some(i32::from(result.is_err()))
}

fn launch() -> Result<(), HandoffError> {
    ordinary_identity()?;
    let mut current = RunningHelper::open()?;
    let release = current.measure()?;
    let executable = ExactExecutable::open(&current.path, &release)?;
    current.check_process(unsafe { libc::getpid() })?;
    executable.recheck()?;
    let result = executable
        .discover_policy(Rendezvous::fresh()?, LaunchAuthority::SudoStock)?
        .map(|discovery| discovery.inputs);
    let mut output = std::io::stdout().lock();
    output
        .write_all(&DiscoveredInputs::encode_result(result))
        .and_then(|()| output.flush())
        .map_err(|_| HandoffError::Transport)
}

pub(super) fn discover(
    executable: &ExactExecutable,
    rendezvous: Rendezvous,
    downloaded: Option<&VerifiedCaller>,
    cancelled: impl Fn() -> bool,
) -> Result<QuiescentReply, HandoffError> {
    exchange(
        executable,
        rendezvous,
        downloaded,
        PeerRequest::Discover,
        cancelled,
    )
}

pub(super) fn publish(
    executable: &ExactExecutable,
    rendezvous: Rendezvous,
    caller: &VerifiedCaller,
    commit: &StockPolicyIntent,
    cancelled: impl Fn() -> bool,
) -> Result<QuiescentReply, HandoffError> {
    commit.verify().map_err(|_| HandoffError::Frame)?;
    exchange(
        executable,
        rendezvous,
        Some(caller),
        PeerRequest::Publish(commit),
        cancelled,
    )
}

pub(super) fn inspect(
    executable: &ExactExecutable,
    rendezvous: Rendezvous,
    caller: &VerifiedCaller,
    intent: &StockPolicyIntent,
    cancelled: impl Fn() -> bool,
) -> Result<QuiescentReply, HandoffError> {
    intent.verify().map_err(|_| HandoffError::Frame)?;
    exchange(
        executable,
        rendezvous,
        Some(caller),
        PeerRequest::Inspect(intent),
        cancelled,
    )
}

enum PeerRequest<'a> {
    Discover,
    Publish(&'a StockPolicyIntent),
    Inspect(&'a StockPolicyIntent),
    Existing(PolicyScope),
}

impl PeerRequest<'_> {
    fn argument(&self) -> &'static str {
        match self {
            Self::Discover => OBSERVE,
            Self::Publish(_) => APPLY,
            Self::Inspect(_) => RECOVER,
            Self::Existing(_) => EXISTING,
        }
    }
}

pub(super) fn verify_existing(
    executable: &ExactExecutable,
    downloaded: Option<&VerifiedCaller>,
    lease: PolicyStateLease,
    common: [u8; 32],
    cancelled: impl Fn() -> bool,
) -> Result<bool, HandoffError> {
    if downloaded.is_some_and(|caller| !caller.matches_release(&executable.release)) {
        return Err(HandoffError::Executable);
    }
    let scope = PolicyScope {
        inputs: common,
        state_lease: lease,
    };
    exchange(
        executable,
        Rendezvous::fresh()?,
        downloaded,
        PeerRequest::Existing(scope),
        cancelled,
    )?
    .existing
    .ok_or(HandoffError::Frame)
}

pub(super) fn installed_executable(output: &OutputPath) -> Result<ExactExecutable, HandoffError> {
    ordinary_identity()?;
    let mut running = RunningHelper::open_at(PolicyHelperLocation::Installed)?;
    if running.path != Path::new(output.as_str()).join("bin/neo") {
        return Err(HandoffError::Executable);
    }
    let release = running.measure()?;
    ExactExecutable::open_at(&running.path, &release, PolicyHelperLocation::Installed)
}

struct PolicyScope {
    inputs: [u8; 32],
    state_lease: PolicyStateLease,
}

impl PolicyScope {
    const BYTES: usize = 48;

    fn from_intent(intent: &StockPolicyIntent) -> Result<Self, HandoffError> {
        let pending = intent.pending();
        Ok(Self {
            inputs: pending.reviewed,
            state_lease: pending.state_lease.ok_or(HandoffError::Frame)?,
        })
    }

    fn decode(bytes: [u8; Self::BYTES]) -> Result<Self, HandoffError> {
        let device = u64::from_le_bytes(bytes[32..40].try_into().unwrap());
        let inode = u64::from_le_bytes(bytes[40..48].try_into().unwrap());
        if inode == 0 {
            return Err(HandoffError::Frame);
        }
        Ok(Self {
            inputs: bytes[..32].try_into().unwrap(),
            state_lease: PolicyStateLease { device, inode },
        })
    }

    fn encode(&self) -> [u8; Self::BYTES] {
        let mut bytes = [0; Self::BYTES];
        bytes[..32].copy_from_slice(&self.inputs);
        bytes[32..40].copy_from_slice(&self.state_lease.device.to_le_bytes());
        bytes[40..48].copy_from_slice(&self.state_lease.inode.to_le_bytes());
        bytes
    }
}

fn exchange(
    executable: &ExactExecutable,
    rendezvous: Rendezvous,
    downloaded: Option<&VerifiedCaller>,
    request: PeerRequest<'_>,
    cancelled: impl Fn() -> bool,
) -> Result<QuiescentReply, HandoffError> {
    let caller = ordinary_identity()?;
    let recheck_caller = || match downloaded {
        Some(downloaded) => downloaded.recheck(caller.pid),
        None => check_process_file(caller.pid, &executable.identity),
    };
    recheck_caller()?;
    if cancelled() {
        return Err(HandoffError::Cancelled);
    }
    let sudo = ProtectedSudo::open()?;
    let deadline = Instant::now() + BUDGET;
    let label = name(&rendezvous.epoch);
    let mut child = Command::new(SUDO)
        .args(["-n", "--"])
        .arg(&executable.path)
        .args([request.argument(), &label["neo-review-".len()..]])
        .current_dir("/")
        .env_clear()
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|_| HandoffError::Executable)?;

    let mut worker = None;
    let exchange = (|| {
        let launched = ProcessPosition::read(child.id() as i32)?;
        if launched.parent != caller.pid {
            return Err(HandoffError::Peer);
        }

        let socket = rendezvous.accept_connection(&mut child, deadline, &cancelled)?;
        let identity = linux::connected_identity(&socket)?;
        if identity.uid != 0 || identity.gid != 0 {
            return Err(HandoffError::Peer);
        }
        let expected = PeerExpectation {
            identity,
            epoch: rendezvous.epoch,
        };
        let chain = process_chain(identity.pid, launched.pid)?;
        if chain.first() == chain.last() || chain.last() != Some(&launched) {
            return Err(HandoffError::Peer);
        }
        worker = chain.first().copied();
        wait_for_packet(&socket, deadline, &cancelled)?;
        linux::receive_inner(&socket, &expected)?;
        require_same_chain(&chain)?;
        sudo.recheck()?;
        executable.recheck()?;
        recheck_caller()?;
        let mut reply = QuiescentReply {
            pid: expected.identity.pid,
            observed: "authenticated stock helper replied",
            policy: None,
            discovery: None,
            inspection: None,
            existing: None,
        };
        let verify = || {
            require_same_chain(&chain)?;
            sudo.recheck()?;
            executable.recheck()?;
            recheck_caller()
        };
        match request {
            PeerRequest::Publish(commit) => {
                let scope = PolicyScope::from_intent(commit)?;
                let reviewed = ReviewedPolicyRequest::from_bytes(scope.inputs);
                send_packet(&socket, STOCK_PUBLISH, &expected.epoch, &scope.encode())?;
                wait_for_packet(&socket, deadline, &cancelled)?;
                let mut response =
                    PublishResponse::receive(&socket, &expected, reviewed, deadline)?;
                let committed = if let PublishResponse::Challenge(challenge) = response {
                    verify()?;
                    commit.verify().map_err(|_| HandoffError::Frame)?;
                    if cancelled() {
                        return Err(HandoffError::Cancelled);
                    }
                    challenge.send_commit(&socket)?;
                    wait_for_packet(&socket, deadline, &cancelled)?;
                    response = PublishResponse::receive(&socket, &expected, reviewed, deadline)?;
                    true
                } else {
                    false
                };
                let PublishResponse::Outcome(result) = response else {
                    return Err(HandoffError::Frame);
                };
                if result == PolicyResult::ManagedRestartObserved && !committed {
                    return Err(HandoffError::Frame);
                }
                reply.policy = Some(result);
            }
            PeerRequest::Inspect(intent) => {
                let mut body = [0; MAX_BODY];
                body[..32].copy_from_slice(&intent.pending().attempt);
                body[32..].copy_from_slice(&PolicyScope::from_intent(intent)?.encode());
                send_packet(&socket, INSPECT, &expected.epoch, &body)?;
                wait_for_packet(&socket, deadline, &cancelled)?;
                reply.inspection = Some(
                    PolicyInspectionResult::decode(linux::receive_policy_inspection(
                        &socket, &expected,
                    )?)
                    .ok_or(HandoffError::Frame)?,
                );
                intent.verify().map_err(|_| HandoffError::Frame)?;
            }
            PeerRequest::Discover => {
                send_packet(&socket, DISCOVER, &expected.epoch, &[])?;
                wait_for_packet(&socket, deadline, &cancelled)?;
                let frame = linux::receive_discovery(&socket, &expected)?;
                reply.discovery = Some(DiscoveredInputs::decode(frame).ok_or(HandoffError::Frame)?);
            }
            PeerRequest::Existing(scope) => {
                send_packet(&socket, EXISTING_FRAME, &expected.epoch, &scope.encode())?;
                wait_for_packet(&socket, deadline, &cancelled)?;
                reply.existing = Some(
                    match receive_body::<1>(&socket, &expected, EXISTING_FRAME)? {
                        [0] => false,
                        [1] => true,
                        _ => return Err(HandoffError::Frame),
                    },
                );
            }
        }
        verify()?;
        send_packet(&socket, ACCEPT, &expected.epoch, &[])?;
        Ok((socket, reply))
    })();
    drop(rendezvous);
    let status = wait_for_exit(&mut child, deadline)?;
    if status.code().is_none() {
        return Err(HandoffError::CleanupUnknown);
    }
    if let Some(worker) = worker {
        worker.check_exited()?;
    }
    if matches!(&exchange, Err(HandoffError::Cancelled)) {
        return Err(HandoffError::Cancelled);
    }
    if !status.success() {
        return Err(HandoffError::Exit);
    }
    let (socket, reply) = exchange?;
    linux::require_eof(&socket).map_err(|_| HandoffError::CleanupUnknown)?;
    executable.recheck()?;
    recheck_caller()?;
    Ok(reply)
}

enum WorkerOperation {
    Observe,
    Apply,
    Inspect,
    Existing,
}

fn serve(epoch: [u8; 32], operation: WorkerOperation) -> Result<(), HandoffError> {
    if credentials() != (0, 0, 0, 0) {
        return Err(HandoffError::Peer);
    }
    let deadline = Instant::now() + BUDGET;
    let mut executable = match operation {
        WorkerOperation::Existing => RunningHelper::open_at(PolicyHelperLocation::Store)
            .or_else(|_| RunningHelper::open_at(PolicyHelperLocation::Installed))?,
        _ => RunningHelper::open()?,
    };
    let sudo = ProtectedSudo::open()?;
    let socket = connect(&epoch)?;
    let caller = linux::connected_identity(&socket)?;
    let worker = unsafe { libc::getpid() };
    if caller.uid == 0 || caller.gid == 0 {
        return Err(HandoffError::Peer);
    }
    let chain = process_chain(worker, caller.pid)?;
    if chain.len() < 3 {
        return Err(HandoffError::Peer);
    }
    let caller_file = ProcessExecutable::open(caller.pid)?;
    let approved = executable.measure()?;
    if !caller_file.matches_release(&approved)? {
        return Err(HandoffError::Executable);
    }
    let verify = || {
        require_same_chain(&chain)?;
        executable.check_process(worker)?;
        caller_file.recheck(caller.pid)?;
        sudo.recheck()?;
        for process in &chain[1..chain.len() - 1] {
            check_process_file(process.pid, &sudo.0)?;
            if effective_uid(process.pid)? != 0 {
                return Err(HandoffError::Peer);
            }
        }
        require_same_chain(&chain)
    };
    verify()?;
    let expected = PeerExpectation {
        identity: caller,
        epoch,
    };
    send_packet(&socket, super::super::DOMAIN, &epoch, &[])?;
    wait_for_packet(&socket, deadline, || false)?;
    match operation {
        WorkerOperation::Observe => {
            linux::receive_domain(&socket, &expected, DISCOVER)?;
            verify()?;
            let result = super::super::super::policy_observation::stock_policy_frame();
            verify()?;
            send_packet(&socket, DISCOVER, &epoch, &result)?;
        }
        WorkerOperation::Apply => {
            use super::super::super::policy_observation::{PolicyAttempt, publish_stock_policy};

            let scope = PolicyScope::decode(receive_body(&socket, &expected, STOCK_PUBLISH)?)?;
            let reviewed = ReviewedPolicyRequest::from_bytes(scope.inputs);
            verify()?;
            let attempt = PolicyAttempt::authenticated(
                caller.uid,
                epoch,
                &executable.path,
                &approved,
                &executable.identity,
                Some(scope.state_lease),
            );
            let result = publish_stock_policy(reviewed, attempt, || {
                verify().map_err(|_| PolicyResult::PublicationRefused)?;
                PublishChallenge::new(epoch, reviewed)
                    .and_then(|challenge| challenge.require_commit(&socket, caller, deadline))
                    .map_err(|_| PolicyResult::PublicationRefused)?;
                verify().map_err(|_| PolicyResult::PublicationRefused)
            });
            verify()?;
            PublishResponse::send_outcome(&socket, &epoch, result)?;
        }
        WorkerOperation::Inspect => {
            use super::super::super::policy_observation::{PolicyAttempt, inspect_stock_policy};

            let body = receive_body::<MAX_BODY>(&socket, &expected, INSPECT)?;
            let attempt_epoch = body[..32].try_into().unwrap();
            if attempt_epoch == [0; 32] {
                return Err(HandoffError::Frame);
            }
            let scope = PolicyScope::decode(body[32..].try_into().unwrap())?;
            verify()?;
            let attempt = PolicyAttempt::authenticated(
                caller.uid,
                attempt_epoch,
                &executable.path,
                &approved,
                &executable.identity,
                Some(scope.state_lease),
            );
            let result =
                inspect_stock_policy(ReviewedPolicyRequest::from_bytes(scope.inputs), attempt);
            verify()?;
            let mut domain = *INSPECTION;
            domain[15] = result as u8;
            send_packet(&socket, &domain, &epoch, &[])?;
        }
        WorkerOperation::Existing => {
            use super::super::super::policy_observation::verify_existing_stock_policy;

            let scope = PolicyScope::decode(receive_body(&socket, &expected, EXISTING_FRAME)?)?;
            verify()?;
            let result = verify_existing_stock_policy(caller.uid, scope.state_lease, scope.inputs);
            verify()?;
            send_packet(&socket, EXISTING_FRAME, &epoch, &[u8::from(result.is_ok())])?;
        }
    }
    wait_for_packet(&socket, deadline, || false)?;
    linux::receive_domain(&socket, &expected, ACCEPT)?;
    verify()
}

struct RunningHelper {
    path: PathBuf,
    file: File,
    identity: fs::Metadata,
}

impl RunningHelper {
    fn open() -> Result<Self, HandoffError> {
        Self::open_at(PolicyHelperLocation::Store)
    }

    fn open_at(location: PolicyHelperLocation) -> Result<Self, HandoffError> {
        let path = std::env::current_exe().map_err(|_| HandoffError::Executable)?;
        if !filesystem::protected_helper_path(&path, location) {
            return Err(HandoffError::Executable);
        }
        let file = File::open("/proc/self/exe").map_err(|_| HandoffError::Executable)?;
        let identity = file.metadata().map_err(|_| HandoffError::Executable)?;
        if !identity.is_file() || identity.uid() != 0 || identity.mode() & 0o7777 != 0o555 {
            return Err(HandoffError::Executable);
        }
        let current = Self {
            path,
            file,
            identity,
        };
        current.check_process(unsafe { libc::getpid() })?;
        Ok(current)
    }

    fn check_process(&self, pid: i32) -> Result<(), HandoffError> {
        let named = fs::symlink_metadata(&self.path).map_err(|_| HandoffError::Executable)?;
        if filesystem::helper_snapshot(&named) != filesystem::helper_snapshot(&self.identity) {
            return Err(HandoffError::Executable);
        }
        check_process_file(pid, &self.identity)
    }

    fn measure(&mut self) -> Result<FileIdentity, HandoffError> {
        let size = usize::try_from(self.identity.len()).map_err(|_| HandoffError::Executable)?;
        let sha = measure_file(&self.file, size)?;
        self.check_process(unsafe { libc::getpid() })?;
        Ok(FileIdentity {
            name: "neo-policy-helper".into(),
            sha,
            size,
        })
    }
}

fn measure_file(file: &File, size: usize) -> Result<String, HandoffError> {
    use sha2::{Digest, Sha256};

    let limit = native_file_limit().map_err(|_| HandoffError::Executable)?;
    if size == 0 || size > limit {
        return Err(HandoffError::Executable);
    }
    let mut input = file.take(size as u64 + 1);
    let mut digest = Sha256::new();
    let count = std::io::copy(&mut input, &mut digest).map_err(|_| HandoffError::Executable)?;
    if count != size as u64 {
        return Err(HandoffError::Executable);
    }
    Ok(format!("{:x}", digest.finalize()))
}

struct ProcessExecutable {
    file: File,
    identity: fs::Metadata,
}

impl ProcessExecutable {
    fn open(pid: i32) -> Result<Self, HandoffError> {
        let file = File::open(format!("/proc/{pid}/exe")).map_err(|_| HandoffError::Executable)?;
        let identity = file.metadata().map_err(|_| HandoffError::Executable)?;
        if !identity.is_file() {
            return Err(HandoffError::Executable);
        }
        let process = Self { file, identity };
        process.recheck(pid)?;
        Ok(process)
    }

    fn matches_release(&self, approved: &FileIdentity) -> Result<bool, HandoffError> {
        Ok(self.identity.len() == approved.size as u64
            && measure_file(&self.file, approved.size)? == approved.sha)
    }

    fn recheck(&self, pid: i32) -> Result<(), HandoffError> {
        let current = self.file.metadata().map_err(|_| HandoffError::Executable)?;
        if filesystem::helper_snapshot(&current) != filesystem::helper_snapshot(&self.identity) {
            return Err(HandoffError::Executable);
        }
        check_process_file(pid, &self.identity)
    }
}

pub(in crate::installation) struct VerifiedCaller {
    source: PathBuf,
    release: FileIdentity,
    process: ProcessExecutable,
}

impl VerifiedCaller {
    pub(in crate::installation) fn open(
        source: &Path,
        release: &FileIdentity,
        reviewed_identity: &(u64, u64, u64, u32, u32, u32, i64, i64, i64, i64),
    ) -> Result<Self, HandoffError> {
        let pid = ordinary_identity()?.pid;
        let (_file, source_identity) =
            filesystem::open_verified_helper(source, release, PolicyHelperLocation::Source)
                .map_err(|_| HandoffError::Executable)?;
        let process = ProcessExecutable::open(pid)?;
        if filesystem::helper_snapshot(&source_identity) != *reviewed_identity
            || filesystem::helper_snapshot(&process.identity) != *reviewed_identity
        {
            return Err(HandoffError::Executable);
        }
        let caller = Self {
            source: source.to_owned(),
            release: release.clone(),
            process,
        };
        caller.recheck(pid)?;
        Ok(caller)
    }

    pub(in crate::installation) fn open_current(
        release: &FileIdentity,
    ) -> Result<Self, HandoffError> {
        let source = std::env::current_exe()
            .and_then(|path| path.canonicalize())
            .map_err(|_| HandoffError::Executable)?;
        let metadata = fs::symlink_metadata(&source).map_err(|_| HandoffError::Executable)?;
        Self::open(&source, release, &filesystem::helper_snapshot(&metadata))
    }

    pub(super) fn matches_release(&self, other: &FileIdentity) -> bool {
        self.release == *other
    }

    fn recheck(&self, pid: i32) -> Result<(), HandoffError> {
        let named = fs::symlink_metadata(&self.source).map_err(|_| HandoffError::Executable)?;
        if filesystem::helper_snapshot(&named)
            != filesystem::helper_snapshot(&self.process.identity)
        {
            return Err(HandoffError::Executable);
        }
        self.process.recheck(pid)
    }
}

struct ProtectedSudo(fs::Metadata);

impl ProtectedSudo {
    fn open() -> Result<Self, HandoffError> {
        let path = Path::new(SUDO);
        if super::super::super::protected_path(path).ok().as_deref() != Some(path) {
            return Err(HandoffError::Executable);
        }
        let metadata = fs::symlink_metadata(path).map_err(|_| HandoffError::Executable)?;
        if !metadata.is_file()
            || metadata.uid() != 0
            || metadata.gid() != 0
            || metadata.mode() & 0o4122 != 0o4100
        {
            return Err(HandoffError::Executable);
        }
        Ok(Self(metadata))
    }

    fn recheck(&self) -> Result<(), HandoffError> {
        if filesystem::helper_snapshot(&self.0) != filesystem::helper_snapshot(&Self::open()?.0) {
            return Err(HandoffError::Executable);
        }
        Ok(())
    }
}

fn check_process_file(pid: i32, expected: &fs::Metadata) -> Result<(), HandoffError> {
    let running = fs::metadata(format!("/proc/{pid}/exe")).map_err(|_| HandoffError::Executable)?;
    if filesystem::helper_snapshot(&running) != filesystem::helper_snapshot(expected) {
        return Err(HandoffError::Executable);
    }
    Ok(())
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct ProcessPosition {
    pid: i32,
    parent: i32,
    start: u64,
}

impl ProcessPosition {
    fn read(pid: i32) -> Result<Self, HandoffError> {
        if pid <= 1 {
            return Err(HandoffError::Peer);
        }
        let stat = process_data(pid, "stat")?;
        let (name, fields) = stat.rsplit_once(") ").ok_or(HandoffError::Peer)?;
        let (number, _) = name.split_once(" (").ok_or(HandoffError::Peer)?;
        let mut fields = fields.split_ascii_whitespace();
        if number.parse::<i32>().ok() != Some(pid)
            || !matches!(fields.next(), Some("R" | "S" | "D" | "T" | "t" | "I"))
        {
            return Err(HandoffError::Peer);
        }
        let parent = fields
            .next()
            .and_then(|field| field.parse::<i32>().ok())
            .filter(|parent| *parent > 0 && *parent != pid)
            .ok_or(HandoffError::Peer)?;
        let start = fields
            .nth(17)
            .and_then(|field| field.parse::<u64>().ok())
            .filter(|start| *start > 0)
            .ok_or(HandoffError::Peer)?;
        Ok(Self { pid, parent, start })
    }

    fn check_exited(&self) -> Result<(), HandoffError> {
        let path = format!("/proc/{}/stat", self.pid);
        match fs::metadata(path) {
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
            Err(_) => return Err(HandoffError::CleanupUnknown),
            Ok(_) => {}
        }
        match Self::read(self.pid) {
            Ok(current) if current.start != self.start => Ok(()),
            _ => Err(HandoffError::CleanupUnknown),
        }
    }
}

fn process_chain(mut pid: i32, ancestor: i32) -> Result<Vec<ProcessPosition>, HandoffError> {
    let mut process_chain = Vec::new();
    for _ in 0..16 {
        let current = ProcessPosition::read(pid)?;
        process_chain.push(current);
        if pid == ancestor {
            return Ok(process_chain);
        }
        pid = current.parent;
    }
    Err(HandoffError::Peer)
}

fn require_same_chain(chain: &[ProcessPosition]) -> Result<(), HandoffError> {
    for position in chain {
        if ProcessPosition::read(position.pid)? != *position {
            return Err(HandoffError::Peer);
        }
    }
    Ok(())
}

fn process_data(pid: i32, file: &str) -> Result<String, HandoffError> {
    const LIMIT: u64 = 16 * 1024;
    let mut text = String::new();
    let count = File::open(format!("/proc/{pid}/{file}"))
        .and_then(|input| input.take(LIMIT + 1).read_to_string(&mut text))
        .map_err(|_| HandoffError::Peer)?;
    if count as u64 > LIMIT {
        return Err(HandoffError::Peer);
    }
    Ok(text)
}

fn effective_uid(pid: i32) -> Result<u32, HandoffError> {
    process_data(pid, "status")?
        .lines()
        .find_map(|line| line.strip_prefix("Uid:"))
        .and_then(|field| field.split_ascii_whitespace().nth(1))
        .and_then(|uid| uid.parse().ok())
        .ok_or(HandoffError::Peer)
}

fn credentials() -> (u32, u32, u32, u32) {
    unsafe {
        (
            libc::getuid(),
            libc::geteuid(),
            libc::getgid(),
            libc::getegid(),
        )
    }
}

fn ordinary_identity() -> Result<PeerIdentity, HandoffError> {
    let (uid, euid, gid, egid) = credentials();
    if uid == 0 || gid == 0 || uid != euid || gid != egid {
        return Err(HandoffError::Peer);
    }
    Ok(PeerIdentity {
        pid: unsafe { libc::getpid() },
        uid,
        gid,
    })
}

fn decode_epoch(encoded: &OsStr) -> Result<[u8; 32], HandoffError> {
    let text = encoded.to_str().ok_or(HandoffError::Frame)?;
    if text.len() != 64
        || !text
            .bytes()
            .all(|byte| matches!(byte, b'0'..=b'9' | b'a'..=b'f'))
    {
        return Err(HandoffError::Frame);
    }
    let mut epoch = [0; 32];
    for (index, byte) in epoch.iter_mut().enumerate() {
        *byte = u8::from_str_radix(&text[2 * index..2 * index + 2], 16)
            .map_err(|_| HandoffError::Frame)?;
    }
    if epoch == [0; 32] {
        return Err(HandoffError::Frame);
    }
    Ok(epoch)
}

fn connect(epoch: &[u8; 32]) -> Result<OwnedFd, HandoffError> {
    let socket = socket(libc::SOCK_SEQPACKET)?;
    let enabled: i32 = 1;
    let (address, length) = address(epoch);
    if unsafe {
        libc::setsockopt(
            socket.as_raw_fd(),
            libc::SOL_SOCKET,
            libc::SO_PASSCRED,
            (&enabled as *const i32).cast(),
            size_of::<i32>() as libc::socklen_t,
        )
    } != 0
        || unsafe {
            libc::connect(
                socket.as_raw_fd(),
                (&address as *const libc::sockaddr_un).cast(),
                length,
            )
        } != 0
    {
        return Err(HandoffError::Socket);
    }
    Ok(socket)
}

fn receive_body<const N: usize>(
    socket: &OwnedFd,
    expected: &PeerExpectation,
    domain: &[u8; 16],
) -> Result<[u8; N], HandoffError> {
    const HEADER: usize = super::super::FRAME_BYTES;
    let mut frame = [0; HEADER + MAX_BODY];
    let count = linux::receive_request_from(socket, &mut frame, expected.identity)?;
    if count != HEADER + N || &frame[..16] != domain || frame[16..HEADER] != expected.epoch {
        return Err(HandoffError::Frame);
    }
    frame[HEADER..count]
        .try_into()
        .map_err(|_| HandoffError::Frame)
}

fn send_packet(
    socket: &OwnedFd,
    domain: &[u8; 16],
    epoch: &[u8; 32],
    body: &[u8],
) -> Result<(), HandoffError> {
    if body.len() > MAX_BODY {
        return Err(HandoffError::Frame);
    }
    let mut frame = [0; super::super::FRAME_BYTES + MAX_BODY];
    let width = super::super::FRAME_BYTES + body.len();
    frame[..16].copy_from_slice(domain);
    frame[16..super::super::FRAME_BYTES].copy_from_slice(epoch);
    frame[super::super::FRAME_BYTES..width].copy_from_slice(body);
    let sent = unsafe {
        libc::send(
            socket.as_raw_fd(),
            frame.as_ptr().cast(),
            width,
            libc::MSG_NOSIGNAL | libc::MSG_DONTWAIT,
        )
    };
    if sent != width as isize {
        return Err(HandoffError::Transport);
    }
    Ok(())
}

fn wait_for_packet(
    socket: &OwnedFd,
    deadline: Instant,
    cancelled: impl Fn() -> bool,
) -> Result<(), HandoffError> {
    let mut poll = libc::pollfd {
        fd: socket.as_raw_fd(),
        events: libc::POLLIN,
        revents: 0,
    };
    loop {
        if cancelled() {
            return Err(HandoffError::Cancelled);
        }
        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() {
            return Err(HandoffError::Timeout);
        }
        match unsafe { libc::poll(&mut poll, 1, remaining.as_millis().min(50) as i32) } {
            0 => continue,
            1 if poll.revents & libc::POLLIN != 0 => return Ok(()),
            _ => return Err(HandoffError::Transport),
        }
    }
}

fn wait_for_exit(child: &mut Child, deadline: Instant) -> Result<ExitStatus, HandoffError> {
    loop {
        if let Some(status) = child.try_wait().map_err(|_| HandoffError::CleanupUnknown)? {
            return Ok(status);
        }
        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() {
            return Err(HandoffError::CleanupUnknown);
        }
        std::thread::sleep(remaining.min(Duration::from_millis(10)));
    }
}
