#[cfg(test)]
use super::super::policy_observation::PolicyInputs;
use super::super::policy_observation::{
    DiscoveredInputs, PolicyInspectionResult, PolicyResult, ReviewedPolicyRequest,
};
use super::{HandoffError, PeerExpectation, PeerIdentity, linux};
use crate::installation::filesystem::{self, PolicyHelperLocation};
use std::fs::{self, File};
use std::mem::{size_of, zeroed};
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
use std::path::{Path, PathBuf};
use std::process::Child;
use std::time::{Duration, Instant};

#[path = "native.rs"]
mod native;
pub(in crate::installation) use native::VerifiedCaller;
pub(crate) use native::{dispatch_existing_policy, dispatch_native_policy};

#[path = "policy_commit.rs"]
mod policy_commit;

pub(super) const DISCOVER: &[u8; 16] = b"NEO-DISCOVER-V01";
pub(super) const INSPECT: &[u8; 16] = b"NEO-RECOVER-V01\0";
pub(super) const INSPECTION: &[u8; 16] = b"NEO-INSPECT-V01\0";

#[derive(Clone, Copy)]
enum HelperRequest {
    Observe,
    Compare(ReviewedPolicyRequest),
    #[cfg(test)]
    Publish(ReviewedPolicyRequest),
    #[cfg(test)]
    Inspect {
        attempt: [u8; 32],
        reviewed: ReviewedPolicyRequest,
    },
    Discover,
}

#[cfg(test)]
const REQUEST: &[u8] = b"NEO-READ-ONLY-V01";
#[cfg(test)]
const PUBLISH: &[u8; 16] = b"NEO-PUBLISH--V01";
#[cfg(test)]
const TIMEOUT: Duration = Duration::from_secs(2);
#[cfg(test)]
const REPLY: &[u8; 16] = b"NEO-OBSERVE-V001";

pub(in crate::installation) struct ExactExecutable {
    file: File,
    path: PathBuf,
    identity: fs::Metadata,
    release: crate::candidate::FileIdentity,
    lifetime: std::sync::Arc<()>,
}

impl ExactExecutable {
    pub(in crate::installation) fn open_installed(
        output: &crate::candidate::OutputPath,
    ) -> Result<Self, HandoffError> {
        native::installed_executable(output)
    }

    pub(in crate::installation) fn verify_existing_policy(
        &self,
        caller: Option<&VerifiedCaller>,
        lease: crate::installation::PolicyStateLease,
        common: [u8; 32],
        cancelled: impl Fn() -> bool,
    ) -> Result<Option<VerifiedExistingPolicy>, HandoffError> {
        self.recheck()?;
        Ok(
            native::verify_existing(self, caller, lease, common, cancelled)?
                .then_some(VerifiedExistingPolicy { lease, common }),
        )
    }

    #[cfg(test)]
    pub(in crate::installation) fn inspect_pending_fixture(
        pending: &crate::installation::PendingNixPolicy,
    ) -> Result<Option<VerifiedPolicyInspection>, HandoffError> {
        let executable = Self::open(Path::new(&pending.helper_path), &pending.helper)?;
        let reply = executable.observe_request(
            Rendezvous::fresh()?,
            LaunchAuthority::SudoManagedFixture,
            HelperRequest::Inspect {
                attempt: pending.attempt,
                reviewed: ReviewedPolicyRequest::from_bytes(pending.reviewed),
            },
        )?;
        match reply.inspection {
            Some(PolicyInspectionResult::VerifiedApplied) => {
                Ok(Some(VerifiedPolicyInspection(pending.clone())))
            }
            Some(PolicyInspectionResult::Pending) => Ok(None),
            None => Err(HandoffError::Frame),
        }
    }

    pub(in crate::installation) fn open(
        path: &Path,
        release: &crate::candidate::FileIdentity,
    ) -> Result<Self, HandoffError> {
        Self::open_at(path, release, PolicyHelperLocation::Store)
    }

    fn open_at(
        path: &Path,
        release: &crate::candidate::FileIdentity,
        location: PolicyHelperLocation,
    ) -> Result<Self, HandoffError> {
        let (file, identity) = filesystem::open_verified_helper(path, release, location)
            .map_err(|_| HandoffError::Executable)?;
        Ok(Self {
            file,
            path: path.to_owned(),
            identity,
            release: release.clone(),
            lifetime: std::sync::Arc::new(()),
        })
    }

    fn recheck(&self) -> Result<(), HandoffError> {
        for metadata in [self.file.metadata(), fs::symlink_metadata(&self.path)] {
            let metadata = metadata.map_err(|_| HandoffError::Executable)?;
            if filesystem::helper_snapshot(&metadata) != filesystem::helper_snapshot(&self.identity)
            {
                return Err(HandoffError::Executable);
            }
        }
        Ok(())
    }

    pub(in crate::installation) fn observe(
        &self,
        rendezvous: Rendezvous,
        authority: LaunchAuthority,
    ) -> Result<QuiescentReply, HandoffError> {
        self.observe_request(rendezvous, authority, HelperRequest::Observe)
    }

    pub(in crate::installation) fn observe_policy(
        &self,
        rendezvous: Rendezvous,
        authority: LaunchAuthority,
        request: ReviewedPolicyRequest,
    ) -> Result<QuiescentReply, HandoffError> {
        self.observe_request(rendezvous, authority, HelperRequest::Compare(request))
    }

    #[cfg(test)]
    pub(in crate::installation) fn pending_policy(
        &self,
        rendezvous: &Rendezvous,
        confirmed: &DiscoveredPolicy,
    ) -> Result<crate::installation::PendingNixPolicy, HandoffError> {
        self.pending(rendezvous, confirmed.comparison(), &confirmed.lifetime)
    }

    pub(in crate::installation) fn pending_stock_policy(
        &self,
        rendezvous: &Rendezvous,
        discovery: StockDiscovery,
    ) -> Result<crate::installation::PendingNixPolicy, HandoffError> {
        self.pending(
            rendezvous,
            discovery.inputs.comparison(),
            &discovery.lifetime,
        )
    }

    fn pending(
        &self,
        rendezvous: &Rendezvous,
        reviewed: ReviewedPolicyRequest,
        discovery_owner: &std::sync::Arc<()>,
    ) -> Result<crate::installation::PendingNixPolicy, HandoffError> {
        if !std::sync::Arc::ptr_eq(&self.lifetime, discovery_owner) {
            return Err(HandoffError::Frame);
        }
        Ok(crate::installation::PendingNixPolicy {
            attempt: rendezvous.epoch,
            reviewed: *reviewed.bytes(),
            helper_path: self.path.to_str().ok_or(HandoffError::Executable)?.into(),
            helper: self.release.clone(),
            state_lease: None,
        })
    }

    #[cfg(test)]
    pub(in crate::installation) fn publish_policy_fixture(
        &self,
        effects: &crate::installation::coordinator::Effects,
        persisted: crate::installation::coordinator::PersistedPolicyIntent,
        rendezvous: Rendezvous,
        authority: LaunchAuthority,
        confirmed: DiscoveredPolicy,
    ) -> Result<QuiescentReply, HandoffError> {
        persisted
            .check_owner(effects)
            .map_err(|_| HandoffError::Frame)?;
        let mut expected = self.pending_policy(&rendezvous, &confirmed)?;
        expected.state_lease = persisted.pending().state_lease;
        if !std::sync::Arc::ptr_eq(&self.lifetime, &confirmed.lifetime)
            || !confirmed.is_confirmed(persisted.binding())
            || persisted.pending() != &expected
        {
            return Err(HandoffError::Frame);
        }
        let authority = match authority {
            LaunchAuthority::PolicyFixture {
                source,
                service,
                executable,
                pid,
                mode,
            } => LaunchAuthority::PolicyFixture {
                mode: if mode == "managed" {
                    "publish-managed"
                } else {
                    "publish"
                },
                source,
                service,
                executable,
                pid,
            },
            LaunchAuthority::SudoManagedFixture => {
                return admin_fixture::observe_managed(
                    self,
                    rendezvous,
                    admin_fixture::ManagedFixtureRequest::Publish(ApprovedPublish {
                        effects,
                        persisted,
                    }),
                );
            }
            _ => return Err(HandoffError::Frame),
        };
        self.observe_request(
            rendezvous,
            authority,
            HelperRequest::Publish(confirmed.comparison()),
        )
    }

    pub(in crate::installation) fn discover_policy(
        &self,
        rendezvous: Rendezvous,
        authority: LaunchAuthority,
    ) -> Result<Result<DiscoveredPolicy, PolicyResult>, HandoffError> {
        let reply = self.observe_request(rendezvous, authority, HelperRequest::Discover)?;
        self.discovered_policy(reply)
    }

    pub(in crate::installation) fn discover_stock_policy(
        &self,
        rendezvous: Rendezvous,
        caller: &VerifiedCaller,
        cancelled: impl Fn() -> bool,
    ) -> Result<Result<StockDiscovery, PolicyResult>, HandoffError> {
        self.recheck()?;
        if !caller.matches_release(&self.release) {
            return Err(HandoffError::Executable);
        }
        let reply = native::discover(self, rendezvous, Some(caller), cancelled)?;
        Ok(reply
            .discovery
            .ok_or(HandoffError::Frame)?
            .map(|inputs| StockDiscovery {
                inputs,
                lifetime: self.lifetime.clone(),
            }))
    }

    pub(in crate::installation) fn publish_stock_policy(
        &self,
        rendezvous: Rendezvous,
        caller: &VerifiedCaller,
        commit: &crate::installation::coordinator::StockPolicyIntent,
        cancelled: impl Fn() -> bool,
    ) -> Result<VerifiedPolicyInspection, HandoffError> {
        let pending = commit.pending();
        if !caller.matches_release(&self.release)
            || pending.helper != self.release
            || Path::new(&pending.helper_path) != self.path
            || pending.attempt != rendezvous.epoch
        {
            return Err(HandoffError::Executable);
        }
        self.recheck()?;
        if native::publish(self, rendezvous, caller, commit, cancelled)?.policy
            != Some(PolicyResult::ManagedRestartObserved)
        {
            return Err(HandoffError::Frame);
        }
        Ok(VerifiedPolicyInspection(pending.clone()))
    }

    pub(in crate::installation) fn inspect_stock_policy(
        &self,
        rendezvous: Rendezvous,
        caller: &VerifiedCaller,
        intent: &crate::installation::coordinator::StockPolicyIntent,
        cancelled: impl Fn() -> bool,
    ) -> Result<Option<VerifiedPolicyInspection>, HandoffError> {
        let pending = intent.pending();
        if !caller.matches_release(&self.release)
            || pending.helper != self.release
            || Path::new(&pending.helper_path) != self.path
        {
            return Err(HandoffError::Executable);
        }
        self.recheck()?;
        let reply = native::inspect(self, rendezvous, caller, intent, cancelled)?;
        match reply.inspection {
            Some(PolicyInspectionResult::VerifiedApplied) => {
                Ok(Some(VerifiedPolicyInspection(pending.clone())))
            }
            Some(PolicyInspectionResult::Pending) => Ok(None),
            None => Err(HandoffError::Frame),
        }
    }

    fn discovered_policy(
        &self,
        reply: QuiescentReply,
    ) -> Result<Result<DiscoveredPolicy, PolicyResult>, HandoffError> {
        Ok(reply
            .discovery
            .ok_or(HandoffError::Frame)?
            .map(|inputs| DiscoveredPolicy {
                inputs,
                release: self.release.clone(),
                path: self.path.clone(),
                lifetime: self.lifetime.clone(),
                check: DiscoveryCheck::Uncompared,
            }))
    }

    pub(in crate::installation) fn confirm_discovery(
        &self,
        effects: &crate::installation::coordinator::Effects,
        binding: &crate::installation::coordinator::PolicyReviewBinding,
        mut discovery: DiscoveredPolicy,
        reviewed: ReviewedPolicyRequest,
        rendezvous: Rendezvous,
        authority: LaunchAuthority,
    ) -> Result<DiscoveredPolicy, HandoffError> {
        effects
            .check_policy_recheck(binding)
            .map_err(|_| HandoffError::Frame)?;
        if !std::sync::Arc::ptr_eq(&self.lifetime, &discovery.lifetime)
            || discovery.inputs.comparison() != reviewed
        {
            return Err(HandoffError::Frame);
        }
        let reply = self.observe_policy(rendezvous, authority, reviewed)?;
        if reply.policy != Some(PolicyResult::MatchingInputs) {
            return Err(HandoffError::Frame);
        }
        effects
            .check_policy_recheck(binding)
            .map_err(|_| HandoffError::Frame)?;
        discovery.check = DiscoveryCheck::Matching(
            effects
                .policy_comparison_witness(binding)
                .map_err(|_| HandoffError::Frame)?,
        );
        Ok(discovery)
    }

    fn observe_request(
        &self,
        rendezvous: Rendezvous,
        authority: LaunchAuthority,
        request: HelperRequest,
    ) -> Result<QuiescentReply, HandoffError> {
        self.recheck()?;
        match authority {
            LaunchAuthority::SudoStock => match request {
                HelperRequest::Discover => native::discover(self, rendezvous, None, || false),
                HelperRequest::Compare(_reviewed) => Err(HandoffError::Frame),
                _ => Err(HandoffError::Frame),
            },
            #[cfg(test)]
            LaunchAuthority::SudoManagedFixture => {
                let request = match request {
                    HelperRequest::Compare(reviewed) => {
                        admin_fixture::ManagedFixtureRequest::Compare(reviewed)
                    }
                    HelperRequest::Discover => admin_fixture::ManagedFixtureRequest::Discover,
                    HelperRequest::Inspect { attempt, reviewed } => {
                        admin_fixture::ManagedFixtureRequest::Inspect { attempt, reviewed }
                    }
                    _ => return Err(HandoffError::Frame),
                };
                admin_fixture::observe_managed(self, rendezvous, request)
            }
            #[cfg(test)]
            LaunchAuthority::SameUidFixture(mode) => {
                let child = self.spawn_fixture(&rendezvous, mode, None)?;
                self.observe_fixture(rendezvous, request, child)
            }
            #[cfg(test)]
            LaunchAuthority::PolicyFixture {
                mode,
                source,
                service,
                executable,
                pid,
            } => {
                let child =
                    self.spawn_fixture(&rendezvous, mode, Some((source, service, executable, pid)))?;
                self.observe_fixture(rendezvous, request, child)
            }
        }
    }

    #[cfg(test)]
    fn observe_fixture(
        &self,
        rendezvous: Rendezvous,
        request: HelperRequest,
        child: Child,
    ) -> Result<QuiescentReply, HandoffError> {
        let mut child = ChildLease(child);
        let identity = PeerIdentity {
            pid: child.0.id() as i32,
            uid: unsafe { libc::getuid() },
            gid: unsafe { libc::getgid() },
        };
        let epoch = rendezvous.epoch;
        let helper = AuthenticatedHelper { identity };
        let endpoint = rendezvous.accept(&helper, &mut child)?;
        self.recheck()?;
        let live = fs::metadata(format!("/proc/{}/exe", identity.pid))
            .map_err(|_| HandoffError::Executable)?;
        if filesystem::helper_snapshot(&live) != filesystem::helper_snapshot(&self.identity) {
            return Err(HandoffError::Executable);
        }
        endpoint.observe(
            child,
            self,
            PeerExpectation { identity, epoch },
            request,
            TIMEOUT,
        )
    }

    #[cfg(test)]
    fn spawn_fixture(
        &self,
        rendezvous: &Rendezvous,
        mode: &str,
        policy: Option<(PathBuf, PathBuf, PathBuf, u32)>,
    ) -> Result<Child, HandoffError> {
        use std::process::{Command, Stdio};
        self.recheck()?;
        let fd = self.file.as_raw_fd();
        let mut command = Command::new(format!("/proc/self/fd/{fd}"));
        command
            .args(["--exact", tests::FIXTURE, "--nocapture"])
            .env_clear()
            .env("NEO_FIXTURE_EPOCH", name(&rendezvous.epoch))
            .env("NEO_FIXTURE_MODE", mode)
            .env("NEO_FIXTURE_PATH", &self.path)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(if mode == "publish-managed" {
                Stdio::inherit()
            } else {
                Stdio::null()
            });
        if let Some((source, service, executable, pid)) = policy {
            command
                .env("NEO_POLICY_SOURCE", source)
                .env("NEO_POLICY_SERVICE", service)
                .env("NEO_POLICY_EXECUTABLE", executable)
                .env("NEO_POLICY_PID", pid.to_string());
        }
        command.spawn().map_err(|_| HandoffError::Executable)
    }
}

#[derive(Clone)]
pub(in crate::installation) enum LaunchAuthority {
    SudoStock,
    #[cfg(test)]
    SudoManagedFixture,
    #[cfg(test)]
    SameUidFixture(&'static str),
    #[cfg(test)]
    PolicyFixture {
        mode: &'static str,
        source: PathBuf,
        service: PathBuf,
        executable: PathBuf,
        pid: u32,
    },
}

enum DiscoveryCheck {
    Uncompared,
    Matching(crate::installation::coordinator::PolicyComparisonWitness),
}

pub(in crate::installation) struct DiscoveredPolicy {
    inputs: DiscoveredInputs,
    release: crate::candidate::FileIdentity,
    path: PathBuf,
    check: DiscoveryCheck,
    lifetime: std::sync::Arc<()>,
}

impl PartialEq for DiscoveredPolicy {
    fn eq(&self, other: &Self) -> bool {
        self.inputs == other.inputs
            && self.release == other.release
            && self.path == other.path
            && std::sync::Arc::ptr_eq(&self.lifetime, &other.lifetime)
    }
}
impl Eq for DiscoveredPolicy {}

impl DiscoveredPolicy {
    pub(in crate::installation) fn matches_helper(
        &self,
        path: &Path,
        release: &crate::candidate::FileIdentity,
    ) -> bool {
        self.path == path && &self.release == release
    }

    pub(in crate::installation) fn is_confirmed(
        &self,
        binding: &crate::installation::coordinator::PolicyReviewBinding,
    ) -> bool {
        matches!(&self.check, DiscoveryCheck::Matching(actual) if actual.same_review(binding))
    }

    pub(in crate::installation) fn common_inputs(&self) -> &[u8; 32] {
        self.inputs.common_inputs()
    }

    pub(in crate::installation) fn comparison(&self) -> ReviewedPolicyRequest {
        self.inputs.comparison()
    }
}

pub(in crate::installation) struct StockDiscovery {
    inputs: DiscoveredInputs,
    lifetime: std::sync::Arc<()>,
}

impl StockDiscovery {
    pub(in crate::installation) fn common_inputs(&self) -> &[u8; 32] {
        self.inputs.common_inputs()
    }
}

pub(in crate::installation) struct QuiescentReply {
    pid: i32,
    observed: &'static str,
    pub(in crate::installation) policy: Option<PolicyResult>,
    discovery: Option<Result<DiscoveredInputs, PolicyResult>>,
    inspection: Option<PolicyInspectionResult>,
    existing: Option<bool>,
}

pub(in crate::installation) struct VerifiedPolicyInspection(crate::installation::PendingNixPolicy);

pub(in crate::installation) struct VerifiedExistingPolicy {
    lease: crate::installation::PolicyStateLease,
    common: [u8; 32],
}

impl VerifiedExistingPolicy {
    pub(in crate::installation) fn matches(
        &self,
        lease: crate::installation::PolicyStateLease,
        common: [u8; 32],
    ) -> bool {
        self.lease == lease && self.common == common
    }
}

impl VerifiedPolicyInspection {
    pub(in crate::installation) fn matches(
        &self,
        pending: &crate::installation::PendingNixPolicy,
    ) -> bool {
        &self.0 == pending
    }
}

pub(in crate::installation) struct Rendezvous {
    listener: OwnedFd,
    epoch: [u8; 32],
}

#[cfg(test)]
struct AuthenticatedHelper {
    identity: PeerIdentity,
}

#[cfg(test)]
struct PeerEndpoint {
    socket: OwnedFd,
}

#[cfg(test)]
struct ApprovedPublish<'a> {
    effects: &'a crate::installation::coordinator::Effects,
    persisted: crate::installation::coordinator::PersistedPolicyIntent,
}

#[cfg(test)]
struct ChildLease(Child);

#[cfg(test)]
impl Drop for ChildLease {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn name(epoch: &[u8; 32]) -> String {
    let mut name = String::from("neo-review-");
    for byte in epoch {
        use std::fmt::Write;
        write!(&mut name, "{byte:02x}").unwrap();
    }
    name
}

fn address(epoch: &[u8; 32]) -> (libc::sockaddr_un, libc::socklen_t) {
    let name = name(epoch);
    let mut address: libc::sockaddr_un = unsafe { zeroed() };
    address.sun_family = libc::AF_UNIX as libc::sa_family_t;
    for (slot, byte) in address.sun_path[1..].iter_mut().zip(name.bytes()) {
        *slot = byte as libc::c_char;
    }
    let length = std::mem::offset_of!(libc::sockaddr_un, sun_path) + 1 + name.len();
    (address, length as libc::socklen_t)
}

fn socket(kind: i32) -> Result<OwnedFd, HandoffError> {
    let fd = unsafe {
        libc::socket(
            libc::AF_UNIX,
            kind | libc::SOCK_CLOEXEC | libc::SOCK_NONBLOCK,
            0,
        )
    };
    if fd < 0 {
        return Err(HandoffError::Socket);
    }
    Ok(unsafe { OwnedFd::from_raw_fd(fd) })
}

fn random_token(flags: libc::c_uint) -> Result<[u8; 32], HandoffError> {
    let mut token = [0u8; 32];
    let mut filled = 0;
    while filled < token.len() {
        let count = unsafe {
            libc::getrandom(
                token[filled..].as_mut_ptr().cast(),
                token.len() - filled,
                flags,
            )
        };
        if count <= 0 {
            return Err(HandoffError::Transport);
        }
        filled += count as usize;
    }
    if token == [0; 32] {
        return Err(HandoffError::Transport);
    }
    Ok(token)
}

impl Rendezvous {
    pub(in crate::installation) fn fresh() -> Result<Self, HandoffError> {
        let epoch = random_token(0)?;
        let listener = socket(libc::SOCK_SEQPACKET)?;
        let enabled: i32 = 1;
        let (address, length) = address(&epoch);
        if unsafe {
            libc::setsockopt(
                listener.as_raw_fd(),
                libc::SOL_SOCKET,
                libc::SO_PASSCRED,
                (&enabled as *const i32).cast(),
                size_of::<i32>() as libc::socklen_t,
            )
        } != 0
            || unsafe {
                libc::bind(
                    listener.as_raw_fd(),
                    (&address as *const libc::sockaddr_un).cast(),
                    length,
                )
            } != 0
            || unsafe { libc::listen(listener.as_raw_fd(), 1) } != 0
        {
            return Err(HandoffError::Socket);
        }
        Ok(Self { listener, epoch })
    }

    #[cfg(test)]
    fn accept(
        self,
        helper: &AuthenticatedHelper,
        child: &mut ChildLease,
    ) -> Result<PeerEndpoint, HandoffError> {
        let socket = self.accept_connection(&mut child.0, Instant::now() + TIMEOUT, || false)?;
        linux::receive_inner(
            &socket,
            &PeerExpectation {
                identity: helper.identity,
                epoch: self.epoch,
            },
        )?;
        if child
            .0
            .try_wait()
            .map_err(|_| HandoffError::Transport)?
            .is_some()
        {
            return Err(HandoffError::Peer);
        }
        Ok(PeerEndpoint { socket })
    }

    fn accept_connection(
        &self,
        child: &mut Child,
        deadline: Instant,
        cancelled: impl Fn() -> bool,
    ) -> Result<OwnedFd, HandoffError> {
        loop {
            if cancelled() {
                return Err(HandoffError::Cancelled);
            }
            if child
                .try_wait()
                .map_err(|_| HandoffError::Transport)?
                .is_some()
            {
                return Err(HandoffError::Peer);
            }
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return Err(HandoffError::Timeout);
            }
            let mut poll = libc::pollfd {
                fd: self.listener.as_raw_fd(),
                events: libc::POLLIN,
                revents: 0,
            };
            let result = unsafe { libc::poll(&mut poll, 1, remaining.as_millis().min(50) as i32) };
            if result < 0 {
                return Err(HandoffError::Transport);
            }
            if result == 0 {
                continue;
            }
            let fd = unsafe {
                libc::accept4(
                    self.listener.as_raw_fd(),
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                    libc::SOCK_CLOEXEC | libc::SOCK_NONBLOCK,
                )
            };
            if fd < 0 {
                return Err(HandoffError::Transport);
            }
            return Ok(unsafe { OwnedFd::from_raw_fd(fd) });
        }
    }
}

#[cfg(test)]
impl PeerEndpoint {
    fn observe(
        self,
        mut child: ChildLease,
        executable: &ExactExecutable,
        expected: PeerExpectation,
        request: HelperRequest,
        timeout: Duration,
    ) -> Result<QuiescentReply, HandoffError> {
        self.send_request(&mut child, request)?;
        if matches!(
            request,
            HelperRequest::Publish(_)
                | HelperRequest::Compare(_)
                | HelperRequest::Discover
                | HelperRequest::Inspect { .. }
        ) {
            let reply_timeout = timeout.max(Duration::from_secs(
                if matches!(
                    request,
                    HelperRequest::Publish(_) | HelperRequest::Inspect { .. }
                ) {
                    15
                } else {
                    3
                },
            ));
            let mut poll = libc::pollfd {
                fd: self.socket.as_raw_fd(),
                events: libc::POLLIN,
                revents: 0,
            };
            if unsafe { libc::poll(&mut poll, 1, reply_timeout.as_millis() as i32) } != 1 {
                return Err(HandoffError::Timeout);
            }
        }
        let mut inspection = None;
        let (policy, discovery) = match request {
            HelperRequest::Compare(_) => (
                Some(
                    PolicyResult::decode(linux::receive_policy(&self.socket, &expected)?)
                        .ok_or(HandoffError::Frame)?,
                ),
                None,
            ),
            HelperRequest::Publish(_) => (
                Some(
                    PolicyResult::decode(linux::receive_policy(&self.socket, &expected)?)
                        .ok_or(HandoffError::Frame)?,
                ),
                None,
            ),
            HelperRequest::Discover => (
                None,
                Some(
                    DiscoveredInputs::decode(linux::receive_discovery(&self.socket, &expected)?)
                        .ok_or(HandoffError::Frame)?,
                ),
            ),
            HelperRequest::Inspect { .. } => {
                inspection = Some(
                    PolicyInspectionResult::decode(linux::receive_policy_inspection(
                        &self.socket,
                        &expected,
                    )?)
                    .ok_or(HandoffError::Frame)?,
                );
                (None, None)
            }
            HelperRequest::Observe => {
                linux::receive_domain(&self.socket, &expected, REPLY)?;
                (None, None)
            }
        };
        self.finish(
            child,
            executable,
            timeout,
            QuiescentReply {
                pid: expected.identity.pid,
                observed: match request {
                    HelperRequest::Publish(_) => "policy publication helper replied",
                    _ => "read-only helper replied",
                },
                policy,
                discovery,
                inspection,
                existing: None,
            },
        )
    }

    fn publish_managed(
        self,
        mut child: ChildLease,
        executable: &ExactExecutable,
        expected: PeerExpectation,
        approved: ApprovedPublish<'_>,
        timeout: Duration,
    ) -> Result<QuiescentReply, HandoffError> {
        use policy_commit::PublishResponse;
        let pending = approved.persisted.pending();
        if pending.attempt != expected.epoch {
            return Err(HandoffError::Frame);
        }
        let reviewed = ReviewedPolicyRequest::from_bytes(pending.reviewed);
        self.send_request(&mut child, HelperRequest::Publish(reviewed))?;
        let deadline = Instant::now() + timeout;
        let result = match PublishResponse::receive(&self.socket, &expected, reviewed, deadline)? {
            PublishResponse::Challenge(challenge) => {
                approved
                    .persisted
                    .check_owner(approved.effects)
                    .map_err(|_| HandoffError::Frame)?;
                executable.recheck()?;
                challenge.send_commit(&self.socket)?;
                match PublishResponse::receive(&self.socket, &expected, reviewed, deadline)? {
                    PublishResponse::Outcome(result) => result,
                    PublishResponse::Challenge(_) => return Err(HandoffError::Frame),
                }
            }
            PublishResponse::Outcome(
                PolicyResult::MatchingInputs
                | PolicyResult::SourcePublishedApplicationPending
                | PolicyResult::ManagedRestartObserved,
            ) => return Err(HandoffError::Frame),
            PublishResponse::Outcome(refused) => refused,
        };
        self.finish(
            child,
            executable,
            deadline.saturating_duration_since(Instant::now()),
            QuiescentReply {
                pid: expected.identity.pid,
                observed: "policy publication helper replied",
                policy: Some(result),
                discovery: None,
                inspection: None,
                existing: None,
            },
        )
    }

    fn finish(
        self,
        mut child: ChildLease,
        executable: &ExactExecutable,
        timeout: Duration,
        reply: QuiescentReply,
    ) -> Result<QuiescentReply, HandoffError> {
        let deadline = Instant::now() + timeout;
        loop {
            if let Some(status) = child.0.try_wait().map_err(|_| HandoffError::Transport)? {
                executable.recheck()?;
                if !status.success() {
                    return Err(HandoffError::Exit);
                }
                linux::require_eof(&self.socket)?;
                return Ok(reply);
            }
            if Instant::now() >= deadline {
                return Err(HandoffError::Timeout);
            }
            std::thread::sleep(Duration::from_millis(10));
        }
    }

    fn send_request(
        &self,
        child: &mut ChildLease,
        request: HelperRequest,
    ) -> Result<(), HandoffError> {
        if child
            .0
            .try_wait()
            .map_err(|_| HandoffError::Transport)?
            .is_some()
        {
            return Err(HandoffError::Peer);
        }
        let mut frame = match request {
            HelperRequest::Discover => DISCOVER.to_vec(),
            HelperRequest::Publish(_) => PUBLISH.to_vec(),
            HelperRequest::Inspect { .. } => INSPECT.to_vec(),
            _ => REQUEST.to_vec(),
        };
        if let HelperRequest::Compare(request) = request {
            frame.extend_from_slice(request.bytes());
        }
        if let HelperRequest::Publish(request) = request {
            frame.extend_from_slice(request.bytes());
        }
        if let HelperRequest::Inspect { attempt, reviewed } = request {
            frame.extend_from_slice(&attempt);
            frame.extend_from_slice(reviewed.bytes());
        }
        let sent = unsafe {
            libc::send(
                self.socket.as_raw_fd(),
                frame.as_ptr().cast(),
                frame.len(),
                libc::MSG_NOSIGNAL | libc::MSG_DONTWAIT,
            )
        };
        if sent != frame.len() as isize {
            return Err(HandoffError::Transport);
        }
        Ok(())
    }
}

#[cfg(test)]
pub(in crate::installation) mod tests {
    use super::super::{DOMAIN, FRAME_BYTES};
    use super::*;
    pub(super) const FIXTURE: &str =
        "installation::nix_owner::peer_handoff::launcher::tests::exec_fixture";

    #[test]
    fn exec_fixture() {
        let Ok(encoded) = std::env::var("NEO_FIXTURE_EPOCH") else {
            return;
        };
        let mode = std::env::var("NEO_FIXTURE_MODE").unwrap();
        if mode == "exit" {
            return;
        }
        if mode == "no-connect" {
            std::thread::sleep(Duration::from_secs(10));
            return;
        }
        let hex = encoded.strip_prefix("neo-review-").unwrap();
        assert_eq!(hex.len(), 64);
        let mut epoch = [0u8; 32];
        for (index, byte) in epoch.iter_mut().enumerate() {
            *byte = u8::from_str_radix(&hex[index * 2..index * 2 + 2], 16).unwrap();
        }
        let sender = socket(libc::SOCK_SEQPACKET).unwrap();
        let enabled: i32 = 1;
        assert_eq!(
            unsafe {
                libc::setsockopt(
                    sender.as_raw_fd(),
                    libc::SOL_SOCKET,
                    libc::SO_PASSCRED,
                    (&enabled as *const i32).cast(),
                    size_of::<i32>() as libc::socklen_t,
                )
            },
            0
        );
        let (address, length) = address(&epoch);
        assert_eq!(
            unsafe {
                libc::connect(
                    sender.as_raw_fd(),
                    (&address as *const libc::sockaddr_un).cast(),
                    length,
                )
            },
            0
        );
        let mut frame = [0u8; FRAME_BYTES];
        frame[..16].copy_from_slice(DOMAIN);
        frame[16..].copy_from_slice(&epoch);
        if mode == "stale" {
            frame[16] ^= 1;
        }
        let sent = unsafe {
            libc::send(
                sender.as_raw_fd(),
                frame.as_ptr().cast(),
                frame.len(),
                libc::MSG_NOSIGNAL,
            )
        };
        if sent < 0 && mode != "ok" {
            return;
        }
        assert_eq!(sent, FRAME_BYTES as isize);
        let mut poll = libc::pollfd {
            fd: sender.as_raw_fd(),
            events: libc::POLLIN,
            revents: 0,
        };
        assert_eq!(unsafe { libc::poll(&mut poll, 1, 3000) }, 1);
        let mut request = [0u8; 64];
        let received = linux::receive_request(&sender, &mut request)
            .map(|n| n as isize)
            .unwrap_or(-1);
        if mode != "stale" {
            let discovering =
                received == DISCOVER.len() as isize && &request[..DISCOVER.len()] == DISCOVER;
            let publishing =
                received == (PUBLISH.len() + 32) as isize && &request[..PUBLISH.len()] == PUBLISH;
            if !discovering && !publishing {
                assert_eq!(&request[..REQUEST.len()], REQUEST);
            }
            let mut discovery = None;
            let policy = std::env::var_os("NEO_POLICY_SOURCE").map(|source| {
                let mut inputs = PolicyInputs::fixture(
                    source.into(),
                    std::env::var_os("NEO_POLICY_SERVICE").unwrap().into(),
                    std::env::var_os("NEO_POLICY_EXECUTABLE").unwrap().into(),
                    std::env::var("NEO_POLICY_PID").unwrap().parse().unwrap(),
                );

                inputs.set_managed_fixture(matches!(mode.as_str(), "managed" | "publish-managed"));
                if discovering {
                    discovery = Some(inputs.discover());
                    return PolicyResult::MatchingInputs;
                }
                let domain_len = if publishing {
                    PUBLISH.len()
                } else {
                    REQUEST.len()
                };
                assert_eq!(received, (domain_len + 32) as isize);
                let commitment = ReviewedPolicyRequest::from_bytes(
                    request[domain_len..received as usize].try_into().unwrap(),
                );
                if publishing {
                    if mode != "publish" && mode != "publish-managed" {
                        return PolicyResult::PublicationRefused;
                    }
                    return inputs.publish_fixture(commitment);
                }
                inputs.evaluate(commitment)
            });
            if policy.is_none() {
                assert_eq!(received, REQUEST.len() as isize);
            }
            if mode == "no-reply" {
                return;
            }
            if mode == "changed-path" {
                let path = PathBuf::from(std::env::var_os("NEO_FIXTURE_PATH").unwrap());
                let moved = path.with_extension("moved");
                fs::rename(&path, &moved).unwrap();
                fs::copy(&moved, &path).unwrap();
            }
            frame[..16].copy_from_slice(REPLY);
            if let Some(result) = policy {
                frame[..16].copy_from_slice(b"NEO-POLICY-V001\0");
                frame[15] = if mode == "invalid-policy" {
                    255
                } else {
                    result as u8
                };
            }
            let mut frame = frame.to_vec();
            if let Some(discovery) = discovery {
                frame[..16].copy_from_slice(DISCOVER);
                let payload = match discovery {
                    Ok(inputs) => inputs.encode(),
                    Err(reason) => {
                        let mut bytes = [0; 65];
                        bytes[0] = reason as u8;
                        bytes
                    }
                };
                frame.extend_from_slice(&payload);
            }
            if mode == "forged-reply" {
                frame[0] ^= 1;
            }
            if mode == "stale-reply" {
                frame[16] ^= 1;
            }
            assert_eq!(
                unsafe {
                    libc::send(
                        sender.as_raw_fd(),
                        frame.as_ptr().cast(),
                        frame.len(),
                        libc::MSG_NOSIGNAL,
                    )
                },
                frame.len() as isize
            );
            if matches!(mode.as_str(), "extra-reply" | "empty-reply") {
                let extra: &[u8] = if mode == "empty-reply" { &[] } else { &frame };
                assert_eq!(
                    unsafe {
                        libc::send(
                            sender.as_raw_fd(),
                            extra.as_ptr().cast(),
                            extra.len(),
                            libc::MSG_NOSIGNAL,
                        )
                    },
                    extra.len() as isize,
                );
            }
            if mode == "hang" {
                std::thread::sleep(Duration::from_secs(10));
            }
            if mode == "nonzero" {
                std::process::exit(7);
            }
        } else {
            assert!(received <= 0, "rejected peer must receive no request");
        }
    }

    pub(in crate::installation) fn prepared() -> (tempfile::TempDir, ExactExecutable) {
        use sha2::{Digest, Sha256};
        let temporary = tempfile::tempdir().unwrap();
        let path = temporary.path().canonicalize().unwrap().join("helper");
        fs::copy(std::env::current_exe().unwrap(), &path).unwrap();
        let bytes = fs::read(&path).unwrap();
        let release = crate::candidate::FileIdentity {
            name: "helper".into(),
            size: bytes.len(),
            sha: format!("{:x}", Sha256::digest(&bytes)),
        };
        let executable =
            ExactExecutable::open_at(&path, &release, PolicyHelperLocation::Source).unwrap();
        (temporary, executable)
    }

    #[test]
    fn exact_exec_reply_is_returned_only_after_reaping() {
        let (_temporary, executable) = prepared();
        let reply = executable
            .observe(
                Rendezvous::fresh().unwrap(),
                LaunchAuthority::SameUidFixture("ok"),
            )
            .unwrap();
        assert_eq!(reply.observed, "read-only helper replied");
        assert_eq!(
            unsafe { libc::waitpid(reply.pid, std::ptr::null_mut(), libc::WNOHANG) },
            -1
        );
        assert_eq!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(libc::ECHILD)
        );
    }

    #[test]
    #[ignore = "requires the pinned disposable Nix store and registered fixture root"]
    fn exact_retained_store_descriptor_exec() {
        let path = std::env::current_exe().unwrap();
        let expected = crate::candidate::FileIdentity {
            name: "neo-policy-helper".into(),
            size: std::env::var("NEO_TEST_PINNED_SIZE")
                .unwrap()
                .parse()
                .unwrap(),
            sha: std::env::var("NEO_TEST_PINNED_SHA").unwrap(),
        };
        let retained = PathBuf::from(std::env::var_os("NEO_TEST_PINNED_ROOT").unwrap());
        assert_eq!(fs::read_link(&retained).unwrap(), path);
        assert_ne!(unsafe { libc::getuid() }, 0);
        let executable = ExactExecutable::open(&path, &expected).unwrap();
        let result = executable
            .observe(
                Rendezvous::fresh().unwrap(),
                LaunchAuthority::SameUidFixture("ok"),
            )
            .unwrap();
        assert_eq!(result.observed, "read-only helper replied");
        assert_eq!(
            unsafe { libc::waitpid(result.pid, std::ptr::null_mut(), libc::WNOHANG) },
            -1,
        );
        assert_eq!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(libc::ECHILD),
        );
        assert_eq!(fs::read_link(&retained).unwrap(), path);
    }

    #[test]
    #[ignore = "requires a root-owned disposable pinned daemon with explicit disabled user sources"]
    fn independently_observes_pinned_daemon_policy() {
        let pid: u32 = std::env::var("NEO_TEST_DAEMON_PID")
            .unwrap()
            .parse()
            .unwrap();
        let daemon = PathBuf::from(std::env::var_os("NEO_TEST_DAEMON_EXECUTABLE").unwrap());
        let source = PathBuf::from("/etc/nix/nix.conf");
        let service = PathBuf::from("/etc/systemd/system/nix-daemon.service");
        let inputs = PolicyInputs::fixture(source.clone(), service.clone(), daemon.clone(), pid);
        let expected = match std::env::var("NEO_TEST_POLICY_EXPECT").as_deref() {
            Ok("unknown-launch") => PolicyResult::UnknownLaunch,
            Ok("inaccessible") => PolicyResult::InaccessibleInputs,
            _ => PolicyResult::MatchingInputs,
        };
        let request = if expected == PolicyResult::MatchingInputs {
            inputs.review().unwrap()
        } else {
            ReviewedPolicyRequest::from_bytes([0; 32])
        };
        let (_temporary, executable) = prepared();
        let reply = executable
            .observe_policy(
                Rendezvous::fresh().unwrap(),
                LaunchAuthority::PolicyFixture {
                    mode: "ok",
                    source: source.clone(),
                    service: service.clone(),
                    executable: daemon.clone(),
                    pid,
                },
                request,
            )
            .unwrap();
        assert_eq!(reply.policy, Some(expected));
        assert_eq!(
            unsafe { libc::waitpid(reply.pid, std::ptr::null_mut(), libc::WNOHANG) },
            -1
        );
        if expected == PolicyResult::MatchingInputs {
            let (_temporary, executable) = prepared();
            assert!(matches!(
                executable.observe_policy(
                    Rendezvous::fresh().unwrap(),
                    LaunchAuthority::PolicyFixture {
                        mode: "invalid-policy",
                        source,
                        service,
                        executable: daemon,
                        pid,
                    },
                    request,
                ),
                Err(HandoffError::Frame)
            ));
        }
    }

    #[test]
    #[ignore = "requires a root-owned disposable pinned daemon with explicit disabled user sources"]
    fn independent_policy_worker_rejects_graph_drift() {
        let pid: u32 = std::env::var("NEO_TEST_DAEMON_PID")
            .unwrap()
            .parse()
            .unwrap();
        let daemon = PathBuf::from(std::env::var_os("NEO_TEST_DAEMON_EXECUTABLE").unwrap());
        for mode in ["source-inode", "nested", "optional", "service", "process"] {
            let (temporary, executable) = prepared();
            let source = temporary.path().join("nix.conf");
            let nested = temporary.path().join("nested.conf");
            let service = temporary.path().join("daemon.service");
            fs::write(&source, "include nested.conf\n!include absent.conf\n").unwrap();
            fs::write(&nested, "max-jobs = 1\n").unwrap();
            fs::write(&service, "fixed service fixture\n").unwrap();
            let inputs =
                PolicyInputs::fixture(source.clone(), service.clone(), daemon.clone(), pid);
            let request = inputs.review().unwrap();
            match mode {
                "source-inode" => {
                    let bytes = fs::read(&source).unwrap();
                    fs::rename(&source, temporary.path().join("old.conf")).unwrap();
                    fs::write(&source, bytes).unwrap();
                }
                "nested" => fs::write(&nested, "max-jobs = 2\n").unwrap(),
                "optional" => {
                    fs::write(temporary.path().join("absent.conf"), "max-jobs = 3\n").unwrap()
                }
                "service" => fs::write(&service, "changed service fixture\n").unwrap(),
                _ => (),
            }
            let reply = executable
                .observe_policy(
                    Rendezvous::fresh().unwrap(),
                    LaunchAuthority::PolicyFixture {
                        mode: "ok",
                        source,
                        service,
                        executable: daemon.clone(),
                        pid: if mode == "process" { 1 } else { pid },
                    },
                    request,
                )
                .unwrap();
            assert_eq!(
                reply.policy,
                Some(if mode == "process" {
                    PolicyResult::InaccessibleInputs
                } else {
                    PolicyResult::ChangedInputs
                }),
                "{mode}"
            );
        }
    }

    #[test]
    fn changed_reply_exit_and_lifetime_cannot_produce_success() {
        for mode in [
            "changed-path",
            "forged-reply",
            "stale-reply",
            "extra-reply",
            "empty-reply",
            "no-reply",
            "nonzero",
            "hang",
            "stale",
            "exit",
            "no-connect",
        ] {
            let (_temporary, executable) = prepared();
            assert!(
                executable
                    .observe(
                        Rendezvous::fresh().unwrap(),
                        LaunchAuthority::SameUidFixture(mode)
                    )
                    .is_err(),
                "{mode}"
            );
        }
    }

    #[test]
    fn discovery_refusal_is_quiescent_and_rejects_corrupted_transport() {
        for mode in [
            "ok",
            "forged-reply",
            "stale-reply",
            "extra-reply",
            "empty-reply",
            "nonzero",
        ] {
            let (temporary, executable) = prepared();
            let source = temporary.path().join("nix.conf");
            let service = temporary.path().join("daemon.service");
            fs::write(&source, "include nix.conf\n").unwrap();
            fs::write(&service, "fixture service\n").unwrap();
            let reply = executable.discover_policy(
                Rendezvous::fresh().unwrap(),
                LaunchAuthority::PolicyFixture {
                    mode,
                    source: source.clone(),
                    service,
                    executable: PathBuf::from("/unavailable-daemon"),
                    pid: 1,
                },
            );
            if mode == "ok" {
                assert!(matches!(reply, Ok(Err(PolicyResult::InaccessibleInputs))));
            } else {
                assert!(reply.is_err(), "{mode}");
            }
            assert_eq!(fs::read(&source).unwrap(), b"include nix.conf\n");
            assert!(
                !source
                    .with_file_name("neohaskell-cache-policy.conf")
                    .exists()
            );
        }
    }

    #[test]
    fn replaced_symlinked_and_modified_files_cannot_launch() {
        use std::os::unix::fs::symlink;
        for mode in ["replace", "symlink", "modify"] {
            let (_temporary, executable) = prepared();
            let path = executable.path.clone();
            if mode == "modify" {
                use std::io::Write;
                fs::OpenOptions::new()
                    .append(true)
                    .open(&path)
                    .unwrap()
                    .write_all(b"changed")
                    .unwrap();
            } else {
                fs::remove_file(&path).unwrap();
                if mode == "replace" {
                    fs::copy(std::env::current_exe().unwrap(), &path).unwrap();
                } else {
                    symlink(std::env::current_exe().unwrap(), &path).unwrap();
                }
            }
            assert!(matches!(
                executable.observe(
                    Rendezvous::fresh().unwrap(),
                    LaunchAuthority::SameUidFixture("ok")
                ),
                Err(HandoffError::Executable)
            ));
        }
    }

    #[test]
    fn namespace_is_fresh_close_on_exec_and_released_on_drop() {
        let first = Rendezvous::fresh().unwrap();
        let second = Rendezvous::fresh().unwrap();
        assert_ne!(first.epoch, second.epoch);
        assert_ne!(
            unsafe { libc::fcntl(first.listener.as_raw_fd(), libc::F_GETFD) } & libc::FD_CLOEXEC,
            0
        );
        let epoch = first.epoch;
        drop(first);
        let sender = socket(libc::SOCK_SEQPACKET).unwrap();
        let (address, length) = address(&epoch);
        assert_eq!(
            unsafe {
                libc::connect(
                    sender.as_raw_fd(),
                    (&address as *const libc::sockaddr_un).cast(),
                    length,
                )
            },
            -1
        );
    }

    #[test]
    fn stream_socket_cannot_enter_the_handoff() {
        let mut fds = [-1; 2];
        assert_eq!(
            unsafe {
                libc::socketpair(
                    libc::AF_UNIX,
                    libc::SOCK_STREAM | libc::SOCK_CLOEXEC,
                    0,
                    fds.as_mut_ptr(),
                )
            },
            0
        );
        let receiver = unsafe { OwnedFd::from_raw_fd(fds[0]) };
        let _sender = unsafe { OwnedFd::from_raw_fd(fds[1]) };
        let expected = PeerExpectation {
            identity: PeerIdentity {
                pid: unsafe { libc::getpid() },
                uid: unsafe { libc::getuid() },
                gid: unsafe { libc::getgid() },
            },
            epoch: [0; 32],
        };
        assert_eq!(
            linux::receive_inner(&receiver, &expected),
            Err(HandoffError::Socket)
        );
    }
}

#[cfg(test)]
#[path = "admin_fixture.rs"]
mod admin_fixture;
