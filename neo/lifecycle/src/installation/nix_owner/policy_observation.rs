use super::*;
#[cfg(target_os = "linux")]
use daemon_process::StockHome;
use daemon_process::UserSources;
use std::io::Read;

#[cfg(target_os = "linux")]
mod continuity;
#[cfg(target_os = "linux")]
mod managed;
#[cfg(target_os = "linux")]
pub(super) use managed::bounded_output;
#[cfg(target_os = "linux")]
mod publication;
#[cfg(all(test, target_os = "linux"))]
pub(super) use publication::FixturePublicationBarrier;
#[cfg(target_os = "linux")]
pub(super) use publication::PolicyAttempt;

#[cfg(target_os = "linux")]
pub(super) enum StockPolicyOutcome {
    Result(PolicyResult),
    AppliedUnchanged([u8; 32]),
}

#[cfg(target_os = "linux")]
pub(super) fn publish_stock_policy(
    operation: PolicyOperation,
    request: ReviewedPolicyRequest,
    attempt: PolicyAttempt,
    caller: StockCaller,
    require_commit: impl FnOnce() -> Result<(), PolicyResult> + Send,
) -> StockPolicyOutcome {
    match managed::mutate_in_stock_view(move |inputs| match operation {
        PolicyOperation::ConventionalAppend => inputs
            .publish_stock(request, attempt, caller, require_commit)
            .map(StockPolicyOutcome::Result),
        PolicyOperation::ApplyUnchanged => inputs
            .apply_stock_unchanged(request, attempt, caller, require_commit)
            .map(StockPolicyOutcome::AppliedUnchanged),
    }) {
        Ok(outcome) => outcome,
        Err(result) => StockPolicyOutcome::Result(result),
    }
}

#[cfg(target_os = "linux")]
pub(super) fn resume_stock_policy(
    request: ReviewedPolicyRequest,
    attempt: PolicyAttempt,
    caller: StockCaller,
    require_commit: impl FnOnce() -> Result<(), PolicyResult> + Send,
) -> StockPolicyOutcome {
    match managed::mutate_in_stock_view(move |inputs| {
        inputs.resume_stock_unchanged(request, attempt, caller, require_commit)
    }) {
        Ok(stamp) => StockPolicyOutcome::AppliedUnchanged(stamp),
        Err(result) => StockPolicyOutcome::Result(result),
    }
}

#[cfg(target_os = "linux")]
pub(super) fn inspect_stock_policy(
    request: ReviewedPolicyRequest,
    attempt: PolicyAttempt,
    caller: StockCaller,
) -> PolicyInspectionResult {
    managed::with_stock_inputs(move |inputs| {
        Ok(inputs.inspect_completed_attempt(&attempt, request, Some(caller)))
    })
    .unwrap_or(PolicyInspectionResult::Pending)
}

#[cfg(target_os = "linux")]
pub(super) fn verify_existing_stock_policy(
    caller_uid: u32,
    lease: crate::installation::PolicyStateLease,
    stamp: [u8; 32],
) -> Result<(), PolicyResult> {
    managed::with_stock_inputs(move |inputs| {
        inputs.verify_existing_receipt(caller_uid, lease, stamp)
    })
}

#[cfg(target_os = "linux")]
pub(crate) const POLICY_OBSERVE_ARGUMENT: &str = "--neo-policy-observe";

#[cfg(target_os = "linux")]
pub(crate) fn run_policy_observer() -> i32 {
    use std::io::Write;

    let frame = stock_policy_frame(None);
    let mut output = io::stdout().lock();
    i32::from(
        output
            .write_all(&frame)
            .and_then(|()| output.flush())
            .is_err(),
    )
}

#[cfg(target_os = "linux")]
pub(super) fn stock_policy_frame(caller: Option<StockCaller>) -> [u8; 65] {
    DiscoveredInputs::encode_result(managed::discover_stock_policy(caller))
}

#[cfg(target_os = "linux")]
impl ConfigurationObservation {
    fn verify_caller_access(&self, caller: StockCaller) -> Result<(), PolicyResult> {
        if self.attachment.access(caller) != caller_access::CallerAccess::Allowed {
            return Err(PolicyResult::InaccessibleInputs);
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::installation) struct ReviewedPolicyRequest([u8; 32]);

#[cfg(target_os = "linux")]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(in crate::installation) enum PolicyInspectionResult {
    Pending = 0,
    VerifiedApplied = 1,
    VerifiedPrepared = 2,
}

#[cfg(target_os = "linux")]
impl PolicyInspectionResult {
    pub(super) fn decode(value: u8) -> Option<Self> {
        match value {
            0 => Some(Self::Pending),
            1 => Some(Self::VerifiedApplied),
            2 => Some(Self::VerifiedPrepared),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(in crate::installation) enum PolicyResult {
    MatchingInputs = 0,
    ChangedInputs = 1,
    InaccessibleInputs = 2,
    UnknownLaunch = 3,
    UnprovedService = 4,
    #[cfg(any(test, target_os = "linux"))]
    SourcePublishedApplicationPending = 5,
    #[cfg(any(test, target_os = "linux"))]
    PublicationRefused = 6,
    #[cfg(any(test, target_os = "linux"))]
    PublicationUnknown = 7,
    #[cfg(target_os = "linux")]
    ManagedRestartObserved = 8,
}

impl PolicyResult {
    pub(super) fn decode(value: u8) -> Option<Self> {
        Some(match value {
            0 => Self::MatchingInputs,
            1 => Self::ChangedInputs,
            2 => Self::InaccessibleInputs,
            3 => Self::UnknownLaunch,
            4 => Self::UnprovedService,
            #[cfg(any(test, target_os = "linux"))]
            5 => Self::SourcePublishedApplicationPending,
            #[cfg(any(test, target_os = "linux"))]
            6 => Self::PublicationRefused,
            #[cfg(any(test, target_os = "linux"))]
            7 => Self::PublicationUnknown,
            #[cfg(target_os = "linux")]
            8 => Self::ManagedRestartObserved,
            _ => return None,
        })
    }
}

impl ReviewedPolicyRequest {
    pub(super) fn bytes(&self) -> &[u8; 32] {
        &self.0
    }
    pub(super) fn from_bytes(bytes: [u8; 32]) -> Self {
        Self(bytes)
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
#[repr(u8)]
pub(in crate::installation) enum PolicyOperation {
    #[default]
    ConventionalAppend = 0,
    ApplyUnchanged = 9,
}

impl PolicyOperation {
    pub(in crate::installation) fn from_tag(value: u8) -> Option<Self> {
        match value {
            0 => Some(Self::ConventionalAppend),
            9 => Some(Self::ApplyUnchanged),
            _ => None,
        }
    }

    pub(in crate::installation) fn is_append(&self) -> bool {
        *self == Self::ConventionalAppend
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::installation) struct DiscoveredInputs {
    commitment: ReviewedPolicyRequest,
    common: [u8; 32],
    operation: PolicyOperation,
}

impl DiscoveredInputs {
    #[cfg(target_os = "linux")]
    pub(super) fn encode_result(result: Result<Self, PolicyResult>) -> [u8; 65] {
        match result {
            Ok(discovered) => discovered.encode(),
            Err(reason) => {
                let mut refusal = [0; 65];
                refusal[0] = reason as u8;
                refusal
            }
        }
    }

    pub(super) fn encode(self) -> [u8; 65] {
        let mut bytes = [0; 65];
        bytes[0] = self.operation as u8;
        bytes[1..33].copy_from_slice(self.commitment.bytes());
        bytes[33..].copy_from_slice(&self.common);
        bytes
    }

    pub(super) fn decode(bytes: [u8; 65]) -> Option<Result<Self, PolicyResult>> {
        if let Some(operation) = PolicyOperation::from_tag(bytes[0]) {
            return Some(Ok(Self {
                commitment: ReviewedPolicyRequest::from_bytes(bytes[1..33].try_into().ok()?),
                common: bytes[33..].try_into().ok()?,
                operation,
            }));
        }
        if bytes[1..] != [0; 64] {
            return None;
        }
        let refusal = PolicyResult::decode(bytes[0])?;
        Some(Err(refusal))
    }

    pub(in crate::installation) fn common_inputs(&self) -> &[u8; 32] {
        &self.common
    }

    pub(in crate::installation) fn comparison(self) -> ReviewedPolicyRequest {
        self.commitment
    }

    pub(in crate::installation) fn operation(&self) -> PolicyOperation {
        self.operation
    }
}

pub(super) struct PolicyInputs {
    source: PathBuf,
    service: PathBuf,
    service_digest: String,
    executable: PathBuf,
    pid: u32,
    #[cfg(test)]
    fixture: bool,
    #[cfg(target_os = "linux")]
    managed: bool,
}

#[derive(PartialEq, Eq, Serialize)]
enum SecondarySources {
    Disabled,
    #[cfg(target_os = "linux")]
    Stock {
        home: ConfigurationFile,
        xdg: ConfigurationFile,
    },
}

impl SecondarySources {
    fn observe(kernel: &KernelDaemonObservation) -> Result<Self, PolicyResult> {
        match kernel.conventional_user_sources() {
            Some(UserSources::Disabled) => Ok(Self::Disabled),
            #[cfg(target_os = "linux")]
            Some(UserSources::Stock(home)) => {
                if home == StockHome::Passwd {
                    managed::verify_stock_account_home()?;
                }
                Ok(Self::Stock {
                    home: missing_secondary(Path::new("/root/.config/nix/nix.conf"))?,
                    xdg: missing_secondary(Path::new("/etc/xdg/nix/nix.conf"))?,
                })
            }
            _ => Err(PolicyResult::UnknownLaunch),
        }
    }
}

#[cfg(target_os = "linux")]
fn missing_secondary(path: &Path) -> Result<ConfigurationFile, PolicyResult> {
    let (source, _) = configuration_source_with(path, &protected_secondary_path, &mut read)
        .map_err(|_| PolicyResult::InaccessibleInputs)?;
    match source {
        ConfigurationFile::Missing { .. } => Ok(source),
        ConfigurationFile::Present { .. } => Err(PolicyResult::UnknownLaunch),
    }
}

#[cfg(target_os = "linux")]
fn protected_secondary_path(path: &Path) -> io::Result<PathBuf> {
    let resolved = protected_path(path)?;
    if resolved != path {
        return Err(io::Error::other("indirect secondary configuration"));
    }
    for ancestor in path.ancestors() {
        let metadata = fs::symlink_metadata(ancestor)?;
        if metadata.uid() != 0
            || metadata.mode() & 0o022 != 0
            || metadata.is_symlink()
            || (ancestor != path && !metadata.is_dir())
        {
            return Err(io::Error::other(
                "unproved secondary configuration ownership",
            ));
        }
    }
    Ok(resolved)
}

fn read(file: &mut File) -> Result<Vec<u8>, TransactionError> {
    let mut bytes = Vec::new();
    file.take(MAX_CONFIGURATION_BYTES as u64 + 1)
        .read_to_end(&mut bytes)
        .map_err(|_| TransactionError::Io)?;
    if bytes.len() > MAX_CONFIGURATION_BYTES {
        return Err(TransactionError::Io);
    }
    Ok(bytes)
}

impl PolicyInputs {
    pub(super) fn conventional(base: &ObservedNixBase) -> Result<Self, PolicyResult> {
        Self::conventional_process(base.service.main_pid)
    }

    fn conventional_process(pid: u32) -> Result<Self, PolicyResult> {
        let identity = target_identity().map_err(|_| PolicyResult::InaccessibleInputs)?;
        Ok(Self {
            source: PathBuf::from("/etc/nix/nix.conf"),
            service: PathBuf::from("/etc/systemd/system/nix-daemon.service"),
            service_digest: identity.service_sha256.to_owned(),
            executable: Path::new(identity.package_root).join("bin/nix-daemon"),
            pid,
            #[cfg(test)]
            fixture: false,
            #[cfg(target_os = "linux")]
            managed: false,
        })
    }

    #[cfg(target_os = "linux")]
    fn secondary_commitment(&self) -> Result<[u8; 32], PolicyResult> {
        let kernel = daemon_process::observe(self.pid, &self.executable, read)
            .map_err(|_| PolicyResult::InaccessibleInputs)?;
        let sources = SecondarySources::observe(&kernel)?;
        let bytes = serde_json::to_vec(&sources).map_err(|_| PolicyResult::InaccessibleInputs)?;
        let mut hash = Sha256::new();
        hash.update(b"NEO-POLICY-SECONDARY-SOURCES-V1\0");
        hash.update(bytes);
        Ok(hash.finalize().into())
    }

    fn capture(&self) -> Result<(ReviewedPolicyRequest, ConfigurationObservation), PolicyResult> {
        let configuration = configuration_input(&self.source, read)
            .map_err(|_| PolicyResult::InaccessibleInputs)?;
        if !matches!(
            configuration.input,
            ConfigurationFile::Present {
                entry: ConfigurationEntry::Direct,
                ..
            }
        ) || !matches!(
            configuration.attachment,
            include_inputs::AttachmentInputs::VerifiedAppend(_)
                | include_inputs::AttachmentInputs::VerifiedExisting(_)
        ) {
            return Err(PolicyResult::InaccessibleInputs);
        }
        let (service, _) = configuration_source_with(&self.service, &protected_path, &mut read)
            .map_err(|_| PolicyResult::InaccessibleInputs)?;
        if !matches!(&service, ConfigurationFile::Present { digest, .. } if digest == &self.service_digest)
        {
            return Err(PolicyResult::ChangedInputs);
        }
        let kernel = daemon_process::observe(self.pid, &self.executable, read)
            .map_err(|_| PolicyResult::InaccessibleInputs)?;
        #[cfg(target_os = "linux")]
        let managed = self.managed_snapshot()?;
        let views = self.namespace_views()?;
        let secondary = SecondarySources::observe(&kernel)?;
        let command = self.command_line()?;
        if command != b"nix-daemon\0--daemon\0" {
            return Err(PolicyResult::UnknownLaunch);
        }
        if kernel
            != daemon_process::observe(self.pid, &self.executable, read)
                .map_err(|_| PolicyResult::InaccessibleInputs)?
            || configuration
                != configuration_input(&self.source, read)
                    .map_err(|_| PolicyResult::InaccessibleInputs)?
            || service
                != configuration_source_with(&self.service, &protected_path, &mut read)
                    .map_err(|_| PolicyResult::InaccessibleInputs)?
                    .0
            || views != self.namespace_views()?
            || command != self.command_line()?
            || secondary != SecondarySources::observe(&kernel)?
        {
            return Err(PolicyResult::ChangedInputs);
        }
        #[cfg(target_os = "linux")]
        if managed != self.managed_snapshot()? {
            return Err(PolicyResult::ChangedInputs);
        }
        let encoded = serde_json::to_vec(&(
            &configuration.input,
            &configuration.attachment,
            &service,
            &kernel,
            &secondary,
            &command,
            &views,
        ))
        .map_err(|_| PolicyResult::InaccessibleInputs)?;
        let mut hash = Sha256::new();
        hash.update(b"NEO-POLICY-INPUTS-V1\0");
        hash.update(encoded);
        #[cfg(target_os = "linux")]
        if let Some(managed) = managed {
            hash.update(
                serde_json::to_vec(&managed).map_err(|_| PolicyResult::InaccessibleInputs)?,
            );
        }
        Ok((ReviewedPolicyRequest(hash.finalize().into()), configuration))
    }

    fn command_line(&self) -> Result<Vec<u8>, PolicyResult> {
        let mut command = File::open(format!("/proc/{}/cmdline", self.pid))
            .map_err(|_| PolicyResult::InaccessibleInputs)?;
        read(&mut command).map_err(|_| PolicyResult::InaccessibleInputs)
    }

    fn namespace_views(&self) -> Result<Vec<(u64, u64)>, PolicyResult> {
        let mut views = Vec::new();
        for (observer, daemon) in [
            (
                PathBuf::from("/"),
                PathBuf::from(format!("/proc/{}/root", self.pid)),
            ),
            (
                PathBuf::from("/proc/thread-self/ns/mnt"),
                PathBuf::from(format!("/proc/{}/ns/mnt", self.pid)),
            ),
        ] {
            let a = fs::metadata(observer).map_err(|_| PolicyResult::InaccessibleInputs)?;
            let b = fs::metadata(daemon).map_err(|_| PolicyResult::InaccessibleInputs)?;
            if (a.dev(), a.ino()) != (b.dev(), b.ino()) {
                return Err(PolicyResult::UnknownLaunch);
            }
            views.push((a.dev(), a.ino()));
        }
        Ok(views)
    }

    pub(super) fn discover(&self) -> Result<DiscoveredInputs, PolicyResult> {
        #[cfg(test)]
        if self.fixture {
            return self.capture_discovery(
                #[cfg(target_os = "linux")]
                None,
            );
        }
        Err(PolicyResult::UnprovedService)
    }

    fn capture_discovery(
        &self,
        #[cfg(target_os = "linux")] caller: Option<StockCaller>,
    ) -> Result<DiscoveredInputs, PolicyResult> {
        let (commitment, configuration) = self.capture()?;
        #[cfg(target_os = "linux")]
        if let Some(caller) = caller {
            configuration.verify_caller_access(caller)?;
        }
        let (operation, commitment) = if configuration.has_complete_stock_policy() {
            #[cfg(target_os = "linux")]
            self.unchanged_stage_absent()?;
            (
                PolicyOperation::ApplyUnchanged,
                unchanged_commitment(commitment),
            )
        } else if matches!(
            configuration.attachment,
            include_inputs::AttachmentInputs::VerifiedAppend(_)
        ) {
            (PolicyOperation::ConventionalAppend, commitment)
        } else {
            return Err(PolicyResult::InaccessibleInputs);
        };
        Ok(DiscoveredInputs {
            commitment,
            common: common_inputs(&configuration, self.pid)?,
            operation,
        })
    }

    #[cfg(target_os = "linux")]
    fn capture_unchanged(
        &self,
    ) -> Result<(ReviewedPolicyRequest, ConfigurationObservation), PolicyResult> {
        let (commitment, configuration) = self.capture()?;
        if !configuration.has_complete_stock_policy() {
            return Err(PolicyResult::InaccessibleInputs);
        }
        Ok((unchanged_commitment(commitment), configuration))
    }

    pub(super) fn review(&self) -> Result<ReviewedPolicyRequest, PolicyResult> {
        self.capture().map(|(commitment, _)| commitment)
    }

    pub(super) fn evaluate(&self, request: ReviewedPolicyRequest) -> PolicyResult {
        match self.capture() {
            Err(reason) => reason,
            Ok((actual, _)) if actual != request => PolicyResult::ChangedInputs,
            Ok(_) => {
                #[cfg(test)]
                if self.fixture {
                    return PolicyResult::MatchingInputs;
                }
                PolicyResult::UnprovedService
            }
        }
    }

    #[cfg(test)]
    pub(super) fn fixture(
        source: PathBuf,
        service: PathBuf,
        executable: PathBuf,
        pid: u32,
    ) -> Self {
        let service_digest = format!("{:x}", Sha256::digest(fs::read(&service).unwrap()));
        Self {
            source,
            service,
            service_digest,
            executable,
            pid,
            fixture: true,
            #[cfg(target_os = "linux")]
            managed: false,
        }
    }
}

fn unchanged_commitment(commitment: ReviewedPolicyRequest) -> ReviewedPolicyRequest {
    let mut hash = Sha256::new();
    hash.update(b"NEO-POLICY-APPLY-UNCHANGED-V1\0");
    hash.update(commitment.bytes());
    ReviewedPolicyRequest::from_bytes(hash.finalize().into())
}

#[cfg(test)]
mod discovery_tests {
    use super::*;

    #[test]
    fn discovery_decodes_only_the_finite_operation_or_redacted_refusal() {
        let mut payload = [7; 65];
        payload[0] = 0;
        let discovered = DiscoveredInputs::decode(payload).unwrap().unwrap();
        assert_eq!(discovered.operation, PolicyOperation::ConventionalAppend);
        assert_eq!(discovered.comparison().bytes(), &[7; 32]);
        assert_eq!(discovered.encode(), payload);
        for reason in 1..=4 {
            let mut refusal = [0; 65];
            refusal[0] = reason;
            assert_eq!(
                DiscoveredInputs::decode(refusal),
                Some(Err(PolicyResult::decode(reason).unwrap()))
            );
            refusal[1] = 1;
            assert!(DiscoveredInputs::decode(refusal).is_none());
        }
        let mut unchanged = payload;
        unchanged[0] = PolicyOperation::ApplyUnchanged as u8;
        let decoded = DiscoveredInputs::decode(unchanged).unwrap().unwrap();
        assert_eq!(decoded.operation(), PolicyOperation::ApplyUnchanged);
        assert_eq!(decoded.encode(), unchanged);
        for invalid in 10..=255 {
            let mut payload = [0; 65];
            payload[0] = invalid;
            assert!(DiscoveredInputs::decode(payload).is_none());
        }
    }
}

fn common_inputs(
    configuration: &ConfigurationObservation,
    pid: u32,
) -> Result<[u8; 32], PolicyResult> {
    let encoded = serde_json::to_vec(&(&configuration.input, &configuration.attachment, pid))
        .map_err(|_| PolicyResult::InaccessibleInputs)?;
    let mut hash = Sha256::new();
    hash.update(b"NEO-POLICY-COMMON-V1\0");
    hash.update(encoded);
    Ok(hash.finalize().into())
}

#[cfg(target_os = "linux")]
fn replacement_input_stamp(
    configuration: &ConfigurationObservation,
    pid: u32,
    started: u64,
    invocation: u128,
    socket: (u64, u64),
) -> Result<[u8; 32], PolicyResult> {
    let mut hash = Sha256::new();
    hash.update(b"NEO-POLICY-REPLACEMENT-V1\0");
    hash.update(common_inputs(configuration, pid)?);
    hash.update(started.to_le_bytes());
    hash.update(invocation.to_le_bytes());
    hash.update(socket.0.to_le_bytes());
    hash.update(socket.1.to_le_bytes());
    Ok(hash.finalize().into())
}

impl ObservedNixBase {
    pub(in crate::installation) fn common_policy_inputs(
        &self,
    ) -> Result<[u8; 32], TransactionError> {
        common_inputs(&self.configuration, self.service.main_pid).map_err(|_| TransactionError::Io)
    }

    #[cfg(target_os = "linux")]
    pub(in crate::installation) fn replacement_policy_stamp(
        &self,
    ) -> Result<[u8; 32], TransactionError> {
        replacement_input_stamp(
            &self.configuration,
            self.service.main_pid,
            self.service.started,
            self.service.invocation_id.get(),
            self.socket,
        )
        .map_err(|_| TransactionError::Io)
    }
}
