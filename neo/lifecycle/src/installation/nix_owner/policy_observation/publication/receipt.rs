use super::super::managed::{self, ManagedSnapshot};
use super::*;
use crate::installation::PolicyStateLease;
use serde::Deserialize;
use std::num::NonZeroU32;
use std::os::unix::fs::PermissionsExt;

const RECEIPT: &str = "receipt.json";
const RECEIPT_TEMP: &str = "receipt.json.tmp";
const MAX_RECEIPT_BYTES: usize = 8 * 1024;

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub(in crate::installation::nix_owner) struct PolicyAttempt {
    caller_uid: u32,
    attempt_epoch: [u8; 32],
    helper: HelperIdentity,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    state_lease: Option<PolicyStateLease>,
}

impl PolicyAttempt {
    pub(in crate::installation::nix_owner) fn authenticated(
        caller_uid: u32,
        attempt_epoch: [u8; 32],
        path: &Path,
        release: &crate::candidate::FileIdentity,
        metadata: &fs::Metadata,
        state_lease: Option<PolicyStateLease>,
    ) -> Self {
        Self {
            caller_uid,
            attempt_epoch,
            state_lease,
            helper: HelperIdentity {
                path: path.to_owned(),
                sha256: release.sha.clone(),
                size: metadata.len(),
                device: metadata.dev(),
                inode: metadata.ino(),
            },
        }
    }

    fn valid(&self) -> bool {
        self.state_lease.is_none_or(|lease| lease.inode != 0)
            && self.caller_uid != 0
            && self.attempt_epoch != [0; 32]
            && self
                .helper
                .path
                .to_str()
                .is_some_and(|path| crate::candidate::OutputPath::parse(path).is_ok())
            && lowercase_hex(&self.helper.sha256, 64)
            && self.helper.size != 0
            && self.helper.inode != 0
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct HelperIdentity {
    path: PathBuf,
    sha256: String,
    size: u64,
    device: u64,
    inode: u64,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct FileEvidence {
    identity: InputIdentity,
    sha256: String,
}

impl FileEvidence {
    fn capture(configuration: &ConfigurationFile) -> Result<Self, PolicyResult> {
        let ConfigurationFile::Present {
            entry: ConfigurationEntry::Direct,
            file,
            digest,
            ..
        } = configuration
        else {
            return Err(PolicyResult::PublicationUnknown);
        };
        Ok(Self {
            identity: file.clone(),
            sha256: digest.clone(),
        })
    }

    fn valid(&self) -> bool {
        let identity = &self.identity;
        identity.mode & libc::S_IFMT == libc::S_IFREG
            && identity.mode & 0o7022 == 0
            && identity.uid == 0
            && identity.links == 1
            && identity.inode != 0
            && identity.size <= MAX_CONFIGURATION_BYTES as u64
            && lowercase_hex(&self.sha256, 64)
    }

    fn verify_moved(&self, path: &Path) -> Result<(ConfigurationFile, Vec<u8>), PolicyResult> {
        let file = open(path)?;
        let identity = InputIdentity::of(&plain(&file)?);
        anchored(&file, path)?;
        let (observed, bytes) = configuration_source_with(path, &protected_path, &mut read)
            .map_err(|_| PolicyResult::PublicationUnknown)?;
        let mut actual = Self::capture(&observed)?;
        if actual.identity != identity || actual.identity.changed < self.identity.changed {
            return Err(PolicyResult::PublicationUnknown);
        }
        actual.identity.changed = self.identity.changed;
        if &actual != self {
            return Err(PolicyResult::PublicationUnknown);
        }
        anchored(&file, path)?;
        Ok((observed, bytes))
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct ManagerIdentity {
    pid: u32,
    started_monotonic: u64,
    invocation_id: String,
    socket: (u64, u64),
    listener: u64,
    snapshot_sha256: [u8; 32],
}

impl ManagerIdentity {
    fn capture(snapshot: &ManagedSnapshot) -> Result<Self, PolicyResult> {
        Ok(Self {
            pid: snapshot.pid,
            started_monotonic: snapshot.started,
            invocation_id: format!("{:032x}", snapshot.invocation),
            socket: snapshot.socket,
            listener: snapshot.listener,
            snapshot_sha256: commitment(b"NEO-POLICY-MANAGER-V1\0", snapshot)?,
        })
    }

    fn valid(&self) -> bool {
        self.pid > 1
            && self.started_monotonic > 0
            && lowercase_hex(&self.invocation_id, 32)
            && self.invocation_id.bytes().any(|byte| byte != b'0')
            && self.socket.1 != 0
            && self.listener != 0
    }

    fn replaces(&self, old: &Self) -> bool {
        self.valid()
            && self.pid != old.pid
            && self.invocation_id != old.invocation_id
            && self.started_monotonic > old.started_monotonic
            && self.socket == old.socket
            && self.listener == old.listener
    }

    fn follows_proof(&self, old: &Self, continuity: &ReceiptContinuity) -> bool {
        self.replaces(old) && self.started_monotonic > continuity.verified_monotonic
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct ReceiptContinuity {
    inputs_sha256: [u8; 32],
    verified_monotonic: u64,
}

fn decode_continuity<'de, D>(deserializer: D) -> Result<Option<ReceiptContinuity>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    ReceiptContinuity::deserialize(deserializer).map(Some)
}

fn valid_continuity(
    version: u8,
    continuity: Option<&ReceiptContinuity>,
    replacement: Option<&ManagerIdentity>,
) -> bool {
    match (version, continuity, replacement) {
        (5, Some(proof), Some(replacement)) => {
            proof.inputs_sha256 != [0; 32]
                && proof.verified_monotonic >= replacement.started_monotonic
        }
        (5, _, _) | (_, Some(_), _) => false,
        (_, None, _) => true,
    }
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct RestartIntent {
    intent_monotonic: u64,
    inputs_sha256: [u8; 32],
}

impl RestartIntent {
    fn valid_since(self, old: &ManagerIdentity) -> bool {
        self.intent_monotonic > old.started_monotonic && self.inputs_sha256 != [0; 32]
    }

    fn admits_replacement(self, replacement: &ManagerIdentity) -> bool {
        replacement.started_monotonic > self.intent_monotonic
    }

    fn verify_completion(
        self,
        replacement: &ManagerIdentity,
        continuity: Option<&ReceiptContinuity>,
    ) -> Result<(), PolicyResult> {
        if self.admits_replacement(replacement)
            && continuity.is_some_and(|proof| proof.inputs_sha256 == self.inputs_sha256)
        {
            Ok(())
        } else {
            Err(PolicyResult::PublicationUnknown)
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct AcceptedRestart {
    intent: RestartIntent,
    job_id: NonZeroU32,
}

fn valid_restart_intent(version: u8, old: &ManagerIdentity, intent: Option<RestartIntent>) -> bool {
    match (version, intent) {
        (6, Some(intent)) => intent.valid_since(old),
        (6, None) | (_, Some(_)) => false,
        (_, None) => true,
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
enum ReceiptPhase {
    Prepared {},
    RestartMayBeQueued {},
    RestartArmed { intent: RestartIntent },
    RestartAccepted { accepted: AcceptedRestart },
    RestartObserved { replacement: ManagerIdentity },
}

impl ReceiptPhase {
    fn intent(&self) -> Option<RestartIntent> {
        match self {
            Self::RestartArmed { intent } => Some(*intent),
            Self::RestartAccepted { accepted } => Some(accepted.intent),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
enum UnchangedReceiptPhase {
    Prepared {},
    RestartMayBeQueued {
        intent_monotonic: u64,
    },
    RestartArmed {
        intent: RestartIntent,
    },
    RestartAccepted {
        accepted: AcceptedRestart,
    },
    RestartObserved {
        intent_monotonic: u64,
        replacement: ManagerIdentity,
    },
}

impl UnchangedReceiptPhase {
    fn intent(&self) -> Option<RestartIntent> {
        match self {
            Self::RestartArmed { intent } => Some(*intent),
            Self::RestartAccepted { accepted } => Some(accepted.intent),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
enum UnchangedEvidence {
    AppliedUnchanged { configuration_sha256: [u8; 32] },
}

impl UnchangedEvidence {
    fn capture(configuration: &ConfigurationObservation) -> Result<Self, PolicyResult> {
        if !configuration.has_complete_stock_policy() {
            return Err(PolicyResult::PublicationUnknown);
        }
        Ok(Self::AppliedUnchanged {
            configuration_sha256: commitment(
                b"NEO-POLICY-UNCHANGED-CONFIGURATION-V1\0",
                &(&configuration.input, &configuration.attachment),
            )?,
        })
    }

    fn verify(&self) -> Result<(), PolicyResult> {
        let actual = configuration_input(Path::new(SOURCE), read)
            .map_err(|_| PolicyResult::InaccessibleInputs)?;
        if Self::capture(&actual)? != *self {
            return Err(PolicyResult::ChangedInputs);
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct UnchangedReceipt {
    version: u8,
    operation: PolicyAttempt,
    reviewed: [u8; 32],
    evidence: UnchangedEvidence,
    secondary_sources_sha256: [u8; 32],
    boot_id: String,
    old_manager: ManagerIdentity,
    phase: UnchangedReceiptPhase,
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "decode_continuity"
    )]
    continuity: Option<ReceiptContinuity>,
}

impl UnchangedReceipt {
    fn matches_prepared(&self, operation: &PolicyAttempt, request: ReviewedPolicyRequest) -> bool {
        matches!(self.phase, UnchangedReceiptPhase::Prepared {})
            && &self.operation == operation
            && &self.reviewed == request.bytes()
    }

    fn valid(&self) -> bool {
        let replacement = match &self.phase {
            UnchangedReceiptPhase::RestartObserved { replacement, .. } => Some(replacement),
            _ => None,
        };
        matches!(self.version, 4 | 5 | 6)
            && valid_continuity(self.version, self.continuity.as_ref(), replacement)
            && valid_restart_intent(self.version, &self.old_manager, self.phase.intent())
            && self.operation.state_lease.is_some()
            && self.operation.valid()
            && valid_boot_id(&self.boot_id)
            && self.old_manager.valid()
            && match &self.phase {
                UnchangedReceiptPhase::Prepared {}
                | UnchangedReceiptPhase::RestartArmed { .. }
                | UnchangedReceiptPhase::RestartAccepted { .. } => true,
                UnchangedReceiptPhase::RestartMayBeQueued { intent_monotonic } => {
                    *intent_monotonic > self.old_manager.started_monotonic
                }
                UnchangedReceiptPhase::RestartObserved {
                    intent_monotonic,
                    replacement,
                } => {
                    *intent_monotonic > self.old_manager.started_monotonic
                        && replacement.started_monotonic > *intent_monotonic
                        && replacement.replaces(&self.old_manager)
                }
            }
    }

    fn verify_unchanged(&self, stage: &File) -> Result<(), PolicyResult> {
        let mut entries = fs::read_dir(descriptor_path(stage, "."))
            .map_err(|_| PolicyResult::PublicationUnknown)?;
        if !matches!(entries.next(), Some(Ok(entry)) if entry.file_name() == RECEIPT)
            || entries.next().is_some()
        {
            return Err(PolicyResult::PublicationUnknown);
        }
        self.evidence.verify()
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct PublishedReceipt {
    version: u8,
    operation: PolicyAttempt,
    reviewed: [u8; 32],
    baseline: FileEvidence,
    staged: FileEvidence,
    leaf: FileEvidence,
    include_graph_sha256: [u8; 32],
    #[serde(default, skip_serializing_if = "Option::is_none")]
    secondary_sources_sha256: Option<[u8; 32]>,
    boot_id: String,
    old_manager: ManagerIdentity,
    phase: ReceiptPhase,
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "decode_continuity"
    )]
    continuity: Option<ReceiptContinuity>,
}

impl PublishedReceipt {
    fn valid(&self) -> bool {
        let replacement = match &self.phase {
            ReceiptPhase::RestartObserved { replacement } => Some(replacement),
            _ => None,
        };
        matches!(
            (
                self.version,
                self.secondary_sources_sha256,
                self.operation.state_lease,
            ),
            (1, None, None) | (2, Some(_), None) | (3 | 5 | 6, Some(_), Some(_))
        ) && valid_continuity(self.version, self.continuity.as_ref(), replacement)
            && valid_restart_intent(self.version, &self.old_manager, self.phase.intent())
            && self.operation.valid()
            && self.baseline.valid()
            && self.staged.valid()
            && self.leaf.valid()
            && distinct_files([&self.baseline, &self.staged, &self.leaf])
            && valid_boot_id(&self.boot_id)
            && self.old_manager.valid()
            && !matches!(
                &self.phase,
                ReceiptPhase::RestartObserved { replacement }
                    if !replacement.replaces(&self.old_manager)
            )
    }

    fn advance(&self, phase: ReceiptPhase) -> Result<Self, PolicyResult> {
        match (&self.phase, &phase) {
            (
                ReceiptPhase::Prepared {},
                ReceiptPhase::RestartMayBeQueued {} | ReceiptPhase::RestartArmed { .. },
            ) => {}
            (ReceiptPhase::RestartArmed { intent }, ReceiptPhase::RestartAccepted { accepted })
                if intent == &accepted.intent => {}
            (
                ReceiptPhase::RestartMayBeQueued {}
                | ReceiptPhase::RestartArmed { .. }
                | ReceiptPhase::RestartAccepted { .. },
                ReceiptPhase::RestartObserved { replacement },
            ) if replacement.replaces(&self.old_manager) => {}
            _ => return Err(PolicyResult::PublicationUnknown),
        }
        Ok(Self {
            phase,
            ..self.clone()
        })
    }

    fn verify_published(&self) -> Result<(), PolicyResult> {
        let path = Path::new(SOURCE);
        let retained = Path::new(STAGE).join("main-slot");
        require_absent(&Path::new(STAGE).join("leaf-slot"))?;
        let (_, original) = self.baseline.verify_moved(&retained)?;
        let (live, source) = self.staged.verify_moved(path)?;
        let (_, companion) = self.leaf.verify_moved(&path.with_file_name(POLICY_LEAF))?;
        let NixIncludePreparation::Append(desired) = NixIncludePreparation::from_input(
            &ConfigurationEntry::Direct,
            &self.baseline.sha256,
            &original,
        ) else {
            return Err(PolicyResult::PublicationUnknown);
        };
        if source != desired.source || companion != desired.leaf {
            return Err(PolicyResult::PublicationUnknown);
        }
        let preparation = NixIncludePreparation::from_input(
            &ConfigurationEntry::Direct,
            &self.staged.sha256,
            &source,
        );
        let (attachment, _) = include_inputs::observe(
            path,
            &live,
            &preparation,
            &source,
            &protected_configuration_path,
            &mut read,
        )
        .map_err(|_| PolicyResult::PublicationUnknown)?;
        let baseline = include_inputs::published_baseline(path, attachment)
            .ok_or(PolicyResult::PublicationUnknown)?;
        if commitment(b"NEO-POLICY-INCLUDE-GRAPH-V1\0", &baseline)? != self.include_graph_sha256 {
            return Err(PolicyResult::PublicationUnknown);
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(untagged)]
enum PolicyReceipt {
    Published(PublishedReceipt),
    Unchanged(UnchangedReceipt),
}

struct LiveReceipt<'a> {
    version: u8,
    operation: &'a PolicyAttempt,
    reviewed: &'a [u8; 32],
    boot_id: &'a str,
    secondary: Option<[u8; 32]>,
    replacement: &'a ManagerIdentity,
    continuity: Option<&'a ReceiptContinuity>,
}

struct RecoverableRestart<'a> {
    old_manager: &'a ManagerIdentity,
    secondary: [u8; 32],
    accepted: AcceptedRestart,
}

impl PolicyReceipt {
    fn decode(raw: &[u8]) -> Result<Self, PolicyResult> {
        if raw.len() > MAX_RECEIPT_BYTES {
            return Err(PolicyResult::PublicationUnknown);
        }
        let parsed: Self =
            serde_json::from_slice(raw).map_err(|_| PolicyResult::PublicationUnknown)?;
        match &parsed {
            Self::Published(receipt) if receipt.valid() => Ok(parsed),
            Self::Unchanged(receipt) if receipt.valid() => Ok(parsed),
            _ => Err(PolicyResult::PublicationUnknown),
        }
    }

    fn boot_id(&self) -> &str {
        match self {
            Self::Published(receipt) => &receipt.boot_id,
            Self::Unchanged(receipt) => &receipt.boot_id,
        }
    }

    fn live(&self) -> Option<LiveReceipt<'_>> {
        match self {
            Self::Published(receipt) => {
                let ReceiptPhase::RestartObserved { replacement } = &receipt.phase else {
                    return None;
                };
                Some(LiveReceipt {
                    version: receipt.version,
                    operation: &receipt.operation,
                    reviewed: &receipt.reviewed,
                    boot_id: &receipt.boot_id,
                    secondary: receipt.secondary_sources_sha256,
                    replacement,
                    continuity: receipt.continuity.as_ref(),
                })
            }
            Self::Unchanged(receipt) => {
                let UnchangedReceiptPhase::RestartObserved { replacement, .. } = &receipt.phase
                else {
                    return None;
                };
                Some(LiveReceipt {
                    version: receipt.version,
                    operation: &receipt.operation,
                    reviewed: &receipt.reviewed,
                    boot_id: &receipt.boot_id,
                    secondary: Some(receipt.secondary_sources_sha256),
                    replacement,
                    continuity: receipt.continuity.as_ref(),
                })
            }
        }
    }

    fn arm_restart(
        &self,
        intent_monotonic: u64,
        inputs_sha256: Option<[u8; 32]>,
    ) -> Result<Self, PolicyResult> {
        let intent = inputs_sha256.map(|inputs_sha256| RestartIntent {
            intent_monotonic,
            inputs_sha256,
        });
        match self {
            Self::Published(receipt) => {
                let phase = match intent {
                    Some(intent) => ReceiptPhase::RestartArmed { intent },
                    None => ReceiptPhase::RestartMayBeQueued {},
                };
                let mut next = receipt.advance(phase)?;
                if intent.is_some() {
                    next.version = 6;
                }
                Ok(Self::Published(next))
            }
            Self::Unchanged(receipt)
                if matches!(receipt.phase, UnchangedReceiptPhase::Prepared {}) =>
            {
                let phase = match intent {
                    Some(intent) => UnchangedReceiptPhase::RestartArmed { intent },
                    None => UnchangedReceiptPhase::RestartMayBeQueued { intent_monotonic },
                };
                Ok(Self::Unchanged(UnchangedReceipt {
                    version: if intent.is_some() { 6 } else { 4 },
                    phase,
                    ..receipt.clone()
                }))
            }
            _ => Err(PolicyResult::PublicationUnknown),
        }
    }

    fn acknowledge_restart(&self, job_id: NonZeroU32) -> Result<Self, PolicyResult> {
        match self {
            Self::Published(receipt) => {
                let ReceiptPhase::RestartArmed { intent } = receipt.phase else {
                    return Err(PolicyResult::PublicationUnknown);
                };
                let accepted = AcceptedRestart { intent, job_id };
                Ok(Self::Published(
                    receipt.advance(ReceiptPhase::RestartAccepted { accepted })?,
                ))
            }
            Self::Unchanged(receipt) => {
                let UnchangedReceiptPhase::RestartArmed { intent } = receipt.phase else {
                    return Err(PolicyResult::PublicationUnknown);
                };
                let accepted = AcceptedRestart { intent, job_id };
                Ok(Self::Unchanged(UnchangedReceipt {
                    phase: UnchangedReceiptPhase::RestartAccepted { accepted },
                    ..receipt.clone()
                }))
            }
        }
    }

    fn complete_restart(
        &self,
        replacement: ManagerIdentity,
        continuity: Option<ReceiptContinuity>,
    ) -> Result<Self, PolicyResult> {
        match self {
            Self::Published(receipt) => {
                if let Some(intent) = receipt.phase.intent() {
                    intent.verify_completion(&replacement, continuity.as_ref())?;
                }
                let mut completed =
                    receipt.advance(ReceiptPhase::RestartObserved { replacement })?;
                if continuity.is_some() {
                    completed.version = 5;
                    completed.continuity = continuity;
                }
                Ok(Self::Published(completed))
            }
            Self::Unchanged(receipt) => {
                let intent_monotonic = match receipt.phase {
                    UnchangedReceiptPhase::RestartMayBeQueued { intent_monotonic } => {
                        intent_monotonic
                    }
                    UnchangedReceiptPhase::RestartArmed { intent }
                    | UnchangedReceiptPhase::RestartAccepted {
                        accepted: AcceptedRestart { intent, .. },
                    } => {
                        intent.verify_completion(&replacement, continuity.as_ref())?;
                        intent.intent_monotonic
                    }
                    _ => return Err(PolicyResult::PublicationUnknown),
                };
                Ok(Self::Unchanged(UnchangedReceipt {
                    phase: UnchangedReceiptPhase::RestartObserved {
                        intent_monotonic,
                        replacement,
                    },
                    version: if continuity.is_some() { 5 } else { 4 },
                    continuity,
                    ..receipt.clone()
                }))
            }
        }
    }

    fn accepted_restart(
        &self,
        operation: &PolicyAttempt,
        request: ReviewedPolicyRequest,
    ) -> Result<RecoverableRestart<'_>, PolicyResult> {
        let (attempt, reviewed, old_manager, secondary, accepted) = match self {
            Self::Published(receipt) => {
                let ReceiptPhase::RestartAccepted { accepted } = receipt.phase else {
                    return Err(PolicyResult::PublicationUnknown);
                };
                (
                    &receipt.operation,
                    &receipt.reviewed,
                    &receipt.old_manager,
                    receipt.secondary_sources_sha256,
                    accepted,
                )
            }
            Self::Unchanged(receipt) => {
                let UnchangedReceiptPhase::RestartAccepted { accepted } = receipt.phase else {
                    return Err(PolicyResult::PublicationUnknown);
                };
                (
                    &receipt.operation,
                    &receipt.reviewed,
                    &receipt.old_manager,
                    Some(receipt.secondary_sources_sha256),
                    accepted,
                )
            }
        };
        if attempt != operation || reviewed != request.bytes() {
            return Err(PolicyResult::PublicationUnknown);
        }
        Ok(RecoverableRestart {
            old_manager,
            secondary: secondary.ok_or(PolicyResult::PublicationUnknown)?,
            accepted,
        })
    }

    fn verify_configuration(&self, stage: &File) -> Result<(), PolicyResult> {
        match self {
            Self::Published(receipt) => receipt.verify_published(),
            Self::Unchanged(receipt) => receipt.verify_unchanged(stage),
        }
    }
}

fn distinct_files(files: [&FileEvidence; 3]) -> bool {
    let [baseline, staged, leaf] = files.map(|file| &file.identity);
    baseline.device == staged.device
        && baseline.device == leaf.device
        && baseline.inode != staged.inode
        && baseline.inode != leaf.inode
        && staged.inode != leaf.inode
}

fn lowercase_hex(value: &str, width: usize) -> bool {
    value.len() == width
        && value
            .bytes()
            .all(|byte| matches!(byte, b'0'..=b'9' | b'a'..=b'f'))
}

fn valid_boot_id(value: &str) -> bool {
    let mut fields = value.split('-');
    [8, 4, 4, 4, 12].into_iter().all(|width| {
        fields
            .next()
            .is_some_and(|field| lowercase_hex(field, width))
    }) && fields.next().is_none()
}

fn boot_id() -> Result<String, PolicyResult> {
    let mut value = String::new();
    open(Path::new("/proc/sys/kernel/random/boot_id"))?
        .take(38)
        .read_to_string(&mut value)
        .map_err(|_| PolicyResult::PublicationUnknown)?;
    let value = value
        .strip_suffix('\n')
        .filter(|value| valid_boot_id(value))
        .ok_or(PolicyResult::PublicationUnknown)?;
    Ok(value.to_owned())
}

fn monotonic_micros() -> Result<u64, PolicyResult> {
    let worker = fs::metadata("/proc/thread-self/ns/time");
    let manager = fs::metadata("/proc/1/ns/time");
    match (worker, manager) {
        (Ok(worker), Ok(manager))
            if (worker.dev(), worker.ino()) == (manager.dev(), manager.ino()) => {}
        (Err(worker), Err(manager))
            if worker.kind() == io::ErrorKind::NotFound
                && manager.kind() == io::ErrorKind::NotFound => {}
        _ => return Err(PolicyResult::PublicationUnknown),
    }
    let mut time = libc::timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    if unsafe { libc::clock_gettime(libc::CLOCK_MONOTONIC, &mut time) } != 0 {
        return Err(PolicyResult::PublicationUnknown);
    }
    let seconds = u64::try_from(time.tv_sec).map_err(|_| PolicyResult::PublicationUnknown)?;
    let nanos = u64::try_from(time.tv_nsec).map_err(|_| PolicyResult::PublicationUnknown)?;
    seconds
        .checked_mul(1_000_000)
        .and_then(|micros| micros.checked_add(nanos / 1_000))
        .ok_or(PolicyResult::PublicationUnknown)
}

fn commitment<T: Serialize>(domain: &[u8], value: &T) -> Result<[u8; 32], PolicyResult> {
    let value = serde_json::to_vec(value).map_err(|_| PolicyResult::PublicationUnknown)?;
    let mut hash = Sha256::new();
    hash.update(domain);
    hash.update(value);
    Ok(hash.finalize().into())
}

pub(super) struct ReceiptWriter<'a> {
    parent: &'a File,
    stage: &'a File,
    current: PolicyReceipt,
    held: File,
}

impl<'a> ReceiptWriter<'a> {
    pub(super) fn prepare(
        parent: &'a File,
        stage: &'a File,
        operation: PolicyAttempt,
        request: ReviewedPolicyRequest,
        evidence: &Evidence<'_>,
        old: &ManagedSnapshot,
        secondary: Option<[u8; 32]>,
    ) -> Result<Self, PolicyResult> {
        let version = match (secondary, operation.state_lease) {
            (None, None) => 1,
            (Some(_), Some(_)) => 3,
            _ => return Err(PolicyResult::PublicationRefused),
        };
        let current = PolicyReceipt::Published(PublishedReceipt {
            version,
            operation,
            reviewed: *request.bytes(),
            baseline: FileEvidence::capture(&evidence.baseline.input)?,
            staged: FileEvidence::capture(evidence.source)?,
            leaf: FileEvidence::capture(evidence.leaf)?,
            include_graph_sha256: commitment(
                b"NEO-POLICY-INCLUDE-GRAPH-V1\0",
                &evidence.baseline.attachment,
            )?,
            secondary_sources_sha256: secondary,
            boot_id: boot_id()?,
            old_manager: ManagerIdentity::capture(old)?,
            phase: ReceiptPhase::Prepared {},
            continuity: None,
        });
        Self::persist_prepared(parent, stage, current)
    }

    pub(super) fn prepare_unchanged(
        parent: &'a File,
        stage: &'a File,
        operation: PolicyAttempt,
        request: ReviewedPolicyRequest,
        configuration: &ConfigurationObservation,
        old: &ManagedSnapshot,
        secondary: [u8; 32],
    ) -> Result<Self, PolicyResult> {
        let current = PolicyReceipt::Unchanged(UnchangedReceipt {
            version: 4,
            operation,
            reviewed: *request.bytes(),
            evidence: UnchangedEvidence::capture(configuration)?,
            secondary_sources_sha256: secondary,
            boot_id: boot_id()?,
            old_manager: ManagerIdentity::capture(old)?,
            phase: UnchangedReceiptPhase::Prepared {},
            continuity: None,
        });
        Self::persist_prepared(parent, stage, current)
    }

    pub(super) fn reopen_prepared_unchanged(
        parent: &'a File,
        stage: &'a File,
        operation: &PolicyAttempt,
        request: ReviewedPolicyRequest,
    ) -> Result<Self, PolicyResult> {
        check_stage(parent, stage)?;
        require_absent(&descriptor_path(stage, RECEIPT_TEMP))?;
        let held = open(&descriptor_path(stage, RECEIPT))?;
        let current = read_current(stage, &held)?;
        if !matches!(&current, PolicyReceipt::Unchanged(receipt) if receipt.matches_prepared(operation, request))
        {
            return Err(PolicyResult::PublicationUnknown);
        }
        Ok(Self {
            parent,
            stage,
            current,
            held,
        })
    }

    pub(super) fn verify_prepared_unchanged(
        &self,
        old: &ManagedSnapshot,
        secondary: [u8; 32],
    ) -> Result<(), PolicyResult> {
        let PolicyReceipt::Unchanged(receipt) = &self.current else {
            return Err(PolicyResult::PublicationUnknown);
        };
        if !matches!(receipt.phase, UnchangedReceiptPhase::Prepared {})
            || receipt.old_manager != ManagerIdentity::capture(old)?
            || receipt.secondary_sources_sha256 != secondary
        {
            return Err(PolicyResult::ChangedInputs);
        }
        self.verify_unchanged()
    }

    fn persist_prepared(
        parent: &'a File,
        stage: &'a File,
        current: PolicyReceipt,
    ) -> Result<Self, PolicyResult> {
        let held = publish(parent, stage, &current, None)?;
        parent
            .sync_all()
            .map_err(|_| PolicyResult::PublicationUnknown)?;
        Ok(Self {
            parent,
            stage,
            current,
            held,
        })
    }

    pub(super) fn queue_restart(
        &mut self,
        inputs_sha256: Option<[u8; 32]>,
    ) -> Result<(), PolicyResult> {
        if boot_id()? != self.current.boot_id() {
            return Err(PolicyResult::PublicationUnknown);
        }
        let intent_monotonic =
            if inputs_sha256.is_some() || matches!(&self.current, PolicyReceipt::Unchanged(_)) {
                monotonic_micros()?
            } else {
                0
            };
        self.advance(self.current.arm_restart(intent_monotonic, inputs_sha256)?)
    }

    pub(super) fn record_restart_job(&mut self, job_id: NonZeroU32) -> Result<(), PolicyResult> {
        if boot_id()? != self.current.boot_id() {
            return Err(PolicyResult::PublicationUnknown);
        }
        self.advance(self.current.acknowledge_restart(job_id)?)
    }

    pub(super) fn observe_restart(
        &mut self,
        replacement: &ManagedSnapshot,
        inputs_sha256: Option<[u8; 32]>,
    ) -> Result<(), PolicyResult> {
        if boot_id()? != self.current.boot_id() {
            return Err(PolicyResult::PublicationUnknown);
        }
        let replacement = ManagerIdentity::capture(replacement)?;
        let continuity = match inputs_sha256 {
            Some(inputs_sha256) => Some(ReceiptContinuity {
                inputs_sha256,
                verified_monotonic: monotonic_micros()?,
            }),
            None => None,
        };
        self.advance(self.current.complete_restart(replacement, continuity)?)
    }

    pub(super) fn verify_unchanged(&self) -> Result<(), PolicyResult> {
        let PolicyReceipt::Unchanged(receipt) = &self.current else {
            return Err(PolicyResult::PublicationUnknown);
        };
        self.verify_current()?;
        receipt.verify_unchanged(self.stage)
    }

    fn verify_current(&self) -> Result<(), PolicyResult> {
        check_stage(self.parent, self.stage)?;
        require_absent(&descriptor_path(self.stage, RECEIPT_TEMP))?;
        if boot_id()? != self.current.boot_id()
            || read_current(self.stage, &self.held)? != self.current
        {
            return Err(PolicyResult::PublicationUnknown);
        }
        Ok(())
    }

    fn advance(&mut self, next: PolicyReceipt) -> Result<(), PolicyResult> {
        let held = publish(
            self.parent,
            self.stage,
            &next,
            Some((&self.held, &self.current)),
        )?;
        self.held = held;
        self.current = next;
        Ok(())
    }
}

fn receipt_identity(file: &File) -> Result<fs::Metadata, PolicyResult> {
    let metadata = file
        .metadata()
        .map_err(|_| PolicyResult::PublicationUnknown)?;
    if metadata.is_file()
        && metadata.uid() == 0
        && metadata.gid() == 0
        && metadata.nlink() == 1
        && metadata.mode() & 0o7777 == 0o600
        && metadata.len() <= MAX_RECEIPT_BYTES as u64
    {
        return Ok(metadata);
    }
    Err(PolicyResult::PublicationUnknown)
}

fn same_named_file(stage: &File, name: &str, file: &File) -> Result<(), PolicyResult> {
    let named = fs::symlink_metadata(descriptor_path(stage, name))
        .map_err(|_| PolicyResult::PublicationUnknown)?;
    if InputIdentity::of(&named) != InputIdentity::of(&receipt_identity(file)?) {
        return Err(PolicyResult::PublicationUnknown);
    }
    Ok(())
}

fn read_current(stage: &File, file: &File) -> Result<PolicyReceipt, PolicyResult> {
    same_named_file(stage, RECEIPT, file)?;
    let mut reader = file;
    reader
        .seek(SeekFrom::Start(0))
        .map_err(|_| PolicyResult::PublicationUnknown)?;
    let mut raw = Vec::new();
    reader
        .take(MAX_RECEIPT_BYTES as u64 + 1)
        .read_to_end(&mut raw)
        .map_err(|_| PolicyResult::PublicationUnknown)?;
    same_named_file(stage, RECEIPT, file)?;
    PolicyReceipt::decode(&raw)
}

pub(super) fn require_absent(path: &Path) -> Result<(), PolicyResult> {
    match fs::symlink_metadata(path) {
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(()),
        _ => Err(PolicyResult::PublicationUnknown),
    }
}

fn check_stage(parent: &File, stage: &File) -> Result<(), PolicyResult> {
    anchored(parent, Path::new("/etc/nix"))?;
    anchored(stage, Path::new(STAGE))?;
    let stage_metadata = stage
        .metadata()
        .map_err(|_| PolicyResult::PublicationUnknown)?;
    let parent_metadata = parent
        .metadata()
        .map_err(|_| PolicyResult::PublicationUnknown)?;
    if stage_metadata.uid() != 0
        || stage_metadata.gid() != 0
        || stage_metadata.mode() & 0o7777 != 0o700
        || stage_metadata.dev() != parent_metadata.dev()
    {
        return Err(PolicyResult::PublicationUnknown);
    }
    Ok(())
}

pub(super) fn verify_completed(
    inputs: &PolicyInputs,
    parent: &File,
    stage: &File,
    operation: &PolicyAttempt,
    request: ReviewedPolicyRequest,
) -> Result<(), PolicyResult> {
    verify_live_receipt(
        inputs,
        parent,
        stage,
        |receipt| {
            matches!(
                (receipt.version, operation.state_lease),
                (1, None) | (3 | 4 | 5, Some(_))
            ) && receipt.operation == operation
                && receipt.reviewed == request.bytes()
        },
        |_| Ok(()),
    )
}

pub(super) fn recover_accepted_restart(
    inputs: &PolicyInputs,
    parent: &File,
    stage: &File,
    operation: &PolicyAttempt,
    request: ReviewedPolicyRequest,
    verify_caller: impl Fn(&PolicyInputs) -> Result<(), PolicyResult> + Sync,
) -> Result<(), PolicyResult> {
    check_stage(parent, stage)?;
    require_absent(&descriptor_path(stage, RECEIPT_TEMP))?;
    let held = open(&descriptor_path(stage, RECEIPT))?;
    let current = read_current(stage, &held)?;
    let mut writer = ReceiptWriter {
        parent,
        stage,
        current,
        held,
    };
    writer.verify_current()?;
    let RecoverableRestart {
        accepted,
        old_manager,
        secondary,
    } = writer.current.accepted_restart(operation, request)?;
    let candidate = inputs
        .managed_snapshot()?
        .ok_or(PolicyResult::PublicationUnknown)?;
    let generation = ManagerIdentity::capture(&candidate)?;
    if !generation.replaces(old_manager) || !accepted.intent.admits_replacement(&generation) {
        return Err(PolicyResult::PublicationUnknown);
    }
    let old_process = Path::new("/proc").join(old_manager.pid.to_string());
    let verify = |current: &PolicyInputs| {
        writer.verify_current()?;
        require_absent(&old_process)?;
        writer.current.verify_configuration(stage)?;
        continuity::capture(current)?.verify(accepted.intent.inputs_sha256)?;
        verify_caller(current)
    };
    let verify_job = || {
        managed::verify_final_restart(
            writer.current.boot_id(),
            accepted.job_id,
            &old_manager.invocation_id,
            accepted.intent.intent_monotonic,
            &candidate,
        )
    };
    verify_job()?;
    if inputs.inspect_managed_view(Some(secondary), &verify)? != candidate {
        return Err(PolicyResult::PublicationUnknown);
    }
    verify_job()?;
    if inputs.managed_snapshot()?.as_ref() != Some(&candidate) {
        return Err(PolicyResult::PublicationUnknown);
    }
    verify(inputs)?;
    writer.observe_restart(&candidate, Some(accepted.intent.inputs_sha256))?;
    writer.verify_current()
}

pub(super) fn verify_existing(
    inputs: &PolicyInputs,
    parent: &File,
    stage: &File,
    caller_uid: u32,
    lease: PolicyStateLease,
    stamp: [u8; 32],
) -> Result<(), PolicyResult> {
    verify_live_receipt(
        inputs,
        parent,
        stage,
        |receipt| {
            matches!(receipt.version, 3 | 4 | 5)
                && receipt.operation.caller_uid == caller_uid
                && receipt.operation.state_lease == Some(lease)
        },
        |observed| {
            let configuration = configuration_input(&inputs.source, read)
                .map_err(|_| PolicyResult::InaccessibleInputs)?;
            if replacement_input_stamp(
                &configuration,
                observed.pid,
                observed.started,
                observed.invocation,
                observed.socket,
            )? != stamp
            {
                return Err(PolicyResult::ChangedInputs);
            }
            Ok(())
        },
    )
}

fn verify_live_receipt(
    inputs: &PolicyInputs,
    parent: &File,
    stage: &File,
    matches_receipt: impl FnOnce(&LiveReceipt<'_>) -> bool,
    verify_current: impl Fn(&ManagedSnapshot) -> Result<(), PolicyResult> + Sync,
) -> Result<(), PolicyResult> {
    check_stage(parent, stage)?;
    require_absent(&descriptor_path(stage, RECEIPT_TEMP))?;
    let file = open(&descriptor_path(stage, RECEIPT))?;
    let receipt = read_current(stage, &file)?;
    let live = receipt.live().ok_or(PolicyResult::PublicationUnknown)?;
    if !matches_receipt(&live) {
        return Err(PolicyResult::PublicationUnknown);
    }
    let candidate = inputs
        .managed_snapshot()?
        .ok_or(PolicyResult::PublicationUnknown)?;
    let generation = ManagerIdentity::capture(&candidate)?;
    let continuity = if generation == *live.replacement {
        None
    } else {
        let proof = live.continuity.ok_or(PolicyResult::PublicationUnknown)?;
        if !generation.follows_proof(live.replacement, proof) {
            return Err(PolicyResult::PublicationUnknown);
        }
        Some(proof.inputs_sha256)
    };
    let verify = |current: &PolicyInputs| {
        check_stage(parent, stage)?;
        require_absent(&descriptor_path(stage, RECEIPT_TEMP))?;
        if boot_id()? != live.boot_id || read_current(stage, &file)? != receipt {
            return Err(PolicyResult::PublicationUnknown);
        }
        receipt.verify_configuration(stage)?;
        if let Some(expected) = continuity {
            continuity::capture(current)?.verify(expected)?;
        }
        verify_current(&candidate)
    };
    let observed = inputs.inspect_managed_view(live.secondary, &verify)?;
    if observed != candidate {
        return Err(PolicyResult::PublicationUnknown);
    }
    Ok(())
}

fn publish(
    parent: &File,
    stage: &File,
    next: &PolicyReceipt,
    previous: Option<(&File, &PolicyReceipt)>,
) -> Result<File, PolicyResult> {
    check_stage(parent, stage)?;
    let raw = serde_json::to_vec(next).map_err(|_| PolicyResult::PublicationUnknown)?;
    if &PolicyReceipt::decode(&raw)? != next {
        return Err(PolicyResult::PublicationUnknown);
    }
    let mut staged = OpenOptions::new()
        .read(true)
        .write(true)
        .create_new(true)
        .mode(0o600)
        .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC)
        .open(descriptor_path(stage, RECEIPT_TEMP))
        .map_err(|_| PolicyResult::PublicationUnknown)?;
    staged
        .set_permissions(fs::Permissions::from_mode(0o600))
        .and_then(|_| staged.write_all(&raw))
        .and_then(|_| staged.sync_all())
        .map_err(|_| PolicyResult::PublicationUnknown)?;
    check_stage(parent, stage)?;
    same_named_file(stage, RECEIPT_TEMP, &staged)?;
    if let Some((file, previous)) = previous {
        if &read_current(stage, file)? != previous {
            return Err(PolicyResult::PublicationUnknown);
        }
    }
    let old_path = CString::new(RECEIPT_TEMP).unwrap();
    let new_path = CString::new(RECEIPT).unwrap();
    let operation = if previous.is_some() {
        libc::RENAME_EXCHANGE
    } else {
        libc::RENAME_NOREPLACE
    };
    if unsafe {
        libc::syscall(
            libc::SYS_renameat2,
            stage.as_raw_fd(),
            old_path.as_ptr(),
            stage.as_raw_fd(),
            new_path.as_ptr(),
            operation,
        )
    } != 0
    {
        return Err(PolicyResult::PublicationUnknown);
    }
    check_stage(parent, stage)?;
    same_named_file(stage, RECEIPT, &staged)?;
    if let Some((file, _)) = previous {
        same_named_file(stage, RECEIPT_TEMP, file)?;
        fs::remove_file(descriptor_path(stage, RECEIPT_TEMP))
            .map_err(|_| PolicyResult::PublicationUnknown)?;
    }
    stage
        .sync_all()
        .map_err(|_| PolicyResult::PublicationUnknown)?;
    check_stage(parent, stage)?;
    same_named_file(stage, RECEIPT, &staged)?;
    Ok(staged)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn evidence(inode: u64, digest: char) -> FileEvidence {
        FileEvidence {
            identity: InputIdentity {
                device: 8,
                inode,
                mode: libc::S_IFREG | 0o644,
                uid: 0,
                gid: 0,
                size: 10,
                links: 1,
                modified: (1, 2),
                changed: (1, 3),
            },
            sha256: digest.to_string().repeat(64),
        }
    }

    fn receipt() -> PublishedReceipt {
        PublishedReceipt {
            version: 1,
            operation: PolicyAttempt {
                caller_uid: 1001,
                attempt_epoch: [1; 32],
                state_lease: None,
                helper: HelperIdentity {
                    path: "/nix/store/00000000000000000000000000000000-neo-policy-helper".into(),
                    sha256: "e".repeat(64),
                    size: 100,
                    device: 8,
                    inode: 22,
                },
            },
            reviewed: [2; 32],
            baseline: evidence(18, 'a'),
            staged: evidence(19, 'b'),
            leaf: evidence(20, 'c'),
            include_graph_sha256: [3; 32],
            secondary_sources_sha256: None,
            boot_id: "2a542360-24da-4f49-88c7-d404bca52d86".into(),
            old_manager: ManagerIdentity {
                pid: 120,
                started_monotonic: 100,
                invocation_id: "9".repeat(32),
                socket: (8, 21),
                listener: 123,
                snapshot_sha256: [4; 32],
            },
            phase: ReceiptPhase::Prepared {},
            continuity: None,
        }
    }

    fn decode_published(raw: &[u8]) -> Result<PublishedReceipt, PolicyResult> {
        match PolicyReceipt::decode(raw)? {
            PolicyReceipt::Published(receipt) => Ok(receipt),
            PolicyReceipt::Unchanged(_) => Err(PolicyResult::PublicationUnknown),
        }
    }

    #[test]
    fn receipt_refuses_unsupported_or_incoherent_data() {
        let prepared = receipt();
        let raw = serde_json::to_vec(&prepared).unwrap();
        assert_eq!(decode_published(&raw).unwrap(), prepared);
        assert!(PolicyReceipt::decode(&vec![b' '; MAX_RECEIPT_BYTES + 1]).is_err());

        for path in [
            &["unexpected"][..],
            &["phase", "unexpected"],
            &["staged", "identity", "unexpected"],
        ] {
            let mut value = serde_json::to_value(&prepared).unwrap();
            let mut field = &mut value;
            for index in path {
                field = &mut field[*index];
            }
            *field = 1.into();
            assert!(PolicyReceipt::decode(&serde_json::to_vec(&value).unwrap()).is_err());
        }

        let mut stock = prepared.clone();
        stock.version = 2;
        stock.secondary_sources_sha256 = Some([4; 32]);
        assert_eq!(
            decode_published(&serde_json::to_vec(&stock).unwrap()).unwrap(),
            stock
        );
        stock.version = 3;
        assert!(PolicyReceipt::decode(&serde_json::to_vec(&stock).unwrap()).is_err());
        stock.operation.state_lease = Some(PolicyStateLease {
            device: 8,
            inode: 23,
        });
        assert_eq!(
            decode_published(&serde_json::to_vec(&stock).unwrap()).unwrap(),
            stock
        );
        stock.operation.state_lease.as_mut().unwrap().inode = 0;
        assert!(PolicyReceipt::decode(&serde_json::to_vec(&stock).unwrap()).is_err());

        let mut invalid = prepared;
        invalid.version += 1;
        assert!(PolicyReceipt::decode(&serde_json::to_vec(&invalid).unwrap()).is_err());
        invalid.version -= 1;
        invalid.leaf.identity.inode = invalid.staged.identity.inode;
        assert!(PolicyReceipt::decode(&serde_json::to_vec(&invalid).unwrap()).is_err());
    }

    #[test]
    fn receipt_only_advances_to_a_verified_replacement() {
        let prepared = receipt();
        let mut replacement = prepared.old_manager.clone();
        replacement.pid += 1;
        replacement.started_monotonic += 1;
        replacement.invocation_id = "b".repeat(32);
        let observed = ReceiptPhase::RestartObserved { replacement };

        assert!(prepared.advance(observed.clone()).is_err());
        let queued = prepared
            .advance(ReceiptPhase::RestartMayBeQueued {})
            .unwrap();
        assert!(queued.advance(ReceiptPhase::Prepared {}).is_err());
        let old = queued.old_manager.clone();
        assert!(
            queued
                .advance(ReceiptPhase::RestartObserved { replacement: old })
                .is_err()
        );
        let terminal = queued.advance(observed).unwrap();
        assert_eq!(
            decode_published(&serde_json::to_vec(&terminal).unwrap()).unwrap(),
            terminal
        );
        assert!(
            terminal
                .advance(ReceiptPhase::RestartMayBeQueued {})
                .is_err()
        );
    }

    #[test]
    fn later_generation_requires_a_completed_continuity_proof() {
        let mut receipt = receipt();
        receipt.version = 3;
        receipt.secondary_sources_sha256 = Some([4; 32]);
        receipt.operation.state_lease = Some(PolicyStateLease {
            device: 8,
            inode: 23,
        });
        let mut replacement = receipt.old_manager.clone();
        replacement.pid += 1;
        replacement.invocation_id = "a".repeat(32);
        replacement.started_monotonic += 2;
        let proof = ReceiptContinuity {
            inputs_sha256: [5; 32],
            verified_monotonic: replacement.started_monotonic + 2,
        };
        let decode = |receipt: &PublishedReceipt| {
            PolicyReceipt::decode(&serde_json::to_vec(receipt).unwrap())
        };
        receipt.continuity = Some(proof.clone());
        assert!(decode(&receipt).is_err());
        receipt.version = 5;
        assert!(decode(&receipt).is_err());
        receipt.phase = ReceiptPhase::RestartObserved {
            replacement: replacement.clone(),
        };
        let decoded = decode(&receipt).unwrap();
        let live = decoded.live().unwrap();
        assert_eq!(live.continuity, Some(&proof));

        let mut candidate = replacement.clone();
        candidate.pid += 1;
        candidate.invocation_id = "b".repeat(32);
        candidate.started_monotonic = proof.verified_monotonic;
        assert!(!candidate.follows_proof(&replacement, &proof));
        candidate.started_monotonic += 1;
        assert!(candidate.follows_proof(&replacement, &proof));
        candidate.listener += 1;
        assert!(!candidate.follows_proof(&replacement, &proof));

        receipt.continuity.as_mut().unwrap().verified_monotonic = replacement.started_monotonic - 1;
        assert!(decode(&receipt).is_err());
        receipt.continuity = None;
        assert!(decode(&receipt).is_err());
        receipt.version = 3;
        assert!(
            decode(&receipt)
                .unwrap()
                .live()
                .unwrap()
                .continuity
                .is_none()
        );
        let mut raw = serde_json::to_value(receipt).unwrap();
        raw["continuity"] = serde_json::Value::Null;
        assert!(PolicyReceipt::decode(&serde_json::to_vec(&raw).unwrap()).is_err());
    }

    #[test]
    fn accepted_restart_keeps_the_original_scope_and_input_fence() {
        let mut published = receipt();
        published.version = 3;
        published.secondary_sources_sha256 = Some([4; 32]);
        published.operation.state_lease = Some(PolicyStateLease {
            device: 8,
            inode: 23,
        });
        let operation = published.operation.clone();
        let reviewed = ReviewedPolicyRequest::from_bytes(published.reviewed);
        let old = published.old_manager.clone();
        let unchanged = UnchangedReceipt {
            version: 4,
            operation: operation.clone(),
            reviewed: published.reviewed,
            evidence: UnchangedEvidence::AppliedUnchanged {
                configuration_sha256: [7; 32],
            },
            secondary_sources_sha256: [4; 32],
            boot_id: published.boot_id.clone(),
            old_manager: old.clone(),
            phase: UnchangedReceiptPhase::Prepared {},
            continuity: None,
        };
        let intent = old.started_monotonic + 1;
        let job = NonZeroU32::new(211).unwrap();
        let stamp = [5; 32];
        let decode =
            |receipt: &PolicyReceipt| PolicyReceipt::decode(&serde_json::to_vec(receipt).unwrap());
        let prepared = [
            (PolicyReceipt::Published(published), 3),
            (PolicyReceipt::Unchanged(unchanged), 4),
        ];
        for (prepared, legacy_version) in prepared {
            let legacy = prepared.arm_restart(intent, None).unwrap();
            assert!(decode(&legacy).unwrap().acknowledge_restart(job).is_err());
            let armed = prepared.arm_restart(intent, Some(stamp)).unwrap();
            assert!(
                decode(&armed)
                    .unwrap()
                    .accepted_restart(&operation, reviewed)
                    .is_err()
            );
            let invalid = prepared
                .arm_restart(old.started_monotonic, Some(stamp))
                .unwrap();
            assert!(decode(&invalid).is_err());
            let invalid = prepared.arm_restart(intent, Some([0; 32])).unwrap();
            assert!(decode(&invalid).is_err());

            let accepted = decode(&armed).unwrap().acknowledge_restart(job).unwrap();
            let accepted = decode(&accepted).unwrap();
            let recoverable = accepted.accepted_restart(&operation, reviewed).unwrap();
            assert_eq!(*recoverable.old_manager, old);
            assert_eq!(recoverable.accepted.job_id, job);
            assert_eq!(recoverable.accepted.intent.intent_monotonic, intent);
            assert_eq!(recoverable.accepted.intent.inputs_sha256, stamp);
            assert!(accepted.acknowledge_restart(job).is_err());
            assert!(accepted.arm_restart(intent, Some(stamp)).is_err());
            assert!(accepted.live().is_none());
            let mut other = operation.clone();
            other.state_lease.as_mut().unwrap().inode += 1;
            assert!(accepted.accepted_restart(&other, reviewed).is_err());
            assert!(
                accepted
                    .accepted_restart(&operation, ReviewedPolicyRequest::from_bytes([8; 32]))
                    .is_err()
            );

            let mut raw = serde_json::to_value(&accepted).unwrap();
            raw["phase"]["accepted"]["job_id"] = 0.into();
            assert!(PolicyReceipt::decode(&serde_json::to_vec(&raw).unwrap()).is_err());
            raw["phase"]["accepted"]["job_id"] = job.get().into();
            raw["version"] = legacy_version.into();
            assert!(PolicyReceipt::decode(&serde_json::to_vec(&raw).unwrap()).is_err());

            let mut replacement = old.clone();
            replacement.pid += 1;
            replacement.invocation_id = "b".repeat(32);
            replacement.started_monotonic = intent;
            let proof = ReceiptContinuity {
                inputs_sha256: stamp,
                verified_monotonic: intent + 2,
            };
            assert!(
                accepted
                    .complete_restart(replacement.clone(), Some(proof.clone()))
                    .is_err()
            );
            replacement.started_monotonic += 1;
            assert!(
                accepted
                    .complete_restart(replacement.clone(), None)
                    .is_err()
            );
            let wrong = ReceiptContinuity {
                inputs_sha256: [6; 32],
                ..proof.clone()
            };
            assert!(
                accepted
                    .complete_restart(replacement.clone(), Some(wrong))
                    .is_err()
            );
            let completed = accepted
                .complete_restart(replacement.clone(), Some(proof.clone()))
                .unwrap();
            let completed = decode(&completed).unwrap();
            let live = completed.live().unwrap();
            assert_eq!(live.version, 5);
            assert_eq!(live.replacement, &replacement);
            assert_eq!(live.operation, &operation);
            assert_eq!(live.reviewed, reviewed.bytes());
            assert_eq!(live.continuity, Some(&proof));
            assert!(completed.accepted_restart(&operation, reviewed).is_err());
            let mut raw = serde_json::to_value(&completed).unwrap();
            raw["version"] = 6.into();
            assert!(PolicyReceipt::decode(&serde_json::to_vec(&raw).unwrap()).is_err());
        }
    }

    #[test]
    fn unchanged_receipt_requires_its_own_evidence_lease_and_restart_fence() {
        let previous = receipt();
        let mut replacement = previous.old_manager.clone();
        replacement.pid += 1;
        replacement.started_monotonic += 2;
        replacement.invocation_id = "b".repeat(32);
        let prepared = UnchangedReceipt {
            version: 4,
            operation: PolicyAttempt {
                state_lease: Some(PolicyStateLease {
                    device: 8,
                    inode: 23,
                }),
                ..previous.operation
            },
            reviewed: previous.reviewed,
            evidence: UnchangedEvidence::AppliedUnchanged {
                configuration_sha256: [7; 32],
            },
            secondary_sources_sha256: [4; 32],
            boot_id: previous.boot_id,
            old_manager: previous.old_manager,
            phase: UnchangedReceiptPhase::Prepared {},
            continuity: None,
        };
        let raw = serde_json::to_vec(&prepared).unwrap();
        assert_eq!(
            PolicyReceipt::decode(&raw).unwrap(),
            PolicyReceipt::Unchanged(prepared.clone())
        );
        assert!(PolicyReceipt::decode(&raw).unwrap().live().is_none());
        let request = ReviewedPolicyRequest::from_bytes(prepared.reviewed);
        assert!(prepared.matches_prepared(&prepared.operation, request));
        assert!(!prepared.matches_prepared(
            &prepared.operation,
            ReviewedPolicyRequest::from_bytes([8; 32])
        ));
        let mut other_attempt = prepared.operation.clone();
        other_attempt.attempt_epoch = [9; 32];
        assert!(!prepared.matches_prepared(&other_attempt, request));
        let mut other_lease = prepared.operation.clone();
        other_lease.state_lease.as_mut().unwrap().inode += 1;
        assert!(!prepared.matches_prepared(&other_lease, request));

        let mut invalid = serde_json::to_value(&prepared).unwrap();
        invalid["baseline"] = serde_json::to_value(previous.baseline).unwrap();
        assert!(PolicyReceipt::decode(&serde_json::to_vec(&invalid).unwrap()).is_err());
        let mut invalid = prepared.clone();
        invalid.operation.state_lease = None;
        assert!(PolicyReceipt::decode(&serde_json::to_vec(&invalid).unwrap()).is_err());

        let intent_monotonic = prepared.old_manager.started_monotonic + 1;
        let queued = UnchangedReceipt {
            phase: UnchangedReceiptPhase::RestartMayBeQueued { intent_monotonic },
            ..prepared.clone()
        };
        assert!(
            PolicyReceipt::decode(&serde_json::to_vec(&queued).unwrap())
                .unwrap()
                .live()
                .is_none()
        );
        assert!(!queued.matches_prepared(&queued.operation, request));
        let completed = UnchangedReceipt {
            phase: UnchangedReceiptPhase::RestartObserved {
                intent_monotonic,
                replacement: replacement.clone(),
            },
            ..prepared
        };
        let observed = PolicyReceipt::decode(&serde_json::to_vec(&completed).unwrap()).unwrap();
        assert_eq!(observed.live().unwrap().replacement, &replacement);
        assert!(!completed.matches_prepared(&completed.operation, request));
        let mut invalid = completed;
        replacement.started_monotonic = intent_monotonic;
        invalid.phase = UnchangedReceiptPhase::RestartObserved {
            intent_monotonic,
            replacement,
        };
        assert!(PolicyReceipt::decode(&serde_json::to_vec(&invalid).unwrap()).is_err());
    }
}
