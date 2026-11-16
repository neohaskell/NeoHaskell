use fs2::FileExt;
use serde::{Deserialize, Serialize};
use std::fs::{self, File, Metadata, OpenOptions};
use std::io::{Read, Write};
use std::os::unix::fs::{MetadataExt, OpenOptionsExt, PermissionsExt, symlink};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

const STATE_SCHEMA: u32 = 1;
const MAX_STATE_BYTES: u64 = 64 * 1024;
const RECEIPT_NAME: &str = "receipt.json";
const INTENT_NAME: &str = "intent.json";
const LOCK_NAME: &str = "lock";
const ACTIVE_NAME: &str = "active";
const ACTIVE_NEXT_NAME: &str = "active.next";

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct PairIdentity {
    version: String,
    output: PathBuf,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct OwnedLinks {
    neo: PathBuf,
    neoctl: PathBuf,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct Receipt {
    schema: u32,
    installation_id: String,
    active: PairIdentity,
    previous: Option<PairIdentity>,
    links: OwnedLinks,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
struct Intent {
    schema: u32,
    operation: Operation,
    before: Option<Receipt>,
    after: Receipt,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum InstallSelection {
    LatestCompleteStable,
    ExactVersion,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Operation {
    Install(InstallSelection),
    Update(InstallSelection),
    Rollback,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum TransactionError {
    Io,
    Busy,
    UnsafePath,
    InvalidPreparedPair,
    InvalidState(&'static str),
    CommandLinkConflict,
    ForeignActiveTarget,
    NoInstallation,
    NoPreviousPair,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Layout {
    state_directory: PathBuf,
    command_directory: PathBuf,
}

impl Layout {
    pub(crate) fn new(state_directory: PathBuf, command_directory: PathBuf) -> Self {
        Self {
            state_directory,
            command_directory,
        }
    }

    fn active_link(&self) -> PathBuf {
        self.state_directory.join(ACTIVE_NAME)
    }

    fn active_next_link(&self) -> PathBuf {
        self.state_directory.join(ACTIVE_NEXT_NAME)
    }

    fn receipt(&self) -> PathBuf {
        self.state_directory.join(RECEIPT_NAME)
    }

    fn intent(&self) -> PathBuf {
        self.state_directory.join(INTENT_NAME)
    }

    fn lock(&self) -> PathBuf {
        self.state_directory.join(LOCK_NAME)
    }

    fn owned_links(&self) -> OwnedLinks {
        OwnedLinks {
            neo: self.command_directory.join("neo"),
            neoctl: self.command_directory.join("neoctl"),
        }
    }

    fn expected_command_target(&self, command: &str) -> PathBuf {
        self.active_link().join("bin").join(command)
    }

    fn validate(&self) -> Result<(), TransactionError> {
        validate_directory(&self.state_directory, true)?;
        validate_directory(&self.command_directory, false)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct PreparedPair {
    identity: PairIdentity,
}

impl PreparedPair {
    pub(crate) fn from_prepared_output(
        version: String,
        output: PathBuf,
    ) -> Result<Self, TransactionError> {
        let identity = PairIdentity { version, output };
        validate_pair(&identity)?;
        Ok(Self { identity })
    }
}

pub(crate) struct InstallationOwner {
    layout: Layout,
}

impl InstallationOwner {
    pub(crate) fn new(layout: Layout) -> Self {
        Self { layout }
    }

    pub(crate) fn install_prepared_pair(
        &self,
        pair: PreparedPair,
        operation: Operation,
    ) -> Result<(), TransactionError> {
        self.with_lock(|| {
            let current = self.recover_locked()?;
            match (&operation, current.as_ref()) {
                (Operation::Install(InstallSelection::LatestCompleteStable), Some(_)) => {
                    return Ok(());
                }
                (Operation::Update(_), None) => return Err(TransactionError::NoInstallation),
                (_, Some(receipt)) if receipt.active == pair.identity => return Ok(()),
                (Operation::Rollback, _) => {
                    return Err(TransactionError::InvalidState("rollback passed to install"));
                }
                _ => {}
            }

            let after = self.next_receipt(current.as_ref(), pair.identity);
            self.switch_pair(current, after, operation)
        })
    }

    pub(crate) fn rollback(&self) -> Result<(), TransactionError> {
        self.with_lock(|| {
            let current = self
                .recover_locked()?
                .ok_or(TransactionError::NoInstallation)?;
            let previous = current
                .previous
                .clone()
                .ok_or(TransactionError::NoPreviousPair)?;
            let after = Receipt {
                schema: STATE_SCHEMA,
                installation_id: current.installation_id.clone(),
                active: previous,
                previous: Some(current.active.clone()),
                links: current.links.clone(),
            };
            self.switch_pair(Some(current), after, Operation::Rollback)
        })
    }

    pub(crate) fn reconcile(&self) -> Result<(), TransactionError> {
        self.with_lock(|| self.recover_locked().map(|_| ()))
    }

    fn with_lock<T>(
        &self,
        action: impl FnOnce() -> Result<T, TransactionError>,
    ) -> Result<T, TransactionError> {
        self.layout.validate()?;
        let _lock = StateLock::acquire(&self.layout.lock())?;
        action()
    }

    fn next_receipt(&self, before: Option<&Receipt>, active: PairIdentity) -> Receipt {
        Receipt {
            schema: STATE_SCHEMA,
            installation_id: before
                .map(|receipt| receipt.installation_id.clone())
                .unwrap_or_else(new_installation_id),
            previous: before.map(|receipt| receipt.active.clone()),
            active,
            links: self.layout.owned_links(),
        }
    }

    fn switch_pair(
        &self,
        before: Option<Receipt>,
        after: Receipt,
        operation: Operation,
    ) -> Result<(), TransactionError> {
        self.validate_receipt(&after)?;
        self.validate_before_links(before.as_ref(), &after)?;
        let intent = Intent {
            schema: STATE_SCHEMA,
            operation,
            before: before.clone(),
            after: after.clone(),
        };
        self.validate_intent(&intent)?;
        self.write_json_atomic(&self.layout.intent(), &intent)?;
        #[cfg(test)]
        pause_after("after_intent");

        self.ensure_command_links(&after)?;
        if self.read_active_target()?
            != before.as_ref().map(|receipt| receipt.active.output.clone())
        {
            return Err(TransactionError::ForeignActiveTarget);
        }
        self.create_active_next(&after.active.output)?;
        fs::rename(self.layout.active_next_link(), self.layout.active_link())
            .map_err(|_| TransactionError::Io)?;
        sync_directory(&self.layout.state_directory)?;
        #[cfg(test)]
        pause_after("after_activation_rename");

        if self.read_active_target()?.as_ref() != Some(&after.active.output) {
            return Err(TransactionError::ForeignActiveTarget);
        }
        self.write_json_atomic(&self.layout.receipt(), &after)?;
        if self.read_active_target()?.as_ref() != Some(&after.active.output) {
            return Err(TransactionError::ForeignActiveTarget);
        }
        self.remove_intent()?;
        Ok(())
    }

    fn recover_locked(&self) -> Result<Option<Receipt>, TransactionError> {
        let receipt = self.read_receipt()?;
        let intent = self.read_intent()?;
        let active = self.read_active_target()?;
        let next = self.read_next_target()?;

        if let Some(intent) = intent {
            let before_target = intent.before.as_ref().map(|receipt| &receipt.active.output);
            let after_target = &intent.after.active.output;
            if active.as_ref() == Some(after_target) {
                self.remove_next_if(after_target, next.as_ref())?;
                self.ensure_command_links(&intent.after)?;
                if receipt.as_ref() != Some(&intent.after) {
                    self.write_json_atomic(&self.layout.receipt(), &intent.after)?;
                }
                if self.read_active_target()?.as_ref() != Some(after_target) {
                    return Err(TransactionError::ForeignActiveTarget);
                }
                self.remove_intent()?;
                return Ok(Some(intent.after));
            }
            if active.as_ref() == before_target {
                if receipt != intent.before {
                    return Err(TransactionError::InvalidState(
                        "receipt differs from journal before state",
                    ));
                }
                self.remove_next_if(after_target, next.as_ref())?;
                if let Some(before) = &intent.before {
                    self.ensure_command_links(before)?;
                } else {
                    self.remove_first_install_links(&intent.after)?;
                }
                self.remove_intent()?;
                return Ok(intent.before);
            }
            return Err(TransactionError::ForeignActiveTarget);
        }

        if next.is_some() {
            return Err(TransactionError::InvalidState(
                "activation temporary exists without journal",
            ));
        }
        match receipt {
            Some(receipt) => {
                if active.as_ref() != Some(&receipt.active.output) {
                    return Err(TransactionError::ForeignActiveTarget);
                }
                self.ensure_command_links(&receipt)?;
                Ok(Some(receipt))
            }
            None => {
                if active.is_some() {
                    return Err(TransactionError::ForeignActiveTarget);
                }
                self.require_absent_command_links()?;
                Ok(None)
            }
        }
    }

    fn validate_before_links(
        &self,
        before: Option<&Receipt>,
        after: &Receipt,
    ) -> Result<(), TransactionError> {
        match before {
            Some(receipt) => self.validate_command_links(&receipt.links),
            None => self.require_absent_links(&after.links),
        }
    }

    fn validate_command_links(&self, links: &OwnedLinks) -> Result<(), TransactionError> {
        if links != &self.layout.owned_links() {
            return Err(TransactionError::InvalidState("receipt command-link paths"));
        }
        self.validate_link(&links.neo, &self.layout.expected_command_target("neo"))?;
        self.validate_link(
            &links.neoctl,
            &self.layout.expected_command_target("neoctl"),
        )
    }

    fn validate_link(&self, path: &Path, target: &Path) -> Result<(), TransactionError> {
        match read_owned_link(path).map_err(|_| TransactionError::CommandLinkConflict)? {
            Some(actual) if actual == target => Ok(()),
            Some(_) => Err(TransactionError::CommandLinkConflict),
            None => Ok(()),
        }
    }

    fn require_absent_links(&self, links: &OwnedLinks) -> Result<(), TransactionError> {
        if links != &self.layout.owned_links() {
            return Err(TransactionError::InvalidState("journal command-link paths"));
        }
        self.require_absent_command_links()
    }

    fn require_absent_command_links(&self) -> Result<(), TransactionError> {
        for path in [
            self.layout.command_directory.join("neo"),
            self.layout.command_directory.join("neoctl"),
        ] {
            if path_kind(&path)?.is_some() {
                return Err(TransactionError::CommandLinkConflict);
            }
        }
        Ok(())
    }

    fn ensure_command_links(&self, receipt: &Receipt) -> Result<(), TransactionError> {
        self.validate_receipt(receipt)?;
        self.ensure_command_link(
            &receipt.links.neo,
            &self.layout.expected_command_target("neo"),
        )?;
        #[cfg(test)]
        pause_after("after_neo_link");
        self.ensure_command_link(
            &receipt.links.neoctl,
            &self.layout.expected_command_target("neoctl"),
        )
    }

    fn ensure_command_link(&self, path: &Path, target: &Path) -> Result<(), TransactionError> {
        match read_owned_link(path).map_err(|_| TransactionError::CommandLinkConflict)? {
            Some(actual) if actual == target => Ok(()),
            Some(_) => Err(TransactionError::CommandLinkConflict),
            None => {
                symlink(target, path).map_err(|_| TransactionError::CommandLinkConflict)?;
                sync_directory(&self.layout.command_directory)
            }
        }
    }

    fn remove_first_install_links(&self, receipt: &Receipt) -> Result<(), TransactionError> {
        self.remove_owned_link(
            &receipt.links.neo,
            &self.layout.expected_command_target("neo"),
        )?;
        self.remove_owned_link(
            &receipt.links.neoctl,
            &self.layout.expected_command_target("neoctl"),
        )
    }

    fn remove_owned_link(&self, path: &Path, target: &Path) -> Result<(), TransactionError> {
        match read_owned_link(path).map_err(|_| TransactionError::CommandLinkConflict)? {
            Some(actual) if actual == target => {
                fs::remove_file(path).map_err(|_| TransactionError::Io)?;
                sync_directory(self.layout.command_directory.as_path())
            }
            Some(_) => Err(TransactionError::CommandLinkConflict),
            None => Ok(()),
        }
    }

    fn create_active_next(&self, target: &Path) -> Result<(), TransactionError> {
        if path_kind(&self.layout.active_next_link())?.is_some() {
            return Err(TransactionError::InvalidState(
                "activation temporary already exists",
            ));
        }
        symlink(target, self.layout.active_next_link()).map_err(|_| TransactionError::Io)?;
        sync_directory(&self.layout.state_directory)?;
        #[cfg(test)]
        pause_after("after_active_next_creation");
        Ok(())
    }

    fn read_active_target(&self) -> Result<Option<PathBuf>, TransactionError> {
        read_owned_link(&self.layout.active_link())
            .map_err(|_| TransactionError::ForeignActiveTarget)
    }

    fn read_next_target(&self) -> Result<Option<PathBuf>, TransactionError> {
        read_owned_link(&self.layout.active_next_link())
            .map_err(|_| TransactionError::ForeignActiveTarget)
    }

    fn remove_next_if(
        &self,
        expected: &Path,
        actual: Option<&PathBuf>,
    ) -> Result<(), TransactionError> {
        match actual {
            Some(actual) if actual == expected => {
                fs::remove_file(self.layout.active_next_link())
                    .map_err(|_| TransactionError::Io)?;
                sync_directory(&self.layout.state_directory)
            }
            Some(_) => Err(TransactionError::ForeignActiveTarget),
            None => Ok(()),
        }
    }

    fn read_receipt(&self) -> Result<Option<Receipt>, TransactionError> {
        let receipt: Option<Receipt> = self.read_json(&self.layout.receipt())?;
        if let Some(receipt) = &receipt {
            self.validate_receipt(receipt)?;
        }
        Ok(receipt)
    }

    fn read_intent(&self) -> Result<Option<Intent>, TransactionError> {
        let intent: Option<Intent> = self.read_json(&self.layout.intent())?;
        if let Some(intent) = &intent {
            self.validate_intent(intent)?;
        }
        Ok(intent)
    }

    fn read_json<T: for<'de> Deserialize<'de>>(
        &self,
        path: &Path,
    ) -> Result<Option<T>, TransactionError> {
        let Some(mut file) = open_owned_file(path, true, false)? else {
            return Ok(None);
        };
        let mut bytes = Vec::new();
        Read::by_ref(&mut file)
            .take(MAX_STATE_BYTES + 1)
            .read_to_end(&mut bytes)
            .map_err(|_| TransactionError::Io)?;
        if bytes.len() as u64 > MAX_STATE_BYTES {
            return Err(TransactionError::InvalidState(
                "state record exceeds the size limit",
            ));
        }
        serde_json::from_slice(&bytes)
            .map(Some)
            .map_err(|_| TransactionError::InvalidState("state record does not match its schema"))
    }

    fn write_json_atomic<T: Serialize>(
        &self,
        path: &Path,
        value: &T,
    ) -> Result<(), TransactionError> {
        let bytes = serde_json::to_vec(value)
            .map_err(|_| TransactionError::InvalidState("could not encode state record"))?;
        if bytes.len() as u64 > MAX_STATE_BYTES {
            return Err(TransactionError::InvalidState(
                "state record exceeds the size limit",
            ));
        }
        let parent = path.parent().ok_or(TransactionError::UnsafePath)?;
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .ok_or(TransactionError::UnsafePath)?;
        let temporary = parent.join(format!(".{name}.tmp"));
        remove_owned_file_if_present(&temporary)?;
        let _existing_target = open_owned_file(path, false, false)?;
        let mut file = OpenOptions::new()
            .write(true)
            .create_new(true)
            .mode(0o600)
            .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC)
            .open(&temporary)
            .map_err(|_| TransactionError::Io)?;
        file.set_permissions(fs::Permissions::from_mode(0o600))
            .map_err(|_| TransactionError::Io)?;
        file.write_all(&bytes).map_err(|_| TransactionError::Io)?;
        file.sync_all().map_err(|_| TransactionError::Io)?;
        drop(file);

        #[cfg(test)]
        if path.file_name().is_some_and(|name| name == RECEIPT_NAME) {
            pause_after("before_receipt_rename");
        }

        fs::rename(&temporary, path).map_err(|_| TransactionError::Io)?;
        sync_directory(parent)?;

        #[cfg(test)]
        if path.file_name().is_some_and(|name| name == RECEIPT_NAME) {
            pause_after("after_receipt_rename");
        }
        Ok(())
    }

    fn remove_intent(&self) -> Result<(), TransactionError> {
        remove_owned_file_if_present(&self.layout.intent())
    }

    fn validate_receipt(&self, receipt: &Receipt) -> Result<(), TransactionError> {
        if receipt.schema != STATE_SCHEMA
            || receipt.installation_id.is_empty()
            || !receipt
                .installation_id
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() || byte == b'-')
            || receipt.links != self.layout.owned_links()
            || receipt
                .previous
                .as_ref()
                .is_some_and(|previous| previous == &receipt.active)
        {
            return Err(TransactionError::InvalidState(
                "receipt identity or ownership invariant",
            ));
        }
        validate_pair(&receipt.active)?;
        if let Some(previous) = &receipt.previous {
            validate_pair(previous)?;
        }
        Ok(())
    }

    fn validate_intent(&self, intent: &Intent) -> Result<(), TransactionError> {
        if intent.schema != STATE_SCHEMA {
            return Err(TransactionError::InvalidState("intent schema"));
        }
        self.validate_receipt(&intent.after)?;
        if let Some(before) = &intent.before {
            self.validate_receipt(before)?;
        }
        match (&intent.operation, &intent.before) {
            (Operation::Rollback, Some(before))
                if before.previous.as_ref() == Some(&intent.after.active)
                    && intent.after.previous.as_ref() == Some(&before.active)
                    && before.installation_id == intent.after.installation_id =>
            {
                Ok(())
            }
            (Operation::Rollback, _) => Err(TransactionError::InvalidState("rollback identities")),
            (Operation::Install(_) | Operation::Update(_), before)
                if intent.after.previous
                    == before.as_ref().map(|receipt| receipt.active.clone())
                    && before.as_ref().is_none_or(|receipt| {
                        receipt.installation_id == intent.after.installation_id
                    }) =>
            {
                Ok(())
            }
            _ => Err(TransactionError::InvalidState("install identities")),
        }
    }

    #[cfg(test)]
    fn hold_lock_for_test(&self) -> Result<StateLock, TransactionError> {
        self.layout.validate()?;
        StateLock::acquire(&self.layout.lock())
    }
}

struct StateLock {
    file: File,
}

impl StateLock {
    fn acquire(path: &Path) -> Result<Self, TransactionError> {
        let file = open_or_create_lock(path)?;
        FileExt::try_lock_exclusive(&file).map_err(|error| {
            if error.kind() == std::io::ErrorKind::WouldBlock {
                TransactionError::Busy
            } else {
                TransactionError::Io
            }
        })?;
        Ok(Self { file })
    }
}

impl Drop for StateLock {
    fn drop(&mut self) {
        let _ = FileExt::unlock(&self.file);
    }
}

fn open_or_create_lock(path: &Path) -> Result<File, TransactionError> {
    if let Some(file) = open_owned_file(path, true, true)? {
        return Ok(file);
    }
    match OpenOptions::new()
        .read(true)
        .write(true)
        .create_new(true)
        .mode(0o600)
        .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC)
        .open(path)
    {
        Ok(file) => {
            file.set_permissions(fs::Permissions::from_mode(0o600))
                .map_err(|_| TransactionError::Io)?;
            file.sync_all().map_err(|_| TransactionError::Io)?;
            sync_directory(path.parent().ok_or(TransactionError::UnsafePath)?)?;
            Ok(file)
        }
        Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {
            open_owned_file(path, true, true)?
                .ok_or(TransactionError::InvalidState("lock file disappeared"))
        }
        Err(_) => Err(TransactionError::Io),
    }
}

fn open_owned_file(path: &Path, read: bool, write: bool) -> Result<Option<File>, TransactionError> {
    let original = match fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(_) => return Err(TransactionError::Io),
    };
    validate_private_file(&original)?;
    let mut options = OpenOptions::new();
    options
        .read(read || !write)
        .write(write)
        .custom_flags(libc::O_NOFOLLOW | libc::O_CLOEXEC);
    let file = options
        .open(path)
        .map_err(|_| TransactionError::InvalidState("state file changed while opening"))?;
    let opened = file.metadata().map_err(|_| TransactionError::Io)?;
    validate_private_file(&opened)?;
    if original.dev() != opened.dev() || original.ino() != opened.ino() {
        return Err(TransactionError::InvalidState(
            "state file changed while opening",
        ));
    }
    Ok(Some(file))
}

fn validate_private_file(metadata: &Metadata) -> Result<(), TransactionError> {
    if !metadata.file_type().is_file()
        || metadata.uid() != unsafe { libc::geteuid() }
        || metadata.mode() & 0o777 != 0o600
    {
        return Err(TransactionError::UnsafePath);
    }
    Ok(())
}

fn remove_owned_file_if_present(path: &Path) -> Result<(), TransactionError> {
    if open_owned_file(path, false, false)?.is_some() {
        fs::remove_file(path).map_err(|_| TransactionError::Io)?;
        if let Some(parent) = path.parent() {
            sync_directory(parent)?;
        }
    }
    Ok(())
}

fn validate_directory(path: &Path, private: bool) -> Result<(), TransactionError> {
    if !path.is_absolute() {
        return Err(TransactionError::UnsafePath);
    }
    let metadata = fs::symlink_metadata(path).map_err(|_| TransactionError::UnsafePath)?;
    if !metadata.is_dir()
        || metadata.file_type().is_symlink()
        || metadata.uid() != unsafe { libc::geteuid() }
        || path
            .canonicalize()
            .map_err(|_| TransactionError::UnsafePath)?
            != path
    {
        return Err(TransactionError::UnsafePath);
    }
    let mode = metadata.mode() & 0o777;
    if (private && (mode & 0o077 != 0 || mode & 0o700 != 0o700)) || (!private && mode & 0o022 != 0)
    {
        return Err(TransactionError::UnsafePath);
    }
    Ok(())
}

fn validate_pair(pair: &PairIdentity) -> Result<(), TransactionError> {
    if semver::Version::parse(&pair.version).is_err()
        || !pair.output.is_absolute()
        || pair
            .output
            .canonicalize()
            .map_err(|_| TransactionError::InvalidPreparedPair)?
            != pair.output
    {
        return Err(TransactionError::InvalidPreparedPair);
    }
    let output =
        fs::symlink_metadata(&pair.output).map_err(|_| TransactionError::InvalidPreparedPair)?;
    if !output.is_dir() || output.file_type().is_symlink() || output.mode() & 0o222 != 0 {
        return Err(TransactionError::InvalidPreparedPair);
    }
    let bin = pair.output.join("bin");
    let bin_metadata =
        fs::symlink_metadata(&bin).map_err(|_| TransactionError::InvalidPreparedPair)?;
    if !bin_metadata.is_dir()
        || bin_metadata.file_type().is_symlink()
        || bin_metadata.mode() & 0o222 != 0
    {
        return Err(TransactionError::InvalidPreparedPair);
    }
    for command in ["neo", "neoctl"] {
        let metadata =
            fs::metadata(bin.join(command)).map_err(|_| TransactionError::InvalidPreparedPair)?;
        if !metadata.is_file() || metadata.mode() & 0o111 == 0 {
            return Err(TransactionError::InvalidPreparedPair);
        }
    }
    Ok(())
}

fn path_kind(path: &Path) -> Result<Option<Metadata>, TransactionError> {
    match fs::symlink_metadata(path) {
        Ok(metadata) => Ok(Some(metadata)),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(_) => Err(TransactionError::Io),
    }
}

fn read_owned_link(path: &Path) -> Result<Option<PathBuf>, TransactionError> {
    let Some(metadata) = path_kind(path)? else {
        return Ok(None);
    };
    if !metadata.file_type().is_symlink() || metadata.uid() != unsafe { libc::geteuid() } {
        return Err(TransactionError::UnsafePath);
    }
    let target = fs::read_link(path).map_err(|_| TransactionError::Io)?;
    if !target.is_absolute() {
        return Err(TransactionError::UnsafePath);
    }
    Ok(Some(target))
}

fn sync_directory(path: &Path) -> Result<(), TransactionError> {
    File::open(path)
        .and_then(|directory| directory.sync_all())
        .map_err(|_| TransactionError::Io)
}

fn new_installation_id() -> String {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_nanos())
        .unwrap_or_default();
    format!("{now:032x}-{:x}", std::process::id())
}

#[cfg(test)]
fn pause_after(point: &str) {
    if std::env::var("NEO_LIFECYCLE_TEST_PAUSE_AFTER").as_deref() != Ok(point) {
        return;
    }
    let marker = std::env::var_os("NEO_LIFECYCLE_TEST_PAUSE_MARKER")
        .expect("the child test process receives a pause marker path");
    fs::write(marker, point).expect("write the transaction cut-point marker");
    loop {
        std::thread::sleep(std::time::Duration::from_secs(3600));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::{PermissionsExt, symlink};
    use std::process::{Child, Command, ExitStatus, Output};
    use std::time::{Duration, Instant};
    use tempfile::TempDir;

    const REQUEST_ENV: &str = "NEO_LIFECYCLE_TEST_REQUEST";

    #[derive(Serialize, Deserialize)]
    #[serde(tag = "action", content = "parameters", rename_all = "snake_case")]
    enum ChildAction {
        Install {
            version: String,
            output: PathBuf,
            operation: Operation,
        },
        Rollback,
        Reconcile,
        HoldLock,
    }

    #[derive(Serialize, Deserialize)]
    struct ChildRequest {
        state_directory: PathBuf,
        command_directory: PathBuf,
        action: ChildAction,
    }

    #[test]
    fn child_transaction_driver() {
        let Some(request_path) = std::env::var_os(REQUEST_ENV) else {
            return;
        };
        let request: ChildRequest = serde_json::from_slice(
            &fs::read(request_path).expect("read the child transaction request"),
        )
        .expect("parse the child transaction request");
        let owner = InstallationOwner::new(Layout::new(
            request.state_directory,
            request.command_directory,
        ));
        let result = match request.action {
            ChildAction::Install {
                version,
                output,
                operation,
            } => PreparedPair::from_prepared_output(version, output)
                .and_then(|pair| owner.install_prepared_pair(pair, operation)),
            ChildAction::Rollback => owner.rollback(),
            ChildAction::Reconcile => owner.reconcile(),
            ChildAction::HoldLock => {
                let _lock = match owner.hold_lock_for_test() {
                    Ok(lock) => lock,
                    Err(error) => finish_child(error),
                };
                pause_after("lock_held");
                Ok(())
            }
        };
        match result {
            Ok(()) => std::process::exit(0),
            Err(error) => finish_child(error),
        }
    }

    #[test]
    fn child_process_activates_both_fixture_commands_reruns_updates_and_rolls_back() {
        let (temp, state, commands) = workspace();
        let pair_a = fixture_pair(&temp, "0.17.1");
        let _cleanup_a = FixtureCleanup(pair_a.clone());
        let pair_b = fixture_pair(&temp, "0.17.2");
        let _cleanup_b = FixtureCleanup(pair_b.clone());
        let unrelated = state.join("unowned.txt");
        fs::write(&unrelated, "preserve").expect("write unrelated state");

        assert_child_success(run_action(
            &temp,
            &state,
            &commands,
            ChildAction::Install {
                version: "0.17.1".to_owned(),
                output: pair_a.clone(),
                operation: Operation::Install(InstallSelection::LatestCompleteStable),
            },
        ));
        assert_versions(&commands, "0.17.1");
        let receipt_path = state.join(RECEIPT_NAME);
        let initial_receipt = fs::read(&receipt_path).expect("read the initial receipt");

        assert_child_success(run_action(
            &temp,
            &state,
            &commands,
            ChildAction::Install {
                version: "0.17.1".to_owned(),
                output: pair_a.clone(),
                operation: Operation::Install(InstallSelection::LatestCompleteStable),
            },
        ));
        assert_eq!(
            fs::read(&receipt_path).expect("read the unchanged receipt"),
            initial_receipt
        );

        assert_child_success(run_action(
            &temp,
            &state,
            &commands,
            ChildAction::Install {
                version: "0.17.2".to_owned(),
                output: pair_b.clone(),
                operation: Operation::Update(InstallSelection::LatestCompleteStable),
            },
        ));
        assert_versions(&commands, "0.17.2");
        let updated: Receipt =
            serde_json::from_slice(&fs::read(&receipt_path).expect("read the updated receipt"))
                .expect("parse the updated receipt");
        assert_eq!(updated.active.version, "0.17.2");
        assert_eq!(updated.previous.as_ref().unwrap().version, "0.17.1");

        assert_child_success(run_action(&temp, &state, &commands, ChildAction::Rollback));
        assert_versions(&commands, "0.17.1");
        let rolled_back: Receipt =
            serde_json::from_slice(&fs::read(&receipt_path).expect("read the rollback receipt"))
                .expect("parse the rollback receipt");
        assert_eq!(rolled_back.active.version, "0.17.1");
        assert_eq!(rolled_back.previous.as_ref().unwrap().version, "0.17.2");
        assert_eq!(fs::read_to_string(unrelated).unwrap(), "preserve");
    }

    #[test]
    fn recovery_keeps_the_before_pair_or_finishes_the_observed_activation() {
        for (cut, expected_version, receipt_version) in [
            ("after_intent", "0.17.1", "0.17.1"),
            ("after_activation_rename", "0.17.2", "0.17.1"),
            ("before_receipt_rename", "0.17.2", "0.17.1"),
            ("after_receipt_rename", "0.17.2", "0.17.2"),
        ] {
            let (temp, state, commands) = workspace();
            let pair_a = fixture_pair(&temp, "0.17.1");
            let _cleanup_a = FixtureCleanup(pair_a.clone());
            let pair_b = fixture_pair(&temp, "0.17.2");
            let _cleanup_b = FixtureCleanup(pair_b.clone());
            assert_child_success(run_action(
                &temp,
                &state,
                &commands,
                ChildAction::Install {
                    version: "0.17.1".to_owned(),
                    output: pair_a.clone(),
                    operation: Operation::Install(InstallSelection::LatestCompleteStable),
                },
            ));

            let marker = temp.path().join(format!("{cut}.reached"));
            let mut child = spawn_action(
                &temp,
                &state,
                &commands,
                ChildAction::Install {
                    version: "0.17.2".to_owned(),
                    output: pair_b.clone(),
                    operation: Operation::Update(InstallSelection::LatestCompleteStable),
                },
                Some((cut, &marker)),
            );
            wait_for_marker(&marker, &mut child);
            let killed = child.kill_and_reap();
            assert!(
                !killed.success(),
                "the interrupted child unexpectedly succeeded"
            );

            let before_recovery: Receipt = serde_json::from_slice(
                &fs::read(state.join(RECEIPT_NAME)).expect("read receipt before recovery"),
            )
            .expect("parse receipt before recovery");
            assert_eq!(before_recovery.active.version, receipt_version, "{cut}");
            assert!(
                state.join(INTENT_NAME).exists(),
                "{cut} lost the durable intent"
            );

            assert_child_success(run_action(&temp, &state, &commands, ChildAction::Reconcile));
            assert_versions(&commands, expected_version);
            let recovered: Receipt = serde_json::from_slice(
                &fs::read(state.join(RECEIPT_NAME)).expect("read recovered receipt"),
            )
            .expect("parse recovered receipt");
            assert_eq!(recovered.active.version, expected_version, "{cut}");
            if expected_version == "0.17.1" {
                assert!(
                    recovered.previous.is_none(),
                    "{cut} selected the pending pair"
                );
                assert_child_success(run_action(
                    &temp,
                    &state,
                    &commands,
                    ChildAction::Install {
                        version: "0.17.2".to_owned(),
                        output: pair_b.clone(),
                        operation: Operation::Install(InstallSelection::LatestCompleteStable),
                    },
                ));
                assert_versions(&commands, "0.17.1");
            } else {
                assert_eq!(recovered.previous.as_ref().unwrap().version, "0.17.1");
            }
            assert!(
                !state.join(INTENT_NAME).exists(),
                "{cut} left a resolved intent"
            );
            assert!(
                pair_a.exists() && pair_b.exists(),
                "{cut} removed a pair output"
            );
        }
    }

    #[test]
    fn first_install_recovery_cleans_partial_state_or_finishes_observed_activation() {
        for (cut, expected_link_count, activated) in [
            ("after_intent", 0, false),
            ("after_neo_link", 1, false),
            ("after_active_next_creation", 2, false),
            ("after_activation_rename", 2, true),
        ] {
            let (temp, state, commands) = workspace();
            let pair = fixture_pair(&temp, "0.17.1");
            let _cleanup = FixtureCleanup(pair.clone());
            let state_marker = state.join("unowned.txt");
            let command_marker = commands.join("unowned.txt");
            fs::write(&state_marker, "preserve").expect("write unrelated state");
            fs::write(&command_marker, "preserve").expect("write unrelated command entry");

            let marker = temp.path().join(format!("{cut}.reached"));
            let mut interrupted = spawn_action(
                &temp,
                &state,
                &commands,
                ChildAction::Install {
                    version: "0.17.1".to_owned(),
                    output: pair.clone(),
                    operation: Operation::Install(InstallSelection::LatestCompleteStable),
                },
                Some((cut, &marker)),
            );
            wait_for_marker(&marker, &mut interrupted);
            assert!(
                !interrupted.kill_and_reap().success(),
                "{cut} was not interrupted"
            );

            assert!(state.join(INTENT_NAME).exists(), "{cut} lost its intent");
            assert!(!state.join(RECEIPT_NAME).exists(), "{cut} committed early");
            assert_eq!(
                usize::from(path_is_present(&commands.join("neo")))
                    + usize::from(path_is_present(&commands.join("neoctl"))),
                expected_link_count,
                "{cut} command-link state"
            );
            assert_eq!(
                state.join(ACTIVE_NAME).exists(),
                activated,
                "{cut} active link"
            );
            assert_eq!(
                state.join(ACTIVE_NEXT_NAME).exists(),
                cut == "after_active_next_creation",
                "{cut} temporary activation link"
            );

            assert_child_success(run_action(&temp, &state, &commands, ChildAction::Reconcile));
            assert!(
                !state.join(INTENT_NAME).exists(),
                "{cut} kept its resolved intent"
            );
            assert!(
                !state.join(ACTIVE_NEXT_NAME).exists(),
                "{cut} kept its temporary link"
            );
            assert_eq!(fs::read_to_string(&state_marker).unwrap(), "preserve");
            assert_eq!(fs::read_to_string(&command_marker).unwrap(), "preserve");
            assert!(pair.exists(), "{cut} removed the prepared pair");

            if activated {
                assert_versions(&commands, "0.17.1");
                let recovered: Receipt = serde_json::from_slice(
                    &fs::read(state.join(RECEIPT_NAME)).expect("read recovered first receipt"),
                )
                .expect("parse recovered first receipt");
                assert_eq!(recovered.active.version, "0.17.1");
            } else {
                assert!(
                    !state.join(RECEIPT_NAME).exists(),
                    "{cut} invented a receipt"
                );
                assert!(
                    !state.join(ACTIVE_NAME).exists(),
                    "{cut} kept an inactive link"
                );
                assert!(
                    !path_is_present(&commands.join("neo")),
                    "{cut} kept the partial neo link"
                );
                assert!(
                    !path_is_present(&commands.join("neoctl")),
                    "{cut} kept the partial neoctl link"
                );
                assert_child_success(run_action(
                    &temp,
                    &state,
                    &commands,
                    ChildAction::Install {
                        version: "0.17.1".to_owned(),
                        output: pair.clone(),
                        operation: Operation::Install(InstallSelection::LatestCompleteStable),
                    },
                ));
                assert_versions(&commands, "0.17.1");
            }
        }
    }

    #[test]
    fn another_process_refuses_the_stable_lock_without_changing_the_pair() {
        let (temp, state, commands) = workspace();
        let pair_a = fixture_pair(&temp, "0.17.1");
        let _cleanup_a = FixtureCleanup(pair_a.clone());
        let pair_b = fixture_pair(&temp, "0.17.2");
        let _cleanup_b = FixtureCleanup(pair_b.clone());
        assert_child_success(run_action(
            &temp,
            &state,
            &commands,
            ChildAction::Install {
                version: "0.17.1".to_owned(),
                output: pair_a,
                operation: Operation::Install(InstallSelection::LatestCompleteStable),
            },
        ));
        let lock_path = state.join(LOCK_NAME);
        let lock_inode = fs::metadata(&lock_path).unwrap().ino();
        let marker = temp.path().join("lock-held.reached");
        let mut holder = spawn_action(
            &temp,
            &state,
            &commands,
            ChildAction::HoldLock,
            Some(("lock_held", &marker)),
        );
        wait_for_marker(&marker, &mut holder);

        let receipt_before = fs::read(state.join(RECEIPT_NAME)).unwrap();
        let refused = run_action(
            &temp,
            &state,
            &commands,
            ChildAction::Install {
                version: "0.17.2".to_owned(),
                output: pair_b,
                operation: Operation::Update(InstallSelection::LatestCompleteStable),
            },
        );
        assert_eq!(
            refused.status.code(),
            Some(73),
            "second transaction did not report busy"
        );
        assert_versions(&commands, "0.17.1");
        assert_eq!(fs::read(state.join(RECEIPT_NAME)).unwrap(), receipt_before);
        assert_eq!(fs::metadata(&lock_path).unwrap().ino(), lock_inode);

        holder.kill_and_reap();
        assert_child_success(run_action(&temp, &state, &commands, ChildAction::Reconcile));
        assert!(lock_path.exists(), "the stable lock file was unlinked");
        assert_eq!(fs::metadata(&lock_path).unwrap().ino(), lock_inode);
    }

    #[test]
    fn changed_command_link_refuses_update_and_preserves_the_foreign_link() {
        let (temp, state, commands) = workspace();
        let pair_a = fixture_pair(&temp, "0.17.1");
        let _cleanup_a = FixtureCleanup(pair_a.clone());
        let pair_b = fixture_pair(&temp, "0.17.2");
        let _cleanup_b = FixtureCleanup(pair_b.clone());
        assert_child_success(run_action(
            &temp,
            &state,
            &commands,
            ChildAction::Install {
                version: "0.17.1".to_owned(),
                output: pair_a,
                operation: Operation::Install(InstallSelection::LatestCompleteStable),
            },
        ));
        let foreign = temp.path().join("foreign-neo");
        let shell = fixture_shell();
        write_executable(
            &foreign,
            &format!("#!{}\nprintf 'foreign\\n'\n", shell.display()),
        );
        fs::remove_file(commands.join("neo")).expect("remove the owned test link");
        symlink(&foreign, commands.join("neo")).expect("install a foreign test link");
        let receipt_before = fs::read(state.join(RECEIPT_NAME)).unwrap();
        let active_before = fs::read_link(state.join(ACTIVE_NAME)).unwrap();

        let refused = run_action(
            &temp,
            &state,
            &commands,
            ChildAction::Install {
                version: "0.17.2".to_owned(),
                output: pair_b,
                operation: Operation::Update(InstallSelection::LatestCompleteStable),
            },
        );
        assert_eq!(
            refused.status.code(),
            Some(74),
            "foreign command link was accepted"
        );
        assert_eq!(fs::read_link(commands.join("neo")).unwrap(), foreign);
        assert_eq!(
            fs::read_link(state.join(ACTIVE_NAME)).unwrap(),
            active_before
        );
        assert_eq!(fs::read(state.join(RECEIPT_NAME)).unwrap(), receipt_before);
        assert_eq!(command_version(&commands.join("neoctl")), "neoctl 0.17.1");
    }

    #[test]
    fn foreign_third_activation_target_stops_recovery_without_deleting_it() {
        let (temp, state, commands) = workspace();
        let pair_a = fixture_pair(&temp, "0.17.1");
        let _cleanup_a = FixtureCleanup(pair_a.clone());
        let pair_b = fixture_pair(&temp, "0.17.2");
        let _cleanup_b = FixtureCleanup(pair_b.clone());
        let pair_c = fixture_pair(&temp, "0.17.3");
        let _cleanup_c = FixtureCleanup(pair_c.clone());
        assert_child_success(run_action(
            &temp,
            &state,
            &commands,
            ChildAction::Install {
                version: "0.17.1".to_owned(),
                output: pair_a.clone(),
                operation: Operation::Install(InstallSelection::LatestCompleteStable),
            },
        ));
        let marker = temp.path().join("intent-before-foreign-target.reached");
        let mut interrupted = spawn_action(
            &temp,
            &state,
            &commands,
            ChildAction::Install {
                version: "0.17.2".to_owned(),
                output: pair_b.clone(),
                operation: Operation::Update(InstallSelection::LatestCompleteStable),
            },
            Some(("after_intent", &marker)),
        );
        wait_for_marker(&marker, &mut interrupted);
        interrupted.kill_and_reap();
        fs::remove_file(state.join(ACTIVE_NAME)).expect("replace the active link in the fixture");
        symlink(&pair_c, state.join(ACTIVE_NAME)).expect("point activation at a third fixture");
        let receipt_before = fs::read(state.join(RECEIPT_NAME)).unwrap();
        let intent_before = fs::read(state.join(INTENT_NAME)).unwrap();

        let refused = run_action(&temp, &state, &commands, ChildAction::Reconcile);
        assert_eq!(
            refused.status.code(),
            Some(75),
            "a third target was accepted"
        );
        assert_eq!(fs::read_link(state.join(ACTIVE_NAME)).unwrap(), pair_c);
        assert_eq!(fs::read(state.join(RECEIPT_NAME)).unwrap(), receipt_before);
        assert_eq!(fs::read(state.join(INTENT_NAME)).unwrap(), intent_before);
        assert!(pair_a.exists() && pair_b.exists() && pair_c.exists());
        assert_eq!(command_version(&commands.join("neo")), "neo 0.17.3");
    }

    fn workspace() -> (TempDir, PathBuf, PathBuf) {
        let temp = tempfile::tempdir().expect("create disposable lifecycle state");
        let root = temp
            .path()
            .canonicalize()
            .expect("canonicalize fixture root");
        let state = root.join("state");
        let commands = root.join("bin");
        fs::create_dir(&state).expect("create state directory");
        fs::create_dir(&commands).expect("create command directory");
        set_mode(&state, 0o700);
        set_mode(&commands, 0o700);
        (temp, state, commands)
    }

    fn path_is_present(path: &Path) -> bool {
        fs::symlink_metadata(path).is_ok()
    }

    fn fixture_pair(temp: &TempDir, version: &str) -> PathBuf {
        let root = temp
            .path()
            .canonicalize()
            .expect("canonicalize fixture root");
        let output = root.join(format!("pair-{version}"));
        let bin = output.join("bin");
        let shell = fixture_shell();
        fs::create_dir(&output).expect("create immutable pair fixture");
        fs::create_dir(&bin).expect("create pair command directory");
        write_executable(
            &bin.join("neo"),
            &format!("#!{}\nprintf 'neo {version}\\n'\n", shell.display()),
        );
        write_executable(
            &bin.join("neoctl"),
            &format!("#!{}\nprintf 'neoctl {version}\\n'\n", shell.display()),
        );
        set_mode(&bin, 0o555);
        set_mode(&output, 0o555);
        output
    }

    fn write_executable(path: &Path, contents: &str) {
        fs::write(path, contents).expect("write fixture executable");
        set_mode(path, 0o555);
    }

    fn fixture_shell() -> PathBuf {
        std::env::split_paths(&std::env::var_os("PATH").expect("test PATH is set"))
            .flat_map(|directory| [directory.join("sh"), directory.join("bash")])
            .find_map(|candidate| {
                let metadata = fs::metadata(&candidate).ok()?;
                (metadata.is_file() && metadata.mode() & 0o111 != 0)
                    .then(|| candidate.canonicalize().ok())
                    .flatten()
            })
            .expect("test PATH contains a POSIX shell")
    }

    fn set_mode(path: &Path, mode: u32) {
        let mut permissions = fs::metadata(path)
            .expect("inspect fixture permissions")
            .permissions();
        permissions.set_mode(mode);
        fs::set_permissions(path, permissions).expect("set fixture permissions");
    }

    fn run_action(temp: &TempDir, state: &Path, commands: &Path, action: ChildAction) -> Output {
        spawn_action(temp, state, commands, action, None).wait_with_output()
    }

    fn spawn_action(
        temp: &TempDir,
        state: &Path,
        commands: &Path,
        action: ChildAction,
        pause: Option<(&str, &Path)>,
    ) -> OwnedChild {
        let request_path = temp.path().join("request.json");
        let request = ChildRequest {
            state_directory: state.to_path_buf(),
            command_directory: commands.to_path_buf(),
            action,
        };
        fs::write(
            &request_path,
            serde_json::to_vec(&request).expect("encode child transaction request"),
        )
        .expect("write child transaction request");
        let mut command =
            Command::new(std::env::current_exe().expect("locate the test executable"));
        command
            .args([
                "--exact",
                "installation::tests::child_transaction_driver",
                "--nocapture",
            ])
            .env(REQUEST_ENV, request_path)
            .env_remove("NEO_LIFECYCLE_TEST_PAUSE_AFTER")
            .env_remove("NEO_LIFECYCLE_TEST_PAUSE_MARKER");
        if let Some((point, marker)) = pause {
            command
                .env("NEO_LIFECYCLE_TEST_PAUSE_AFTER", point)
                .env("NEO_LIFECYCLE_TEST_PAUSE_MARKER", marker);
        }
        OwnedChild::spawn(&mut command)
    }

    struct OwnedChild {
        child: Option<Child>,
    }

    impl OwnedChild {
        fn spawn(command: &mut Command) -> Self {
            Self {
                child: Some(command.spawn().expect("start transaction child")),
            }
        }

        fn try_wait(&mut self) -> std::io::Result<Option<ExitStatus>> {
            self.child
                .as_mut()
                .expect("owned child is available")
                .try_wait()
        }

        fn kill_and_reap(&mut self) -> ExitStatus {
            let mut child = self.child.take().expect("owned child is available");
            if !matches!(child.try_wait(), Ok(Some(_))) {
                let _ = child.kill();
            }
            child.wait().expect("reap owned child")
        }

        fn wait_with_output(&mut self) -> Output {
            let deadline = Instant::now() + Duration::from_secs(30);
            let mut delay = Duration::from_millis(1);
            loop {
                if self
                    .try_wait()
                    .expect("inspect transaction child")
                    .is_some()
                {
                    break;
                }
                let remaining = deadline.saturating_duration_since(Instant::now());
                if remaining.is_zero() {
                    let status = self.kill_and_reap();
                    panic!("transaction child exceeded its 30 second deadline: {status:?}");
                }
                std::thread::sleep(delay.min(remaining));
                delay = delay.saturating_mul(2);
            }
            self.child
                .take()
                .expect("owned child is available")
                .wait_with_output()
                .expect("read transaction child output")
        }
    }

    impl Drop for OwnedChild {
        fn drop(&mut self) {
            let Some(mut child) = self.child.take() else {
                return;
            };
            if !matches!(child.try_wait(), Ok(Some(_))) {
                let _ = child.kill();
            }
            let _ = child.wait();
        }
    }

    fn wait_for_marker(marker: &Path, child: &mut OwnedChild) {
        let deadline = Instant::now() + Duration::from_secs(10);
        let mut delay = Duration::from_millis(1);
        loop {
            if marker.exists() {
                return;
            }
            if let Some(status) = child.try_wait().expect("inspect transaction child") {
                panic!("child exited before its cut point with {status:?}");
            }
            if Instant::now() >= deadline {
                child.kill_and_reap();
                panic!("child did not reach the requested transaction cut point");
            }
            let remaining = deadline.saturating_duration_since(Instant::now());
            std::thread::sleep(delay.min(remaining));
            delay = delay.saturating_mul(2);
        }
    }

    fn assert_child_success(output: Output) {
        assert!(
            output.status.success(),
            "transaction child failed with {:?}\n{}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fn assert_versions(commands: &Path, version: &str) {
        assert_eq!(
            command_version(&commands.join("neo")),
            format!("neo {version}")
        );
        assert_eq!(
            command_version(&commands.join("neoctl")),
            format!("neoctl {version}")
        );
    }

    fn command_version(path: &Path) -> String {
        let output = Command::new(path)
            .arg("--version")
            .output()
            .expect("run the active fixture command");
        assert!(output.status.success(), "fixture command failed");
        String::from_utf8(output.stdout)
            .expect("fixture command writes UTF-8")
            .trim()
            .to_owned()
    }

    fn finish_child(error: TransactionError) -> ! {
        eprintln!("{error:?}");
        let code = match error {
            TransactionError::Busy => 73,
            TransactionError::CommandLinkConflict => 74,
            TransactionError::ForeignActiveTarget => 75,
            _ => 76,
        };
        std::process::exit(code);
    }

    struct FixtureCleanup(PathBuf);

    impl Drop for FixtureCleanup {
        fn drop(&mut self) {
            set_mode(&self.0.join("bin"), 0o700);
            set_mode(&self.0, 0o700);
        }
    }
}
