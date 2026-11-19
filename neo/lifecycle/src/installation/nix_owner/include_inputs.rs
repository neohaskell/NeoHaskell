use super::*;
use base64::{Engine, engine::general_purpose::STANDARD};
#[cfg(target_os = "linux")]
use caller_access::{CallerAccess, DaemonAccessPolicy};
use std::path::Component;

const MAX_EDGES: usize = 128;
const MAX_DEPTH: usize = 32;
const MAX_BYTES: usize = 1024 * 1024;

#[derive(PartialEq, Eq, Serialize)]
pub(super) enum AttachmentInputs {
    Unresolved,
    Conflict,
    VerifiedAppend(Graph),
    VerifiedExisting(Graph),
}

#[derive(PartialEq, Eq, Serialize)]
pub(super) struct Graph {
    companion: ConfigurationFile,
    edges: Vec<Edge>,
    #[cfg(target_os = "linux")]
    #[serde(skip)]
    access: DaemonAccessPolicy,
}

#[cfg(target_os = "linux")]
impl AttachmentInputs {
    pub(super) fn access(&self, caller: StockCaller) -> CallerAccess {
        match self {
            Self::VerifiedAppend(graph) | Self::VerifiedExisting(graph) => {
                graph.access.access(caller)
            }
            Self::Unresolved | Self::Conflict => CallerAccess::Unproved,
        }
    }
}

#[derive(PartialEq, Eq, Serialize)]
struct Edge {
    containing: PathBuf,
    path: PathBuf,
    optional: bool,
    input: ConfigurationFile,
}

impl std::fmt::Debug for AttachmentInputs {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::Unresolved => "UnresolvedAttachmentInputs",
            Self::Conflict => "ConflictingAttachmentInputs",
            Self::VerifiedAppend(_) => "VerifiedAppendInputs",
            Self::VerifiedExisting(_) => "VerifiedExistingInputs",
        })
    }
}

fn normalize(path: &Path) -> Option<PathBuf> {
    if !path.is_absolute() {
        return None;
    }
    let mut result = PathBuf::new();
    for component in path.components() {
        match component {
            Component::ParentDir => {
                result.pop();
            }
            Component::CurDir => {}
            component => result.push(component.as_os_str()),
        }
    }
    Some(result)
}

fn location(path: &Path, input: &ConfigurationFile) -> Option<PathBuf> {
    match input {
        ConfigurationFile::Present { resolved, .. } => Some(resolved.clone()),
        ConfigurationFile::Missing {
            ancestors,
            resolved_parent,
            ..
        } => {
            let existing = path
                .ancestors()
                .zip(ancestors)
                .find_map(|(path, identity)| identity.as_ref().map(|_| path))?;
            Some(resolved_parent.join(path.strip_prefix(existing).ok()?))
        }
    }
}

fn same_file(left: &ConfigurationFile, right: &ConfigurationFile) -> bool {
    matches!((left, right),
        (ConfigurationFile::Present { file: a, .. }, ConfigurationFile::Present { file: b, .. })
        if a.device == b.device && a.inode == b.inode)
}

pub(super) struct EffectivePolicy {
    requires_signatures: bool,
    features: Vec<String>,
    substituters: Vec<String>,
    trusted_substituters: Vec<String>,
    public_keys: Vec<String>,
    loads_plugins: bool,
    #[cfg(target_os = "linux")]
    access: DaemonAccessPolicy,
}

impl Default for EffectivePolicy {
    fn default() -> Self {
        Self {
            requires_signatures: true,
            features: Vec::new(),
            substituters: vec!["https://cache.nixos.org/".into()],
            trusted_substituters: Vec::new(),
            public_keys: vec![
                "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=".into(),
            ],
            loads_plugins: false,
            #[cfg(target_os = "linux")]
            access: DaemonAccessPolicy::default(),
        }
    }
}

fn unsupported_policy() -> TransactionError {
    TransactionError::NixBlocked(NixBlock::ConfigurationOwnerUnknown)
}

fn parse_public_key(entry: &str) -> Option<(&str, [u8; 32])> {
    let (name, encoded) = entry.split_once(':')?;
    if name.is_empty() {
        return None;
    }
    let mut decoded = [0; 32];
    let len = STANDARD.decode_slice(encoded, &mut decoded).ok()?;
    (len == decoded.len()).then_some((name, decoded))
}

impl EffectivePolicy {
    pub(super) fn assign(&mut self, name: &str, values: &[&str]) -> Result<(), TransactionError> {
        match name {
            "require-sigs" => {
                self.requires_signatures = match values {
                    ["true" | "yes" | "1"] => true,
                    ["false" | "no" | "0"] => false,
                    _ => return Err(TransactionError::Io),
                };
                return Ok(());
            }
            "store" if !matches!(values, [] | ["auto" | "local"]) => {
                return Err(unsupported_policy());
            }
            "plugin-files" => {
                self.loads_plugins = !values.is_empty();
                return Ok(());
            }
            #[cfg(target_os = "linux")]
            "build-users-group" => {
                self.access.build_users_group = values.join(" ");
                return Ok(());
            }
            _ => {}
        }

        let (setting, append) = match name.strip_prefix("extra-") {
            Some(setting) => (setting, true),
            None => (name, false),
        };
        let target = match setting {
            "experimental-features" => &mut self.features,
            "substituters" | "binary-caches" => &mut self.substituters,
            "trusted-substituters" | "trusted-binary-caches" => &mut self.trusted_substituters,
            "trusted-public-keys" | "binary-cache-public-keys" => &mut self.public_keys,
            #[cfg(target_os = "linux")]
            "allowed-users" => &mut self.access.allowed_users,
            #[cfg(target_os = "linux")]
            "trusted-users" => &mut self.access.trusted_users,
            _ => return Ok(()),
        };
        if !append {
            target.clear();
        }
        target.extend(values.iter().map(|value| (*value).to_owned()));
        Ok(())
    }

    fn verify(&self) -> Result<(), TransactionError> {
        if !["nix-command", "flakes"]
            .into_iter()
            .all(|feature| self.features.iter().any(|configured| configured == feature))
            || !self.required_cache_policy()?
        {
            return Err(unsupported_policy());
        }
        Ok(())
    }

    pub(super) fn required_cache_policy(&self) -> Result<bool, TransactionError> {
        let (urls, keys) =
            crate::candidate::nix_cache_settings().map_err(|_| TransactionError::Io)?;
        if !self.requires_signatures || self.loads_plugins {
            return Err(unsupported_policy());
        }
        let mut complete = true;
        for url in urls.split_ascii_whitespace() {
            // The daemon compares the requested StoreReference including its parameters.
            if !self
                .substituters
                .iter()
                .chain(&self.trusted_substituters)
                .any(|configured| configured == url || configured.strip_suffix('/') == Some(url))
            {
                complete = false;
            }
        }
        let mut trusted_keys = BTreeMap::new();
        for configured in &self.public_keys {
            let (name, key) = parse_public_key(configured).ok_or_else(unsupported_policy)?;
            // Nix inserts keys by name in configuration order and keeps the first.
            trusted_keys.entry(name).or_insert(key);
        }
        for key in keys.split_ascii_whitespace() {
            let (name, expected) = parse_public_key(key).ok_or(TransactionError::Io)?;
            match trusted_keys.get(name) {
                Some(actual) if actual == &expected => {}
                Some(_) => return Err(unsupported_policy()),
                None => complete = false,
            }
        }
        Ok(complete)
    }
}

struct Walk<'a, P, R> {
    protect: &'a P,
    read: &'a mut R,
    reserved: PathBuf,
    reserved_location: PathBuf,
    graph: Graph,
    active: Vec<PathBuf>,
    bytes: usize,
    references: usize,
    existing: bool,
    policy: EffectivePolicy,
}

impl<P, R> Walk<'_, P, R>
where
    P: Fn(&Path) -> io::Result<PathBuf>,
    R: FnMut(&mut File) -> Result<Vec<u8>, TransactionError>,
{
    fn visit_pending(
        &mut self,
        observed_source_bytes: usize,
        desired: &DesiredNixInclude,
    ) -> Result<(), TransactionError> {
        if self.graph.edges.len() >= MAX_EDGES {
            return Err(TransactionError::Io);
        }
        self.bytes += desired
            .source
            .len()
            .checked_sub(observed_source_bytes)
            .ok_or(TransactionError::Io)?;
        self.visit(&self.reserved.clone(), Some(&desired.leaf), 1)
    }

    fn visit(
        &mut self,
        path: &Path,
        bytes: Option<&[u8]>,
        depth: usize,
    ) -> Result<(), TransactionError> {
        if depth > MAX_DEPTH {
            return Err(TransactionError::Io);
        }
        let Some(bytes) = bytes else {
            return Ok(());
        };
        if self.bytes.saturating_add(bytes.len()) > MAX_BYTES {
            return Err(TransactionError::Io);
        }
        self.bytes += bytes.len();
        if self.active.iter().any(|active| active == path) {
            return Err(TransactionError::Io);
        }
        self.active.push(path.to_owned());
        let text = std::str::from_utf8(bytes).map_err(|_| TransactionError::Io)?;
        if text
            .bytes()
            .any(|byte| byte.is_ascii_control() && !b"\t\r\n".contains(&byte))
        {
            return Err(TransactionError::Io);
        }
        for line in text.lines() {
            let line = line
                .split('#')
                .next()
                .unwrap_or_default()
                .trim_matches([' ', '\t', '\r']);
            if line.is_empty() {
                continue;
            }
            let words: Vec<_> = line.split_ascii_whitespace().collect();
            if !matches!(words.first(), Some(&"include" | &"!include")) {
                if words.get(1) != Some(&"=")
                    || !words[0]
                        .bytes()
                        .all(|b| b.is_ascii_alphanumeric() || b == b'-')
                {
                    return Err(TransactionError::Io);
                }
                self.policy.assign(words[0], &words[2..])?;
                continue;
            }
            if words.len() != 2 || self.graph.edges.len() >= MAX_EDGES {
                return Err(TransactionError::Io);
            }
            let child = normalize(&path.parent().ok_or(TransactionError::Io)?.join(words[1]))
                .ok_or(TransactionError::Io)?;
            let optional = words[0] == "!include";
            let (child_input, child_bytes) =
                configuration_source_with(&child, self.protect, self.read)?;
            let reserved = child == self.reserved
                || location(&child, &child_input).as_ref() == Some(&self.reserved_location)
                || same_file(&child_input, &self.graph.companion);
            if reserved {
                self.references += 1;
                if !self.existing
                    || depth != 0
                    || line != POLICY_REFERENCE
                    || child != self.reserved
                    || self.references != 1
                {
                    return Err(TransactionError::NixBlocked(
                        NixBlock::ConfigurationOwnerUnknown,
                    ));
                }
                if child_input != self.graph.companion {
                    return Err(TransactionError::Io);
                }
            }
            let missing = matches!(&child_input, ConfigurationFile::Missing { .. });
            if missing && !optional {
                return Err(TransactionError::Io);
            }
            self.graph.edges.push(Edge {
                containing: path.to_owned(),
                path: child.clone(),
                optional,
                input: child_input,
            });
            self.visit(
                &child,
                (!missing).then_some(child_bytes.as_slice()),
                depth + 1,
            )?;
        }
        self.active.pop();
        Ok(())
    }
}

pub(super) fn observe(
    path: &Path,
    input: &ConfigurationFile,
    preparation: &NixIncludePreparation,
    bytes: &[u8],
    protect: &impl Fn(&Path) -> io::Result<PathBuf>,
    read: &mut impl FnMut(&mut File) -> Result<Vec<u8>, TransactionError>,
) -> Result<(AttachmentInputs, bool), TransactionError> {
    let ConfigurationFile::Present {
        entry: ConfigurationEntry::Direct,
        ..
    } = input
    else {
        return Ok((AttachmentInputs::Unresolved, false));
    };
    let (existing, desired) = match preparation {
        NixIncludePreparation::Append(desired) => (false, desired),
        NixIncludePreparation::ExistingReference(desired) => (true, desired),
        NixIncludePreparation::Conflict => return Ok((AttachmentInputs::Conflict, false)),
        _ => return Ok((AttachmentInputs::Unresolved, false)),
    };
    let mut collect = || -> Result<(AttachmentInputs, bool), TransactionError> {
        let reserved = normalize(&path.parent().ok_or(TransactionError::Io)?.join(POLICY_LEAF))
            .ok_or(TransactionError::Io)?;
        let (companion, companion_bytes) = configuration_source_with(&reserved, protect, read)?;
        let acceptable = if existing {
            matches!(&companion, ConfigurationFile::Present { entry: ConfigurationEntry::Direct, file, .. } if file.links == 1)
                && companion_bytes == desired.leaf
        } else {
            matches!(&companion, ConfigurationFile::Missing { .. })
        };
        if !acceptable {
            return Ok((AttachmentInputs::Conflict, false));
        }
        let reserved_location = location(&reserved, &companion).ok_or(TransactionError::Io)?;
        let mut walk = Walk {
            protect,
            read,
            reserved,
            reserved_location,
            graph: Graph {
                companion,
                edges: Vec::new(),
                #[cfg(target_os = "linux")]
                access: DaemonAccessPolicy::default(),
            },
            active: Vec::new(),
            bytes: 0,
            references: 0,
            existing,
            policy: EffectivePolicy::default(),
        };
        walk.visit(path, Some(bytes), 0)?;
        let complete = walk.policy.verify().is_ok();
        if !complete {
            if !existing {
                walk.visit_pending(bytes.len(), desired)?;
            }
            walk.policy.verify()?;
        }
        if existing && walk.references != 1 {
            return Ok((AttachmentInputs::Conflict, false));
        }
        if configuration_source_with(path, protect, walk.read)?.0 != *input
            || configuration_source_with(&walk.reserved, protect, walk.read)?.0
                != walk.graph.companion
        {
            return Err(TransactionError::Io);
        }
        for edge in &walk.graph.edges {
            if configuration_source_with(&edge.path, protect, walk.read)?.0 != edge.input {
                return Err(TransactionError::Io);
            }
        }
        #[cfg(target_os = "linux")]
        {
            walk.graph.access = walk.policy.access;
        }
        let inputs = if existing {
            AttachmentInputs::VerifiedExisting(walk.graph)
        } else {
            AttachmentInputs::VerifiedAppend(walk.graph)
        };
        Ok((inputs, complete))
    };
    match collect() {
        Err(TransactionError::StopRequested) => Err(TransactionError::StopRequested),
        Err(TransactionError::NixBlocked(_)) => Ok((AttachmentInputs::Conflict, false)),
        Err(_) => Ok((AttachmentInputs::Unresolved, false)),
        result => result,
    }
}

#[cfg(target_os = "linux")]
pub(super) fn published_baseline(
    path: &Path,
    attachment: AttachmentInputs,
) -> Option<AttachmentInputs> {
    let AttachmentInputs::VerifiedExisting(mut graph) = attachment else {
        return None;
    };
    let final_edge = graph.edges.pop()?;
    let reserved = path.with_file_name(POLICY_LEAF);
    if final_edge.containing != path
        || final_edge.path != reserved
        || final_edge.optional
        || final_edge.input != graph.companion
    {
        return None;
    }
    let ConfigurationFile::Present {
        entry: ConfigurationEntry::Direct,
        mut ancestors,
        resolved,
        mut resolved_ancestors,
        ..
    } = graph.companion
    else {
        return None;
    };
    if resolved != reserved || ancestors.len() < 2 || resolved_ancestors.len() < 2 {
        return None;
    }
    ancestors[0] = None;
    resolved_ancestors.remove(0);
    graph.companion = ConfigurationFile::Missing {
        ancestors,
        resolved_parent: resolved.parent()?.to_owned(),
        resolved_ancestors,
    };
    Some(AttachmentInputs::VerifiedAppend(graph))
}

#[cfg(any(test, target_os = "linux"))]
pub(in crate::installation::nix_owner) mod recovery {
    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    pub(in crate::installation::nix_owner) enum Stage {
        Baseline,
        CompanionOnly,
        SourcePublishedRetained,
        SourcePublishedRetired,
        Conflict,
        Unresolved,
    }

    pub(in crate::installation::nix_owner) struct Evidence<'a> {
        pub(in crate::installation::nix_owner) baseline: &'a ConfigurationObservation,
        pub(in crate::installation::nix_owner) baseline_bytes: &'a [u8],
        pub(in crate::installation::nix_owner) source_slot: &'a Path,
        pub(in crate::installation::nix_owner) source: &'a ConfigurationFile,
        pub(in crate::installation::nix_owner) leaf_slot: &'a Path,
        pub(in crate::installation::nix_owner) leaf: &'a ConfigurationFile,
        pub(in crate::installation::nix_owner) leaf_anchor: &'a File,
    }

    fn direct(input: &ConfigurationFile) -> Option<&InputIdentity> {
        match input {
            ConfigurationFile::Present {
                entry: ConfigurationEntry::Direct,
                file,
                ..
            } => Some(file),
            _ => None,
        }
    }

    fn moved(expected: &ConfigurationFile, actual: &ConfigurationFile, links: u64) -> bool {
        let (Some(a), Some(b)) = (direct(expected), direct(actual)) else {
            return false;
        };
        a.links == 1
            && b.links == links
            && a.device == b.device
            && a.inode == b.inode
            && a.mode == b.mode
            && a.uid == b.uid
            && a.gid == b.gid
            && a.size == b.size
            && a.modified == b.modified
            && b.changed >= a.changed
    }

    fn parents(input: &ConfigurationFile) -> (&[Option<InputIdentity>], &[Option<InputIdentity>]) {
        match input {
            ConfigurationFile::Present {
                ancestors,
                resolved_ancestors,
                ..
            }
            | ConfigurationFile::Missing {
                ancestors,
                resolved_ancestors,
                ..
            } => (&ancestors[1..], &resolved_ancestors[1..]),
        }
    }

    fn same_parent(expected: &ConfigurationFile, actual: &ConfigurationFile) -> bool {
        match (expected, actual) {
            (
                ConfigurationFile::Present { resolved: a, .. },
                ConfigurationFile::Present { resolved: b, .. },
            ) => a.parent() == b.parent() && parents(expected) == parents(actual),
            _ => false,
        }
    }

    pub(in crate::installation::nix_owner) fn observe(
        path: &Path,
        evidence: &Evidence<'_>,
        protect: &impl Fn(&Path) -> io::Result<PathBuf>,
        read: &mut impl FnMut(&mut File) -> Result<Vec<u8>, TransactionError>,
    ) -> Result<Stage, TransactionError> {
        let collect = |read: &mut _| -> Result<Stage, TransactionError> {
            let AttachmentInputs::VerifiedAppend(baseline_graph) = &evidence.baseline.attachment
            else {
                return Ok(Stage::Unresolved);
            };
            let NixIncludePreparation::Append(desired) = &evidence.baseline.preparation else {
                return Ok(Stage::Unresolved);
            };
            if NixIncludePreparation::from_input(
                &ConfigurationEntry::Direct,
                &desired.observed_digest,
                evidence.baseline_bytes,
            ) != evidence.baseline.preparation
            {
                return Ok(Stage::Unresolved);
            }
            for (input, bytes) in [
                (&evidence.baseline.input, evidence.baseline_bytes),
                (evidence.source, desired.source.as_slice()),
                (evidence.leaf, desired.leaf.as_slice()),
            ] {
                if !matches!(input, ConfigurationFile::Present {
                    entry: ConfigurationEntry::Direct, file, digest, ..
                } if file.links == 1 && *digest == format!("{:x}", Sha256::digest(bytes)))
                {
                    return Ok(Stage::Unresolved);
                }
            }
            let reserved = path.with_file_name(POLICY_LEAF);
            if [
                path,
                reserved.as_path(),
                evidence.source_slot,
                evidence.leaf_slot,
            ]
            .iter()
            .any(|p| normalize(p).as_deref() != Some(*p))
                || evidence.source_slot == evidence.leaf_slot
                || [path, reserved.as_path()].contains(&evidence.source_slot)
                || [path, reserved.as_path()].contains(&evidence.leaf_slot)
                || same_file(evidence.source, evidence.leaf)
                || same_file(evidence.source, &evidence.baseline.input)
                || same_file(evidence.leaf, &evidence.baseline.input)
            {
                return Ok(Stage::Unresolved);
            }
            let live = configuration_source_with(path, protect, read)?;
            let companion = configuration_source_with(&reserved, protect, read)?;
            let source_slot = configuration_source_with(evidence.source_slot, protect, read)?;
            let leaf_slot = configuration_source_with(evidence.leaf_slot, protect, read)?;
            if !same_parent(&evidence.baseline.input, &live.0)
                || !same_parent(evidence.source, &source_slot.0)
            {
                return Ok(Stage::Conflict);
            }
            let baseline_source =
                live.0 == evidence.baseline.input && live.1 == evidence.baseline_bytes;
            let prepared_source =
                source_slot.0 == *evidence.source && source_slot.1 == desired.source;
            let published_source = moved(evidence.source, &live.0, 1)
                && live.1 == desired.source
                && moved(&evidence.baseline.input, &source_slot.0, 1)
                && source_slot.1 == evidence.baseline_bytes;
            let missing_companion = companion.0 == baseline_graph.companion;
            let retained_leaf = same_parent(evidence.leaf, &leaf_slot.0)
                && moved(evidence.leaf, &leaf_slot.0, 2)
                && leaf_slot.1 == desired.leaf;
            let published_leaf = moved(evidence.leaf, &companion.0, 2)
                && companion.1 == desired.leaf
                && same_file(&companion.0, &leaf_slot.0);
            let anchor = InputIdentity::of(
                &evidence
                    .leaf_anchor
                    .metadata()
                    .map_err(|_| TransactionError::Io)?,
            );
            let anchored = if missing_companion {
                direct(&leaf_slot.0)
            } else {
                direct(&companion.0)
            };
            if anchored != Some(&anchor) {
                return Ok(Stage::Conflict);
            }
            let retired_leaf = match (&leaf_slot.0, evidence.leaf) {
                (
                    ConfigurationFile::Missing {
                        ancestors,
                        resolved_parent,
                        resolved_ancestors,
                    },
                    ConfigurationFile::Present {
                        ancestors: old,
                        resolved,
                        resolved_ancestors: old_resolved,
                        ..
                    },
                ) => {
                    ancestors.first() == Some(&None)
                        && ancestors[1..] == old[1..]
                        && Some(resolved_parent.as_path()) == resolved.parent()
                        && resolved_ancestors.as_slice() == &old_resolved[1..]
                }
                _ => false,
            };
            let stage = if baseline_source
                && prepared_source
                && missing_companion
                && leaf_slot.0 == *evidence.leaf
                && leaf_slot.1 == desired.leaf
            {
                Stage::Baseline
            } else if baseline_source && prepared_source && retained_leaf && published_leaf {
                Stage::CompanionOnly
            } else if published_source && retained_leaf && published_leaf {
                Stage::SourcePublishedRetained
            } else if published_source
                && retired_leaf
                && moved(evidence.leaf, &companion.0, 1)
                && companion.1 == desired.leaf
            {
                Stage::SourcePublishedRetired
            } else {
                return Ok(Stage::Conflict);
            };
            let published = matches!(
                stage,
                Stage::SourcePublishedRetained | Stage::SourcePublishedRetired
            );
            let mut walk = Walk {
                protect,
                read,
                reserved: reserved.clone(),
                reserved_location: location(&reserved, &companion.0).ok_or(TransactionError::Io)?,
                graph: Graph {
                    companion: companion.0,
                    edges: Vec::new(),
                    #[cfg(target_os = "linux")]
                    access: DaemonAccessPolicy::default(),
                },
                active: Vec::new(),
                bytes: 0,
                references: 0,
                existing: published,
                policy: EffectivePolicy::default(),
            };
            walk.visit(path, Some(&live.1), 0)?;
            if !published {
                walk.visit_pending(live.1.len(), desired)?;
            }
            walk.policy.verify()?;
            if published {
                let Some(final_edge) = walk.graph.edges.pop() else {
                    return Ok(Stage::Conflict);
                };
                if walk.references != 1
                    || final_edge.containing != path
                    || final_edge.path != reserved
                    || final_edge.optional
                    || final_edge.input != walk.graph.companion
                {
                    return Ok(Stage::Conflict);
                }
            }
            if walk.graph.edges != baseline_graph.edges {
                return Ok(Stage::Conflict);
            }
            for (p, input, bytes) in [
                (path, &live.0, live.1.as_slice()),
                (
                    reserved.as_path(),
                    &walk.graph.companion,
                    companion.1.as_slice(),
                ),
                (
                    evidence.source_slot,
                    &source_slot.0,
                    source_slot.1.as_slice(),
                ),
                (evidence.leaf_slot, &leaf_slot.0, leaf_slot.1.as_slice()),
            ] {
                let checked = configuration_source_with(p, protect, walk.read)?;
                if checked.0 != *input || checked.1 != bytes {
                    return Ok(Stage::Conflict);
                }
            }
            if InputIdentity::of(
                &evidence
                    .leaf_anchor
                    .metadata()
                    .map_err(|_| TransactionError::Io)?,
            ) != anchor
            {
                return Ok(Stage::Conflict);
            }
            for edge in &walk.graph.edges {
                if configuration_source_with(&edge.path, protect, walk.read)?.0 != edge.input {
                    return Ok(Stage::Conflict);
                }
            }
            Ok(stage)
        };
        match collect(read) {
            Err(TransactionError::StopRequested) => Err(TransactionError::StopRequested),
            Err(TransactionError::NixBlocked(_)) => Ok(Stage::Conflict),
            Err(_) => Ok(Stage::Unresolved),
            result => result,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::fixture_input;
    use super::*;
    use std::os::unix::fs::symlink;

    fn attachment(input: &ConfigurationObservation) -> &AttachmentInputs {
        &input.attachment
    }

    fn fixture() -> (tempfile::TempDir, PathBuf) {
        let temporary = tempfile::tempdir().unwrap();
        let main = temporary.path().canonicalize().unwrap().join("nix.conf");
        fs::write(&main, b"keep-outputs = true\n").unwrap();
        (temporary, main)
    }

    #[test]
    fn conventional_policy_requires_effective_signatures_and_stock_store() {
        let (_temporary, main) = fixture();
        let nested = main.with_file_name("settings.conf");
        for (source, included, eligible) in [
            ("keep-outputs = true\n!include absent.conf\n", "", true),
            ("require-sigs = 1\nstore = auto\n", "", true),
            ("store =\n", "", true),
            (
                "require-sigs = no\n!include settings.conf\n",
                "require-sigs = yes # operator override\nstore = local\n",
                true,
            ),
            ("require-sigs = false\n", "", false),
            (
                "require-sigs = true\ninclude settings.conf\n",
                "require-sigs = 0\n",
                false,
            ),
            (
                "include settings.conf\nrequire-sigs = false\n",
                "require-sigs = true\n",
                false,
            ),
            ("require-sigs = enabled\nrequire-sigs = true\n", "", false),
            ("require-sigs = true false\n", "", false),
            ("require-sigs =\n", "", false),
            (
                "include settings.conf\n",
                "store = local?require-sigs=false\n",
                false,
            ),
            ("store = daemon\n", "", false),
            ("plugin-files = /root/extension.so\n", "", false),
            (
                "plugin-files = /root/extension.so\ninclude settings.conf\n",
                "plugin-files =\n",
                true,
            ),
        ] {
            fs::write(&nested, included).unwrap();
            fs::write(&main, source).unwrap();
            let input = fixture_input(&main).unwrap();
            assert_eq!(
                matches!(attachment(&input), AttachmentInputs::VerifiedAppend(_)),
                eligible,
                "main {source:?}, included {included:?}"
            );
        }
    }

    #[test]
    fn conventional_policy_rechecks_signatures_with_the_exact_existing_leaf() {
        let (_temporary, main) = fixture();
        let nested = main.with_file_name("settings.conf");
        fs::write(&nested, b"require-sigs = true\n").unwrap();
        fs::write(&main, b"include settings.conf\n").unwrap();
        let baseline = fixture_input(&main).unwrap();
        let NixIncludePreparation::Append(desired) = &baseline.preparation else {
            panic!("append");
        };
        let leaf = main.with_file_name(POLICY_LEAF);
        fs::write(&leaf, &desired.leaf).unwrap();
        fs::write(&main, &desired.source).unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::VerifiedExisting(_)
        ));
        fs::write(&nested, b"require-sigs = false\n").unwrap();
        assert!(!matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::VerifiedExisting(_)
        ));
        assert_eq!(fs::read(&leaf).unwrap(), desired.leaf);
    }

    #[test]
    fn conventional_policy_rejects_a_shadowed_release_key() {
        let (_temporary, main) = fixture();
        let nested = main.with_file_name("settings.conf");
        let (_, required_keys) = crate::candidate::nix_cache_settings().unwrap();
        let expected = required_keys.split_ascii_whitespace().next().unwrap();
        let (name, _) = expected.split_once(':').unwrap();
        fs::write(
            &nested,
            format!("trusted-public-keys = {name}:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\n"),
        )
        .unwrap();
        fs::write(&main, b"include settings.conf\n").unwrap();
        let observation = fixture_input(&main).unwrap();
        assert!(matches!(
            attachment(&observation),
            AttachmentInputs::Conflict
        ));
        let NixIncludePreparation::Append(desired) = observation.preparation else {
            panic!("append");
        };
        fs::write(main.with_file_name(POLICY_LEAF), &desired.leaf).unwrap();
        fs::write(&main, &desired.source).unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::Conflict
        ));

        fs::write(
            &main,
            format!(
                "include settings.conf\nbinary-cache-public-keys = {required_keys}\n{POLICY_REFERENCE}\n"
            ),
        )
        .unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::VerifiedExisting(_)
        ));
        fs::write(
            &main,
            format!("extra-trusted-public-keys = unrelated:bad-key\n{POLICY_REFERENCE}\n"),
        )
        .unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::Conflict
        ));
    }

    #[test]
    fn append_counts_the_pending_include_in_the_edge_and_byte_limits() {
        let (_temporary, main) = fixture();
        let nested = main.with_file_name("settings.conf");
        let extra_edge = "include settings.conf\n";
        fs::write(&nested, b"keep-outputs = true\n").unwrap();
        for (count, allowed) in [(MAX_EDGES, false), (MAX_EDGES - 1, true)] {
            fs::write(&main, extra_edge.repeat(count)).unwrap();
            assert_eq!(
                matches!(
                    attachment(&fixture_input(&main).unwrap()),
                    AttachmentInputs::VerifiedAppend(_)
                ),
                allowed,
                "edge count {count}"
            );
        }

        let visits = 8;
        let base = extra_edge.repeat(visits);
        fs::write(&main, &base).unwrap();
        let NixIncludePreparation::Append(desired) = fixture_input(&main).unwrap().preparation
        else {
            panic!("append");
        };
        let space = MAX_BYTES - desired.source.len() - desired.leaf.len();
        let fit = space / visits;
        for (length, allowed) in [(fit + 1, false), (fit, true)] {
            fs::write(&nested, "#".repeat(length)).unwrap();
            fs::write(&main, &base).unwrap();
            let baseline = fixture_input(&main).unwrap();
            assert_eq!(
                matches!(attachment(&baseline), AttachmentInputs::VerifiedAppend(_)),
                allowed,
                "nested length {length}"
            );
        }
        fs::write(&main, &desired.source).unwrap();
        fs::write(main.with_file_name(POLICY_LEAF), &desired.leaf).unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::VerifiedExisting(_)
        ));
    }

    fn recovery_read(file: &mut File) -> Result<Vec<u8>, TransactionError> {
        use std::io::Read;
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes)
            .map_err(|_| TransactionError::Io)?;
        Ok(bytes)
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn published_graph_recovers_the_original_logical_include_closure() {
        let (_temporary, main) = fixture();
        let base = main.parent().unwrap();
        let nested = base.join("nested.conf");
        fs::write(&nested, b"keep-outputs = true\n").unwrap();
        fs::write(&main, b"include nested.conf\n!include missing.conf\n").unwrap();
        let baseline = fixture_input(&main).unwrap();
        let NixIncludePreparation::Append(desired) = &baseline.preparation else {
            panic!("append");
        };
        fs::write(base.join(POLICY_LEAF), &desired.leaf).unwrap();
        fs::write(&main, &desired.source).unwrap();
        let restored = |path: &Path| {
            published_baseline(path, fixture_input(path).unwrap().attachment).unwrap()
        };
        assert_eq!(restored(&main), baseline.attachment);
        fs::write(&nested, b"keep-outputs = false\n").unwrap();
        assert_ne!(restored(&main), baseline.attachment);
    }

    #[test]
    fn exact_recovery_stages_and_foreign_inputs() {
        use recovery::{Evidence, Stage};
        for foreign in ["none", "leaf", "main", "displaced", "optional", "alias"] {
            let (_temporary, main) = fixture();
            let base = main.parent().unwrap();
            fs::write(base.join("nested"), b"keep-outputs = true\n").unwrap();
            fs::write(base.join("other"), b"keep-outputs = true\n").unwrap();
            symlink(base.join("nested"), base.join("alias")).unwrap();
            fs::write(&main, b"include alias\n!include optional\n").unwrap();
            let baseline_bytes = fs::read(&main).unwrap();
            let baseline = fixture_input(&main).unwrap();
            let NixIncludePreparation::Append(desired) = &baseline.preparation else {
                panic!("append");
            };
            let source_slot = base.join("source-stage");
            let leaf_slot = base.join("leaf-stage");
            fs::write(&source_slot, &desired.source).unwrap();
            fs::write(&leaf_slot, &desired.leaf).unwrap();
            let protect = |p: &Path| p.canonicalize();
            let source = configuration_source_with(&source_slot, &protect, &mut recovery_read)
                .unwrap()
                .0;
            let leaf = configuration_source_with(&leaf_slot, &protect, &mut recovery_read)
                .unwrap()
                .0;
            let leaf_anchor = File::open(&leaf_slot).unwrap();
            let evidence = Evidence {
                baseline: &baseline,
                baseline_bytes: &baseline_bytes,
                source_slot: &source_slot,
                source: &source,
                leaf_slot: &leaf_slot,
                leaf: &leaf,
                leaf_anchor: &leaf_anchor,
            };
            let observe =
                || recovery::observe(&main, &evidence, &protect, &mut recovery_read).unwrap();
            assert_eq!(observe(), Stage::Baseline);
            let companion = base.join(POLICY_LEAF);
            fs::hard_link(&leaf_slot, &companion).unwrap();
            assert_eq!(observe(), Stage::CompanionOnly);
            assert!(matches!(
                fixture_input(&main).unwrap().attachment,
                AttachmentInputs::Conflict
            ));
            let displaced = base.join("exchange-temp");
            fs::rename(&main, &displaced).unwrap();
            fs::rename(&source_slot, &main).unwrap();
            fs::rename(&displaced, &source_slot).unwrap();
            assert_eq!(observe(), Stage::SourcePublishedRetained);
            assert!(matches!(
                fixture_input(&main).unwrap().attachment,
                AttachmentInputs::Conflict
            ));
            fs::remove_file(&leaf_slot).unwrap();
            assert_eq!(observe(), Stage::SourcePublishedRetired);
            assert!(matches!(
                fixture_input(&main).unwrap().attachment,
                AttachmentInputs::VerifiedExisting(_)
            ));
            let wrong_anchor = File::open(&source_slot).unwrap();
            let mismatched = Evidence {
                leaf_anchor: &wrong_anchor,
                ..evidence
            };
            assert_eq!(
                recovery::observe(&main, &mismatched, &protect, &mut recovery_read).unwrap(),
                Stage::Conflict
            );
            match foreign {
                "leaf" | "main" | "displaced" => {
                    let target = match foreign {
                        "leaf" => &companion,
                        "main" => &main,
                        _ => &source_slot,
                    };
                    let replacement = base.join("replacement");
                    fs::write(&replacement, fs::read(target).unwrap()).unwrap();
                    fs::rename(&replacement, target).unwrap();
                }
                "optional" => fs::write(base.join("optional"), b"keep-outputs = true\n").unwrap(),
                "alias" => {
                    fs::remove_file(base.join("alias")).unwrap();
                    symlink(base.join("other"), base.join("alias")).unwrap();
                }
                _ => continue,
            }
            assert_eq!(observe(), Stage::Conflict, "{foreign}");
        }
    }

    #[test]
    fn recovery_rechecks_edges_after_the_walk() {
        use recovery::{Evidence, Stage};
        let (_temporary, main) = fixture();
        let base = main.parent().unwrap();
        fs::write(&main, b"!include optional\n").unwrap();
        let baseline_bytes = fs::read(&main).unwrap();
        let baseline = fixture_input(&main).unwrap();
        let NixIncludePreparation::Append(desired) = &baseline.preparation else {
            panic!("append");
        };
        let source_slot = base.join("source-stage");
        let leaf_slot = base.join("leaf-stage");
        fs::write(&source_slot, &desired.source).unwrap();
        fs::write(&leaf_slot, &desired.leaf).unwrap();
        let protect = |p: &Path| p.canonicalize();
        let source = configuration_source_with(&source_slot, &protect, &mut recovery_read)
            .unwrap()
            .0;
        let leaf = configuration_source_with(&leaf_slot, &protect, &mut recovery_read)
            .unwrap()
            .0;
        let leaf_anchor = File::open(&leaf_slot).unwrap();
        let evidence = Evidence {
            baseline: &baseline,
            baseline_bytes: &baseline_bytes,
            source_slot: &source_slot,
            source: &source,
            leaf_slot: &leaf_slot,
            leaf: &leaf,
            leaf_anchor: &leaf_anchor,
        };
        let mut reads = 0;
        let result = recovery::observe(&main, &evidence, &protect, &mut |file| {
            reads += 1;
            if reads == 4 {
                fs::write(base.join("optional"), b"keep-outputs = true\n").unwrap();
            }
            recovery_read(file)
        })
        .unwrap();
        assert_eq!(result, Stage::Conflict);
    }

    #[test]
    fn nested_paths_are_relative_to_syntactic_parent_and_normalized_before_symlinks() {
        let (_temporary, main) = fixture();
        let base = main.parent().unwrap();
        fs::create_dir(base.join("real")).unwrap();
        fs::create_dir(base.join("prefix")).unwrap();
        fs::write(base.join("real/source"), b"include child\n").unwrap();
        fs::write(base.join("prefix/child"), b"keep-outputs = true\n").unwrap();
        symlink(base.join("real/source"), base.join("prefix/source")).unwrap();
        symlink(base.join("real"), base.join("shortcut")).unwrap();
        fs::write(&main, b"include shortcut/../prefix/source\n").unwrap();
        let input = fixture_input(&main).unwrap();
        let AttachmentInputs::VerifiedAppend(graph) = attachment(&input) else {
            panic!("syntactic containing directory expected")
        };
        assert_eq!(graph.edges[0].path, base.join("prefix/source"));
        assert_eq!(graph.edges[1].path, base.join("prefix/child"));
        assert_eq!(
            normalize(Path::new("/../../one/./two/../three")),
            Some(PathBuf::from("/one/three"))
        );
    }

    #[test]
    fn absent_optional_inputs_are_retained_and_materialization_changes_evidence() {
        let (_temporary, main) = fixture();
        fs::write(&main, b"!include optional\n").unwrap();
        let before = fixture_input(&main).unwrap();
        let AttachmentInputs::VerifiedAppend(graph) = attachment(&before) else {
            panic!("optional absence allowed")
        };
        assert!(matches!(
            graph.edges[0].input,
            ConfigurationFile::Missing { .. }
        ));
        fs::write(main.with_file_name("optional"), b"keep-outputs = true\n").unwrap();
        let after = fixture_input(&main).unwrap();
        assert_ne!(before, after);
        assert_eq!(
            serde_json::to_value(before).unwrap(),
            serde_json::to_value(after).unwrap()
        );
    }

    #[test]
    fn required_absence_and_cycles_are_unresolved_but_shared_dags_are_allowed() {
        let (_temporary, main) = fixture();
        for source in ["include absent\n", "include nix.conf\n"] {
            fs::write(&main, source).unwrap();
            assert!(matches!(
                attachment(&fixture_input(&main).unwrap()),
                AttachmentInputs::Unresolved
            ));
        }
        fs::write(main.with_file_name("shared"), b"keep-outputs = true\n").unwrap();
        fs::write(main.with_file_name("branch"), b"include shared\n").unwrap();
        fs::write(&main, b"include branch\ninclude shared\n").unwrap();
        let input = fixture_input(&main).unwrap();
        let AttachmentInputs::VerifiedAppend(graph) = attachment(&input) else {
            panic!("shared DAG allowed")
        };
        assert_eq!(graph.edges.len(), 3);
    }

    #[test]
    fn reserved_optional_reference_in_a_nested_file_conflicts_even_when_missing() {
        let (_temporary, main) = fixture();
        let base = main.parent().unwrap();
        fs::create_dir(base.join("nested")).unwrap();
        symlink(base, base.join("alias")).unwrap();
        for source in [
            "!include ../neohaskell-cache-policy.conf\n",
            "!include ../alias/neohaskell-cache-policy.conf\n",
        ] {
            fs::write(base.join("nested/source"), source).unwrap();
            fs::write(&main, b"include nested/source\n").unwrap();
            assert!(matches!(
                attachment(&fixture_input(&main).unwrap()),
                AttachmentInputs::Conflict
            ));
        }
    }

    #[test]
    fn only_the_exact_referenced_direct_companion_is_verified() {
        let (_temporary, main) = fixture();
        let leaf = main.with_file_name(POLICY_LEAF);
        let policy = crate::candidate::nix_cache_policy().unwrap();
        fs::write(&main, POLICY_REFERENCE).unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::Conflict
        ));
        fs::write(&leaf, b"keep-outputs = true\n").unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::Conflict
        ));
        fs::write(&leaf, &policy).unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::VerifiedExisting(_)
        ));
        fs::write(&main, b"keep-outputs = true\n").unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::Conflict
        ));
        assert_eq!(fs::read(&leaf).unwrap(), policy.as_bytes());
        fs::write(&main, POLICY_REFERENCE).unwrap();
        let other = main.with_file_name("other");
        fs::rename(&leaf, &other).unwrap();
        symlink(&other, &leaf).unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::Conflict
        ));
        fs::remove_file(&leaf).unwrap();
        fs::hard_link(&other, &leaf).unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::Conflict
        ));
    }

    #[test]
    fn extra_nested_and_aliased_references_conflict() {
        let (_temporary, main) = fixture();
        let leaf = main.with_file_name(POLICY_LEAF);
        fs::write(&leaf, crate::candidate::nix_cache_policy().unwrap()).unwrap();
        let alias = main.with_file_name("alias");
        symlink(&leaf, &alias).unwrap();
        for source in ["include neohaskell-cache-policy.conf\n", "include alias\n"] {
            fs::write(main.with_file_name("nested"), source).unwrap();
            fs::write(&main, format!("include nested\n{POLICY_REFERENCE}\n")).unwrap();
            assert!(matches!(
                attachment(&fixture_input(&main).unwrap()),
                AttachmentInputs::Conflict
            ));
        }
    }

    #[test]
    fn byte_identical_nested_and_companion_replacement_changes_evidence() {
        let (_temporary, main) = fixture();
        let nested = main.with_file_name("nested");
        let leaf = main.with_file_name(POLICY_LEAF);
        fs::write(&nested, b"keep-outputs = true\n").unwrap();
        fs::write(&leaf, crate::candidate::nix_cache_policy().unwrap()).unwrap();
        fs::write(&main, format!("include nested\n{POLICY_REFERENCE}\n")).unwrap();
        for path in [&nested, &leaf] {
            let before = fixture_input(&main).unwrap();
            let replacement = main.with_file_name("replacement");
            fs::write(&replacement, fs::read(path).unwrap()).unwrap();
            fs::rename(replacement, path).unwrap();
            let after = fixture_input(&main).unwrap();
            assert_ne!(before, after);
            assert_eq!(
                serde_json::to_value(before).unwrap(),
                serde_json::to_value(after).unwrap()
            );
        }
    }

    #[test]
    fn supplementary_read_failure_is_unresolved_and_cancellation_propagates() {
        let (_temporary, main) = fixture();
        fs::write(&main, b"include nested\n").unwrap();
        fs::write(main.with_file_name("nested"), b"keep-outputs = true\n").unwrap();
        for stop in [false, true] {
            let mut reads = 0;
            let result = configuration_input_with(
                &main,
                |path| path.canonicalize(),
                |file| {
                    use std::io::Read;
                    reads += 1;
                    if reads > 1 {
                        return Err(if stop {
                            TransactionError::StopRequested
                        } else {
                            TransactionError::Io
                        });
                    }
                    let mut bytes = Vec::new();
                    file.read_to_end(&mut bytes).unwrap();
                    Ok(bytes)
                },
            );
            if stop {
                assert!(matches!(result, Err(TransactionError::StopRequested)));
            } else {
                assert!(matches!(
                    attachment(&result.unwrap()),
                    AttachmentInputs::Unresolved
                ));
            }
        }
    }

    #[test]
    fn graph_size_depth_and_aggregate_bytes_are_bounded() {
        let (_temporary, main) = fixture();
        let base = main.parent().unwrap();
        fs::write(base.join("shared"), b"keep-outputs = true\n").unwrap();
        fs::write(&main, "include shared\n".repeat(MAX_EDGES + 1)).unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::Unresolved
        ));
        fs::write(
            base.join("shared"),
            format!("#{}\n", "x".repeat(MAX_CONFIGURATION_BYTES - 2)),
        )
        .unwrap();
        fs::write(
            &main,
            "include shared\n".repeat(MAX_BYTES / MAX_CONFIGURATION_BYTES + 1),
        )
        .unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::Unresolved
        ));
        for index in 0..=MAX_DEPTH {
            fs::write(
                base.join(format!("depth{index}")),
                format!("include depth{}\n", index + 1),
            )
            .unwrap();
        }
        fs::write(&main, b"include depth0\n").unwrap();
        assert!(matches!(
            attachment(&fixture_input(&main).unwrap()),
            AttachmentInputs::Unresolved
        ));
    }
}
