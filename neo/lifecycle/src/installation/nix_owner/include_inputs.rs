use super::*;
use std::path::Component;

const MAX_EDGES: usize = 128;
const MAX_DEPTH: usize = 32;
const MAX_BYTES: usize = 1024 * 1024;

#[derive(PartialEq, Eq)]
pub(super) enum AttachmentInputs {
    Unresolved,
    Conflict,
    VerifiedAppend(Graph),
    VerifiedExisting(Graph),
}

#[derive(PartialEq, Eq)]
pub(super) struct Graph {
    companion: ConfigurationFile,
    edges: Vec<Edge>,
}

#[derive(PartialEq, Eq)]
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
}

impl<P, R> Walk<'_, P, R>
where
    P: Fn(&Path) -> io::Result<PathBuf>,
    R: FnMut(&mut File) -> Result<Vec<u8>, TransactionError>,
{
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
) -> Result<AttachmentInputs, TransactionError> {
    let ConfigurationFile::Present {
        entry: ConfigurationEntry::Direct,
        ..
    } = input
    else {
        return Ok(AttachmentInputs::Unresolved);
    };
    let (existing, desired) = match preparation {
        NixIncludePreparation::Append(desired) => (false, desired),
        NixIncludePreparation::ExistingReference(desired) => (true, desired),
        NixIncludePreparation::Conflict => return Ok(AttachmentInputs::Conflict),
        _ => return Ok(AttachmentInputs::Unresolved),
    };
    let mut collect = || -> Result<AttachmentInputs, TransactionError> {
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
            return Ok(AttachmentInputs::Conflict);
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
            },
            active: Vec::new(),
            bytes: companion_bytes.len(),
            references: 0,
            existing,
        };
        walk.visit(path, Some(bytes), 0)?;
        if existing && walk.references != 1 {
            return Ok(AttachmentInputs::Conflict);
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
        Ok(if existing {
            AttachmentInputs::VerifiedExisting(walk.graph)
        } else {
            AttachmentInputs::VerifiedAppend(walk.graph)
        })
    };
    match collect() {
        Err(TransactionError::StopRequested) => Err(TransactionError::StopRequested),
        Err(TransactionError::NixBlocked(_)) => Ok(AttachmentInputs::Conflict),
        Err(_) => Ok(AttachmentInputs::Unresolved),
        result => result,
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
